-module(af_hos_runtime).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0, spawn_system/1, send_event/3, send_event_call/4,
         stop_system/1, get_log/1, introspect_child/2]).
-export([a4_exec_stateless_body/2]).

%% HOS runtime, a4-native.
%%
%% Previously this module contained the full gen_server-style loop
%% plus event dispatch, trans-index lookup, and effect dispatch in
%% Erlang. That code now lives in src/bootstrap/hos/runtime.a4 — the
%% a4 compiler builds it into the same BEAM process model, and the
%% receive/dispatch hot path runs through the a4 interpreter.
%%
%% This file now only:
%%   1. Loads runtime.a4 at init time.
%%   2. Provides the Erlang-side public API (spawn_system/send_event/
%%      stop_system/get_log/introspect_child/send_event_call) as thin
%%      wrappers that build the a4 wire-format messages.
%%   3. Provides a4_exec_stateless_body/2 as an FFI helper for the
%%      one awkward iteration path the a4 runtime delegates back to
%%      Erlang (see dispatch-stateless in runtime.a4).

%% ---------------------------------------------------------------
%% Init: load the a4 runtime so its words are in the global dict
%% ---------------------------------------------------------------

%% init/0 is a no-op now — the HOS runtime's a4 source loads lazily
%% on the first spawn_system/1 call.  Loading eagerly during
%% af_repl:init_types/0 would pollute the global word registry for
%% tests that assert a clean "no user-defined words" state.
init() ->
    ok.

load_a4_runtime() ->
    %% Idempotent: skip if the `spawn-system` word is still in the
    %% Tuple dict.  Each call to af_repl:init_types/0 re-registers
    %% the stock types (clearing their dicts), so check for the
    %% runtime word directly rather than the HosSelf type which
    %% lives under its own (unaffected) ETS row.
    case spawn_system_word_loaded() of
        true  -> ok;
        false ->
            case locate_runtime_a4() of
                {ok, Path} ->
                    {ok, Content} = file:read_file(Path),
                    Tokens = af_parser:parse(binary_to_list(Content), Path),
                    af_interpreter:interpret_tokens(Tokens,
                        af_interpreter:new_continuation()),
                    af_type_compiler:finalize_pending_checks(),
                    compile_hot_loop_words(),
                    ok;
                not_found ->
                    CWD = case file:get_cwd() of
                        {ok, Dir} -> Dir;
                        _ -> unknown
                    end,
                    error({hos_runtime_a4_source_missing,
                           "Expected src/bootstrap/hos/runtime.a4 relative "
                           "to cwd (" ++ CWD ++ ") or priv/hos/runtime.a4"})
            end
    end.

spawn_system_word_loaded() ->
    %% Check the Actor dict because Actor gets cleared on every
    %% af_repl:init_types/0 call (stock type), whereas HosBlueprint —
    %% declared in runtime.a4 — survives re-init and would give us a
    %% stale "already loaded" reading after a test-fixture reset.
    %% spawn-system-with-parent is the canonical HOS word registered
    %% on Actor (TOS-first sig_in [Actor, HosBlueprint]); if it's
    %% absent, the runtime's hot-loop words are gone and we must
    %% reload.
    case af_type:find_op_by_name("spawn-system-with-parent", 'Actor') of
        {ok, _} -> true;
        _       -> false
    end.

%% Native-compile the runtime's hot-loop words. Each word becomes a
%% BEAM function that runs without interpreter dispatch. Cross-word
%% calls resolve to same-module (when batched together) or native
%% cross-module calls — no apply_impl fallback.
%%
%% Words listed here must compile cleanly (no reduce/each/map list
%% combinators in the body). Init-path words (spawn-children-list
%% etc.) stay interpreted — they run once per actor spawn, perf
%% irrelevant.
compile_hot_loop_words() ->
    Prev = persistent_term:get(af_auto_compile, false),
    persistent_term:put(af_auto_compile, true),
    try
        %% Compile all hot words into ONE module so inter-word calls
        %% become same-module BEAM calls instead of ETS lookups via
        %% apply_impl. Without batching, a caller's compiled body
        %% calls into apply_impl for each callee — dominating cost
        %% and making native compile slower than the interpreter.
        Hot = [
            %% Leaves.
            "empty-target?", "after-target?",
            "trans-key-pair",
            "make-hoscast",
            "log-event",
            %% Map helpers.
            "map-lookup-flag-step", "map-lookup-flag",
            "map-get-or-not-found-step", "map-get-or-not-found",
            %% Dispatch chain (stateful path still blocked by #161).
            "dispatch-cast-step", "dispatch-cast",
            "handle-message",
            "hos-loop-step", "hos-loop"
        ],
        compile_batch_safe(af_native_hos_runtime, Hot)
    after
        persistent_term:put(af_auto_compile, Prev)
    end,
    ok.

%% Compile a list of words into one module and register wrappers for
%% each. On compile failure, each word falls back to its interpreted
%% form individually. Inter-word calls inside the module resolve as
%% same-module BEAM calls (fast); unknown ops fall through to
%% apply_impl (slow) — see the Hot list for what's safe to include.
compile_batch_safe(ModAtom, Names) ->
    try
        AllDefs = lists:flatmap(
                    fun(N) -> af_word_compiler:find_compiled_word_defs(N) end,
                    Names),
        case AllDefs of
            [] -> ok;
            _ ->
                case af_word_compiler:compile_words_to_module(ModAtom, AllDefs) of
                    {ok, ModAtom} ->
                        [register_wrapper(ModAtom, N) || N <- Names],
                        ok;
                    {error, _} ->
                        ok
                end
        end
    catch _:_ -> ok
    end.

register_wrapper(ModAtom, Name) ->
    try
        Defs = af_word_compiler:find_compiled_word_defs(Name),
        FunAtom = list_to_atom(Name),
        ByType = group_by_target(Defs),
        lists:foreach(fun({TargetType, TypeDefs}) ->
            {BroadSigIn, BroadSigOut, BroadGuard} = broadest_def(TypeDefs),
            Wrapper0 = af_word_compiler:make_wrapper(
                         ModAtom, FunAtom, BroadSigIn, BroadSigOut),
            Wrapper = Wrapper0#operation{guard = BroadGuard},
            af_type:replace_ops(TargetType, Name, [Wrapper])
        end, ByType)
    catch _:_ -> ok
    end.

group_by_target(Defs) ->
    Groups = lists:foldl(fun(Def, Acc) ->
        SigIn = element(2, Def),
        Target = case SigIn of
            [{T, _} | _] -> T;
            [T | _] when is_atom(T) -> T;
            [] -> 'Any'
        end,
        maps:update_with(Target, fun(L) -> L ++ [Def] end, [Def], Acc)
    end, #{}, Defs),
    maps:to_list(Groups).

broadest_def(Defs) ->
    Broadest = case lists:filter(
        fun(D) ->
            not lists:any(fun(S) -> is_tuple(S) end, element(2, D))
        end, Defs) of
        [D | _] -> D;
        [] -> lists:last(Defs)
    end,
    SigIn = [case S of {T, _} -> T; T -> T end
             || S <- element(2, Broadest)],
    SigOut = element(3, Broadest),
    Guard = case tuple_size(Broadest) of
        5 -> element(5, Broadest);
        _ -> undefined
    end,
    {SigIn, SigOut, Guard}.




locate_runtime_a4() ->
    DevPath = "src/bootstrap/hos/runtime.a4",
    PrivPath = case code:priv_dir(actorforth) of
        {error, _} -> "priv/hos/runtime.a4";
        Dir        -> filename:join([Dir, "hos", "runtime.a4"])
    end,
    case {filelib:is_regular(DevPath), filelib:is_regular(PrivPath)} of
        {true,  _}    -> {ok, DevPath};
        {_,     true} -> {ok, PrivPath};
        _             -> not_found
    end.


%% ---------------------------------------------------------------
%% Public API: thin wrappers over the a4 wire format
%% ---------------------------------------------------------------

%% spawn_system/1 drives the a4 `spawn-system` word via the
%% interpreter. The HosBlueprint is pushed as-is — the DSL already
%% builds it as a flat-tuple product-type instance whose element(1)
%% is the 'HosBlueprint' type tag, so no extra wrapping is needed.
spawn_system(HosBlueprint) ->
    load_a4_runtime(),
    Cont0 = (af_interpreter:new_continuation())#continuation{
        data_stack = [HosBlueprint]
    },
    Cont1 = af_interpreter:interpret_token(
        #token{value = "spawn-system"}, Cont0),
    case Cont1#continuation.data_stack of
        [{'Actor', #{pid := Pid}}] -> Pid;
        Other ->
            error({hos_runtime_spawn_system_unexpected, Other})
    end.

send_event(Pid, EventName, Args) when is_atom(EventName), is_list(Args) ->
    Pid ! {af_msg, {'HosCast', EventName, Args}},
    ok;
send_event(Pid, EventName, Args) when is_list(EventName) ->
    send_event(Pid, list_to_atom(EventName), Args).

send_event_call(Pid, EventName, Args, Timeout) when is_atom(EventName) ->
    Ref = erlang:make_ref(),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    Pid ! {af_msg, {'HosCall', EventName, Args, From, Ref}},
    receive
        {af_msg, {'Tuple', {Ref, Reply}}} -> {ok, Reply}
    after Timeout ->
        {error, timeout}
    end.

stop_system(Pid) ->
    Pid ! {af_msg, {'HosStop', normal}},
    ok.

%% Test-only hook: fetch the per-actor event log. The a4 runtime
%% replies with `{af_msg, {'Tuple', {Ref, Log}}}`; we unwrap and
%% normalise to the chronological list-of-strings shape that the
%% old all-Erlang runtime's get_log used, so existing tests observing
%% `["step"]`-style output keep passing.
get_log(Pid) ->
    Ref = erlang:make_ref(),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    Pid ! {af_msg, {'HosGetLog', From, Ref}},
    receive
        {af_msg, {'Tuple', {Ref, Log}}} ->
            [event_to_string(E) || E <- lists:reverse(Log)]
    after 1000 ->
        {error, timeout}
    end.

event_to_string(A) when is_atom(A) -> atom_to_list(A);
event_to_string(B) when is_binary(B) -> binary_to_list(B);
event_to_string(S) when is_list(S) -> S.

%% Test-only hook: look up a child PID by name.
introspect_child(Pid, ChildName) ->
    ChildKey = case ChildName of
        B when is_binary(B) -> B;
        L when is_list(L)   -> list_to_binary(L);
        A when is_atom(A)   -> atom_to_binary(A, utf8)
    end,
    Ref = erlang:make_ref(),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    Pid ! {af_msg, {'HosIntrospectChild', ChildKey, From, Ref}},
    receive
        {af_msg, {'Tuple', {Ref, Result}}} -> Result
    after 1000 ->
        {error, timeout}
    end.


%% ---------------------------------------------------------------
%% (Ref creation is erlang:make_ref/0 inline at the call sites — no
%% helper needed. Refs are GC'd, so unlike the old atom correlators
%% there's no risk of exhausting the atom table under sustained load.)
%% ---------------------------------------------------------------


%% ---------------------------------------------------------------
%% a4 HOS-runtime helper: stateless handler-body dispatch
%% ---------------------------------------------------------------
%%
%% Called from the a4 `dispatch-stateless` word via erlang-apply.
%% For the given event, finds the matching handler and iterates its
%% body in (Target, EventName) pairs. For each pair whose target
%% resolves to a child's Actor, sends {af_msg, HosCast(event, [])}.
%% Returns HosSelf unchanged.
%%
%% Scoped-to-Erlang rationale: pair-wise iteration over a List in
%% pure a4 hits a stack-management wall without list pattern matching
%% or a return stack. The rest of the HOS runtime remains a4.

a4_exec_stateless_body(#{type := 'HosSelf'} = HosSelf, Event) ->
    Handlers = maps:get(handlers, HosSelf),
    ChildMap = maps:get('child-map', HosSelf),
    case find_body_for_event(Event, Handlers) of
        not_found -> HosSelf;
        Body ->
            exec_body_pairs(Body, ChildMap),
            HosSelf
    end;
a4_exec_stateless_body(HosSelf, Event)
  when is_tuple(HosSelf), element(1, HosSelf) =:= 'HosSelf' ->
    Handlers = element(9, HosSelf),
    ChildMap = element(4, HosSelf),
    case find_body_for_event(Event, Handlers) of
        not_found -> HosSelf;
        Body ->
            exec_body_pairs(Body, ChildMap),
            HosSelf
    end.

%% List elements may arrive raw or wrapped as {'Tuple', Inner}
%% depending on whether the list was built directly in Erlang or
%% extracted through the a4 `elt` path (to_stack_item wraps tuples).
unwrap_tuple({'Tuple', T}) when is_tuple(T) -> T;
unwrap_tuple(T) when is_tuple(T) -> T.

find_body_for_event(_Event, []) -> not_found;
find_body_for_event(Event, [H0 | Rest]) ->
    H = unwrap_tuple(H0),
    case element(1, H) =:= 'HandlerSpec' andalso element(2, H) =:= Event of
        true  -> element(5, H);
        false -> find_body_for_event(Event, Rest)
    end.

exec_body_pairs([], _ChildMap) -> ok;
exec_body_pairs([_Single], _ChildMap) -> ok;
exec_body_pairs([TargetItem, EventItem | Rest], ChildMap) ->
    dispatch_stateless_send(TargetItem, EventItem, ChildMap),
    exec_body_pairs(Rest, ChildMap).

dispatch_stateless_send({'String', _} = Key, {'String', EventBin}, ChildMap) ->
    case maps:find(Key, ChildMap) of
        {ok, {'Actor', #{pid := Pid}}} ->
            EventAtom = binary_to_atom(EventBin, utf8),
            Pid ! {af_msg, {'HosCast', EventAtom, []}},
            ok;
        _ -> ok
    end;
dispatch_stateless_send(_Target, _Event, _ChildMap) ->
    ok.
