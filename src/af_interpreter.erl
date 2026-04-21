-module(af_interpreter).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([interpret_tokens/2, interpret_fast/2, interpret_traced/2,
         interpret_token/2, interpret_token_traced/2, new_continuation/0]).

-spec new_continuation() -> #continuation{}.
new_continuation() ->
    #continuation{dictionary = af_type:snapshot()}.

%% Gate at interpret_tokens/2 entry: production (tracing = false) takes the
%% fast path, which is bit-identical to the historical hot path. Test mode
%% (tracing = true) diverts to interpret_traced, which records one exec
%% event and updates depth stats per dispatch.
-spec interpret_tokens([#token{}], #continuation{}) -> #continuation{}.
interpret_tokens(Tokens, #continuation{tracing = false} = Cont) ->
    interpret_fast(Tokens, Cont);
interpret_tokens(Tokens, #continuation{tracing = true} = Cont) ->
    interpret_traced(Tokens, Cont).

%% Fast path — today's hot loop with one extra clause so that mid-batch
%% `trace-on` starts recording events from the next token. The extra guard
%% only matches when tracing has just been switched on; in steady-state
%% production (tracing = false) BEAM's clause selection skips it for free.
-spec interpret_fast([#token{}], #continuation{}) -> #continuation{}.
interpret_fast([], Cont) ->
    Cont;
interpret_fast([_ | _] = Tokens, #continuation{tracing = true} = Cont) ->
    interpret_traced(Tokens, Cont);
interpret_fast([Token | Rest], Cont) ->
    NewCont = interpret_token(Token, Cont),
    interpret_fast(Rest, NewCont).

%% Traced path — same dispatch, plus event push + depth-stats update per token.
%% Symmetric mid-stream check so `trace-off` stops recording immediately.
-spec interpret_traced([#token{}], #continuation{}) -> #continuation{}.
interpret_traced([], Cont) ->
    Cont;
interpret_traced([_ | _] = Tokens, #continuation{tracing = false} = Cont) ->
    interpret_fast(Tokens, Cont);
interpret_traced([Token | Rest], Cont) ->
    NewCont = interpret_token_traced(Token, Cont),
    interpret_traced(Rest, NewCont).

%% The outer interpreter. For each token:
%% 1. Search TOS type's dictionary
%% 2. If not found, check TOS type's handler (compiler states intercept here)
%% 3. If no handler, search Any dictionary
%% 4. If still not found, try literal handlers (Int, Bool, etc.)
%% 5. If still not found, push as Atom
-spec interpret_token(#token{}, #continuation{}) -> #continuation{}.
interpret_token(#token{value = Value, quoted = Quoted} = Token, Cont) ->
    Stack = Cont#continuation.data_stack,
    Debug = Cont#continuation.debug,
    Cont1 = Cont#continuation{current_token = Token},
    %% Auto-field bindings: unless quoted, check the top locals frame
    %% for a matching name. Locals win over dictionary dispatch. Inline
    %% the common case (empty locals) to skip a function call per token.
    Locals = Cont#continuation.locals,
    case Quoted of
        true ->
            dispatch_no_local(Value, Token, Stack, Debug, Cont1, Cont);
        false ->
            case Locals of
                [] ->
                    dispatch_no_local(Value, Token, Stack, Debug, Cont1, Cont);
                [Frame | _] when is_map(Frame) ->
                    case maps:find(Value, Frame) of
                        {ok, LocalVal} ->
                            Cont1#continuation{data_stack = [LocalVal | Stack]};
                        error ->
                            dispatch_no_local(Value, Token, Stack, Debug, Cont1, Cont)
                    end;
                _ ->
                    dispatch_no_local(Value, Token, Stack, Debug, Cont1, Cont)
            end
    end.

dispatch_no_local(Value, Token, Stack, Debug, Cont1, Cont) ->
    case Cont#continuation.dictionary of
        undefined ->
            interpret_token_ets(Value, Token, Stack, Debug, Cont1);
        Dict ->
            interpret_token_dict(Value, Token, Stack, Dict, Debug, Cont1)
    end.

%% Look up Name in the top locals frame. Returns {ok, Value} or false.
%% Kept for callers that still need a fun-form check.
lookup_local(_Name, []) -> false;
lookup_local(Name, [Frame | _]) when is_map(Frame) ->
    case maps:find(Name, Frame) of
        {ok, V} -> {ok, V};
        error   -> false
    end;
lookup_local(_, _) -> false.

%% Fast path: local dictionary lookup (no ETS)
interpret_token_dict(Value, Token, Stack, Dict, Debug, Cont1) ->
    case Token#token.quoted of
        true ->
            %% Quoted strings skip dictionary lookups (steps 1 & 3)
            %% but still go through handlers (step 2) so compiler can
            %% compile them into word bodies as string literal pushes.
            case get_tos_handler(Stack, Dict) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Handler(Value, Cont1);
                none ->
                    StringVal = {'String', list_to_binary(Value)},
                    debug_trace(Debug, Value, Stack, {literal, StringVal}),
                    Cont1#continuation{data_stack = [StringVal | Stack]}
            end;
        false ->
            dispatch_unquoted(Value, Stack, Dict, Debug, Cont1)
    end.

%% Unquoted dispatch with a monomorphic inline cache keyed on the top-3 stack
%% types plus the token value. The 3-deep key is enough to make dispatch
%% deterministic for every op in the codebase (the deepest sig_in is 4, and
%% we skip caching entries that would resolve against that one op).
%%
%% Cache is invalidated by clearing `dispatch_cache` whenever the continuation
%% dictionary is updated (word definition, compile, type definition).
%% Hottest path in the interpreter. Inlines stack_key + the maps:find
%% + the three cache-entry cases so each per-token dispatch is a
%% single function call plus one maps:find + one Impl call on a hit.
dispatch_unquoted(Value, Stack, Dict, Debug, Cont1) ->
    Key = case Stack of
        [A, B, C | _] -> {Value, tag(A), tag(B), tag(C)};
        [A, B]        -> {Value, tag(A), tag(B), none};
        [A]           -> {Value, tag(A), none, none};
        []            -> {Value, none, none, none}
    end,
    case maps:find(Key, Cont1#continuation.dispatch_cache) of
        {ok, {op, Impl}} ->
            debug_trace(Debug, Value, Stack, cache_op),
            Impl(Cont1);
        {ok, {literal, TypedValue}} ->
            debug_trace(Debug, Value, Stack, {literal, TypedValue}),
            Cont1#continuation{data_stack = [TypedValue | Stack]};
        {ok, atom} ->
            debug_trace(Debug, Value, Stack, atom),
            Cont1#continuation{data_stack = [{'Atom', Value} | Stack]};
        error ->
            full_dispatch_and_cache(Value, Stack, Dict, Debug, Cont1, Key)
    end.

full_dispatch_and_cache(Value, Stack, Dict, Debug, Cont1, Key) ->
    TosType = case Key of {_, T0, _, _} -> T0 end,
    case af_type:dict_find_op_in_tos(Value, Stack, Dict) of
        {ok, #operation{impl = Impl, sig_in = SigIn, guard = Guard} = Op} ->
            debug_trace(Debug, Value, Stack, {tos, Op}),
            ClauseCount = af_type:dict_op_clause_count(TosType, Value, Dict),
            maybe_cache(Key, SigIn, Guard, ClauseCount, Cont1, {op, Impl}, Impl(Cont1));
        not_found ->
            case get_tos_handler(Stack, Dict) of
                {ok, Handler} ->
                    %% Handler dispatch is stateful — never cache.
                    debug_trace(Debug, Value, Stack, handler),
                    Handler(Value, Cont1);
                none ->
                    case af_type:dict_find_op_in_any(Value, Stack, Dict) of
                        {ok, #operation{impl = Impl, sig_in = SigIn, guard = Guard} = Op} ->
                            debug_trace(Debug, Value, Stack, {any, Op}),
                            AnyCount = af_type:dict_op_clause_count('Any', Value, Dict),
                            maybe_cache(Key, SigIn, Guard, AnyCount, Cont1, {op, Impl}, Impl(Cont1));
                        not_found ->
                            case try_literals(Value, Dict) of
                                {ok, TypedValue} ->
                                    debug_trace(Debug, Value, Stack, {literal, TypedValue}),
                                    Result = Cont1#continuation{data_stack = [TypedValue | Stack]},
                                    maybe_cache(Key, [], undefined, 1, Cont1, {literal, TypedValue}, Result);
                                not_found ->
                                    debug_trace(Debug, Value, Stack, atom),
                                    Result = Cont1#continuation{data_stack = [{'Atom', Value} | Stack]},
                                    maybe_cache_atom(Key, Value, Cont1, Result)
                            end
                    end
            end
    end.

%% Compute a cache key from Token value and top-3 stack types.
%% Positions below the stack depth use 'none'. Non-atom-tagged stack items
%% force the whole key to be skipped (we return a sentinel that never hits
%% the cache) so legacy raw stack items don't corrupt the cache.
%% Hot path. Inline type_tag for each of the top-3 stack items so
%% dispatch_unquoted does one function call (stack_key) and the
%% body avoids three more. Bool values are folded into the tag so
%% Bool-value-constrained sub-clauses cache independently.
stack_key(Value, [A, B, C | _]) ->
    {Value, tag(A), tag(B), tag(C)};
stack_key(Value, [A, B]) ->
    {Value, tag(A), tag(B), none};
stack_key(Value, [A]) ->
    {Value, tag(A), none, none};
stack_key(Value, []) ->
    {Value, none, none, none}.

%% Inlinable version of type_tag; keep the exported name too for
%% callers that use it directly (stats tooling etc.).
tag({'Bool', V})                                    -> {'Bool', V};
tag(T) when is_tuple(T), tuple_size(T) >= 2 ->
    case element(1, T) of
        First when is_atom(First) -> First;
        _                         -> '$uncacheable'
    end;
tag(_)                                              -> '$uncacheable'.

%% Cache-key type tag. For Bool/Atom-tagged stack items we fold the
%% value into the tag so sub-clauses that dispatch on a value
%% constraint (hos-loop-step / dispatch-*-step, keyed on True/False)
%% cache each branch independently.  The value is only in the key,
%% it's not compared at dispatch — full_dispatch_and_cache still
%% runs its normal match_sig for the initial miss.
type_tag(Item) when is_tuple(Item), tuple_size(Item) >= 2 ->
    case element(1, Item) of
        'Bool' -> {'Bool', element(2, Item)};
        First when is_atom(First) -> First;
        _ -> '$uncacheable'
    end;
type_tag(_) -> '$uncacheable'.

%% Cache a dispatch result only when it's safe to replay:
%%  - TOS type has no handler (dispatch is state-dependent otherwise)
%%  - sig_in length <= 3 (the key-depth, so stack shape fully determines match)
%%  - no value constraints in sig_in (tuple entries trigger different clauses)
%%  - no guard expression (runtime-checked, not key-determined)
%%  - only one clause under this name (multi-clause dispatch picks by stack
%%    value, our key captures only types)
maybe_cache(Key, SigIn, Guard, ClauseCount, CallCont, CacheEntry, Result) ->
    case should_cache(Key, SigIn, Guard, ClauseCount, CallCont) of
        false -> Result;
        true ->
            OldCache = CallCont#continuation.dispatch_cache,
            ResCache = Result#continuation.dispatch_cache,
            %% If the callee swapped out the cache (e.g. word definition
            %% cleared it), respect that — otherwise merge our new entry in.
            MergedCache = case ResCache =:= OldCache orelse map_size(ResCache) >= map_size(OldCache) of
                true  -> maps:put(Key, CacheEntry, ResCache);
                false -> ResCache
            end,
            Result#continuation{dispatch_cache = MergedCache}
    end.

%% Atom cache: safe only if NO op with this name exists in the dictionary.
%% If any op exists, the atom fallback was the fallthrough for THIS specific
%% stack shape; a different stack with the same 3-deep key could match it.
maybe_cache_atom(Key, Value, CallCont, Result) ->
    Dict = CallCont#continuation.dictionary,
    case any_op_named(Value, Dict) of
        true  -> Result;
        false -> maybe_cache(Key, [], undefined, 1, CallCont, atom, Result)
    end.

should_cache({_, '$uncacheable', _, _}, _, _, _, _) -> false;
should_cache({_, _, '$uncacheable', _}, _, _, _, _) -> false;
should_cache({_, _, _, '$uncacheable'}, _, _, _, _) -> false;
should_cache(_Key, _SigIn, Guard, _ClauseCount, _Cont) when Guard =/= undefined ->
    false;
should_cache(_Key, _SigIn, _Guard, ClauseCount, _Cont) when ClauseCount > 1 ->
    false;
should_cache({_, T0, _, _}, SigIn, _Guard, _ClauseCount, Cont) ->
    length(SigIn) =< 3
        andalso not has_uncacheable_value_constraint(SigIn)
        andalso not tos_has_handler(T0, Cont#continuation.dictionary).

%% Value constraints that the cache key fully distinguishes (Bool
%% values are folded into the type_tag above) are safe to cache —
%% different literals produce different keys. Int/String/other
%% value constraints still disable caching for now.
has_uncacheable_value_constraint([]) -> false;
has_uncacheable_value_constraint([{'Bool', _} | Rest]) ->
    has_uncacheable_value_constraint(Rest);
has_uncacheable_value_constraint([{_, _} | _]) -> true;
has_uncacheable_value_constraint([_ | Rest]) ->
    has_uncacheable_value_constraint(Rest).

any_op_named(Name, Dict) when is_map(Dict) ->
    maps:fold(fun(_, #af_type{ops = Ops}, Acc) ->
        Acc orelse maps:is_key(Name, Ops)
    end, false, Dict);
any_op_named(_, _) -> false.

tos_has_handler(none, _Dict) -> false;
tos_has_handler(TosType, Dict) ->
    case af_type:dict_get_type(TosType, Dict) of
        {ok, #af_type{handler = Handler}} when Handler =/= undefined -> true;
        _ -> false
    end.

%% Legacy path: ETS-based lookup (for continuations without dictionary)
interpret_token_ets(Value, Token, Stack, Debug, Cont1) ->
    case Token#token.quoted of
        true ->
            %% Quoted strings skip dictionary lookups (steps 1 & 3)
            %% but still go through handlers (step 2) so compiler can
            %% compile them into word bodies as string literal pushes.
            case get_tos_handler(Stack) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Handler(Value, Cont1);
                none ->
                    StringVal = {'String', list_to_binary(Value)},
                    debug_trace(Debug, Value, Stack, {literal, StringVal}),
                    Cont1#continuation{data_stack = [StringVal | Stack]}
            end;
        false ->
            interpret_unquoted(Value, Stack, Debug, Cont1)
    end.

interpret_unquoted(Value, Stack, Debug, Cont1) ->
    case af_type:find_op_in_tos(Value, Stack) of
        {ok, #operation{impl = Impl} = Op} ->
            debug_trace(Debug, Value, Stack, {tos, Op}),
            Impl(Cont1);
        not_found ->
            case get_tos_handler(Stack) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Handler(Value, Cont1);
                none ->
                    case af_type:find_op_in_any(Value, Stack) of
                        {ok, #operation{impl = Impl} = Op} ->
                            debug_trace(Debug, Value, Stack, {any, Op}),
                            Impl(Cont1);
                        not_found ->
                            case try_literals_ets(Value) of
                                {ok, TypedValue} ->
                                    debug_trace(Debug, Value, Stack, {literal, TypedValue}),
                                    Cont1#continuation{data_stack = [TypedValue | Stack]};
                                not_found ->
                                    debug_trace(Debug, Value, Stack, atom),
                                    Cont1#continuation{data_stack = [{'Atom', Value} | Stack]}
                            end
                    end
            end
    end.

%%% Traced dispatch path
%%% Parallel to interpret_token + its helpers, with a record_event/3 call
%%% at each dispatch outcome. The traced path mirrors the fast path's shape
%%% exactly so a failing test's event sequence reflects what really happened.

-spec interpret_token_traced(#token{}, #continuation{}) -> #continuation{}.
interpret_token_traced(#token{value = Value, quoted = Quoted} = Token, Cont) ->
    Stack = Cont#continuation.data_stack,
    Debug = Cont#continuation.debug,
    Cont1 = Cont#continuation{current_token = Token},
    case Quoted =:= false andalso lookup_local(Value, Cont#continuation.locals) of
        {ok, LocalVal} ->
            Cont1#continuation{data_stack = [LocalVal | Stack]};
        _ ->
            case Cont#continuation.dictionary of
                undefined ->
                    interpret_token_ets_traced(Value, Token, Stack, Debug, Cont1);
                Dict ->
                    interpret_token_dict_traced(Value, Token, Stack, Dict, Debug, Cont1)
            end
    end.

interpret_token_dict_traced(Value, Token, Stack, Dict, Debug, Cont1) ->
    case Token#token.quoted of
        true ->
            case get_tos_handler(Stack, Dict) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Result = Handler(Value, Cont1),
                    record_event(Token, handler, Result);
                none ->
                    StringVal = {'String', list_to_binary(Value)},
                    debug_trace(Debug, Value, Stack, {literal, StringVal}),
                    Result = Cont1#continuation{data_stack = [StringVal | Stack]},
                    record_event(Token, {literal, StringVal}, Result)
            end;
        false ->
            dispatch_unquoted_traced(Value, Token, Stack, Dict, Debug, Cont1)
    end.

dispatch_unquoted_traced(Value, Token, Stack, Dict, Debug, Cont1) ->
    Key = stack_key(Value, Stack),
    Cache = Cont1#continuation.dispatch_cache,
    case maps:find(Key, Cache) of
        {ok, {op, Impl} = Entry} ->
            debug_trace(Debug, Value, Stack, cache_op),
            Result = Impl(Cont1),
            record_event(Token, {cache_hit, Entry}, Result);
        {ok, {literal, TypedValue} = Entry} ->
            debug_trace(Debug, Value, Stack, {literal, TypedValue}),
            Result = Cont1#continuation{data_stack = [TypedValue | Stack]},
            record_event(Token, {cache_hit, Entry}, Result);
        {ok, atom} ->
            debug_trace(Debug, Value, Stack, atom),
            Result = Cont1#continuation{data_stack = [{'Atom', Value} | Stack]},
            record_event(Token, {cache_hit, atom}, Result);
        error ->
            full_dispatch_and_cache_traced(Value, Token, Stack, Dict, Debug, Cont1, Key)
    end.

full_dispatch_and_cache_traced(Value, Token, Stack, Dict, Debug, Cont1, Key) ->
    TosType = case Key of {_, T0, _, _} -> T0 end,
    case af_type:dict_find_op_in_tos(Value, Stack, Dict) of
        {ok, #operation{impl = Impl, sig_in = SigIn, guard = Guard} = Op} ->
            debug_trace(Debug, Value, Stack, {tos, Op}),
            ClauseCount = af_type:dict_op_clause_count(TosType, Value, Dict),
            Cached = maybe_cache(Key, SigIn, Guard, ClauseCount, Cont1, {op, Impl}, Impl(Cont1)),
            record_event(Token, {tos, Op}, Cached);
        not_found ->
            case get_tos_handler(Stack, Dict) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Result = Handler(Value, Cont1),
                    record_event(Token, handler, Result);
                none ->
                    case af_type:dict_find_op_in_any(Value, Stack, Dict) of
                        {ok, #operation{impl = Impl, sig_in = SigIn, guard = Guard} = Op} ->
                            debug_trace(Debug, Value, Stack, {any, Op}),
                            AnyCount = af_type:dict_op_clause_count('Any', Value, Dict),
                            Cached = maybe_cache(Key, SigIn, Guard, AnyCount, Cont1, {op, Impl}, Impl(Cont1)),
                            record_event(Token, {any, Op}, Cached);
                        not_found ->
                            case try_literals(Value, Dict) of
                                {ok, TypedValue} ->
                                    debug_trace(Debug, Value, Stack, {literal, TypedValue}),
                                    Result = Cont1#continuation{data_stack = [TypedValue | Stack]},
                                    Cached = maybe_cache(Key, [], undefined, 1, Cont1, {literal, TypedValue}, Result),
                                    record_event(Token, {literal, TypedValue}, Cached);
                                not_found ->
                                    debug_trace(Debug, Value, Stack, atom),
                                    Result = Cont1#continuation{data_stack = [{'Atom', Value} | Stack]},
                                    Cached = maybe_cache_atom(Key, Value, Cont1, Result),
                                    record_event(Token, atom, Cached)
                            end
                    end
            end
    end.

interpret_token_ets_traced(Value, Token, Stack, Debug, Cont1) ->
    case Token#token.quoted of
        true ->
            case get_tos_handler(Stack) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Result = Handler(Value, Cont1),
                    record_event(Token, handler, Result);
                none ->
                    StringVal = {'String', list_to_binary(Value)},
                    debug_trace(Debug, Value, Stack, {literal, StringVal}),
                    Result = Cont1#continuation{data_stack = [StringVal | Stack]},
                    record_event(Token, {literal, StringVal}, Result)
            end;
        false ->
            interpret_unquoted_traced(Value, Token, Stack, Debug, Cont1)
    end.

interpret_unquoted_traced(Value, Token, Stack, Debug, Cont1) ->
    case af_type:find_op_in_tos(Value, Stack) of
        {ok, #operation{impl = Impl} = Op} ->
            debug_trace(Debug, Value, Stack, {tos, Op}),
            Result = Impl(Cont1),
            record_event(Token, {tos, Op}, Result);
        not_found ->
            case get_tos_handler(Stack) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Result = Handler(Value, Cont1),
                    record_event(Token, handler, Result);
                none ->
                    case af_type:find_op_in_any(Value, Stack) of
                        {ok, #operation{impl = Impl} = Op} ->
                            debug_trace(Debug, Value, Stack, {any, Op}),
                            Result = Impl(Cont1),
                            record_event(Token, {any, Op}, Result);
                        not_found ->
                            case try_literals_ets(Value) of
                                {ok, TypedValue} ->
                                    debug_trace(Debug, Value, Stack, {literal, TypedValue}),
                                    Result = Cont1#continuation{data_stack = [TypedValue | Stack]},
                                    record_event(Token, {literal, TypedValue}, Result);
                                not_found ->
                                    debug_trace(Debug, Value, Stack, atom),
                                    Result = Cont1#continuation{data_stack = [{'Atom', Value} | Stack]},
                                    record_event(Token, atom, Result)
                            end
                    end
            end
    end.

%% Record one exec-stack event, update depth stats, and add the token's
%% position to the coverage map. Called only in the traced path.
%%
%% If the op we just ran flipped tracing off (e.g. `trace-off`), skip
%% the event so control-plane ops aren't part of their own trace.
record_event(_Token, _Kind, #continuation{tracing = false} = Cont) ->
    Cont;
record_event(Token, Kind, #continuation{data_stack = DS,
                                        exec_stack = ES,
                                        depth_stats = Stats0,
                                        coverage = Cov0} = Cont) ->
    Event = {exec, Token, DS, Kind},
    Stats1 = case Stats0 of
        undefined -> #depth_stats{};
        S -> S
    end,
    DataDepth = length(DS),
    Stats2 = Stats1#depth_stats{
        data_max   = erlang:max(Stats1#depth_stats.data_max, DataDepth),
        data_sum   = Stats1#depth_stats.data_sum + DataDepth,
        count      = Stats1#depth_stats.count + 1
    },
    Cov1 = record_coverage(Token, Cov0),
    Cont#continuation{exec_stack = [Event | ES],
                      depth_stats = Stats2,
                      coverage = Cov1}.

%% Stamp one {File, {Line, Col}} position. Each file gets a map of
%% positions to visit counts (so future passes can report hot sites).
record_coverage(#token{file = File, line = L, column = C}, Cov)
  when File =/= undefined ->
    FileBin = case is_binary(File) of
        true  -> File;
        false -> list_to_binary(File)
    end,
    Positions = maps:get(FileBin, Cov, #{}),
    Key = {L, C},
    NewPositions = maps:put(Key, maps:get(Key, Positions, 0) + 1, Positions),
    Cov#{FileBin => NewPositions};
record_coverage(_, Cov) -> Cov.

%%% Literal handlers

try_literals(TokenValue, Dict) ->
    AllTypes = af_type:dict_all_types(Dict),
    try_literal_types(TokenValue, AllTypes).

try_literals_ets(TokenValue) ->
    AllTypes = af_type:all_types(),
    try_literal_types(TokenValue, AllTypes).

try_literal_types(_TokenValue, []) -> not_found;
try_literal_types(TokenValue, [#af_type{ops = Ops} | Rest]) ->
    case maps:get("literal", Ops, []) of
        [] -> try_literal_types(TokenValue, Rest);
        [#operation{impl = Impl} | _] ->
            TempCont = #continuation{data_stack = [{'Atom', TokenValue}]},
            try
                ResultCont = Impl(TempCont),
                [TypedValue | _] = ResultCont#continuation.data_stack,
                {ok, TypedValue}
            catch _:_ ->
                try_literal_types(TokenValue, Rest)
            end
    end.

%%% Debug trace

debug_trace(false, _, _, _) -> ok;
debug_trace(true, Value, Stack, Dispatch) ->
    StackStr = format_stack(Stack),
    DispatchStr = format_dispatch(Dispatch),
    io:format("[~s] ~s -> ~s~n", [StackStr, Value, DispatchStr]).

format_stack([]) -> "";
format_stack(Stack) ->
    Items = lists:map(fun({Type, Val}) ->
        io_lib:format("~p(~p)", [Type, Val])
    end, Stack),
    lists:join(" ", Items).

format_dispatch({tos, #operation{name = Name}}) ->
    TosInfo = io_lib:format("TOS.~s", [Name]),
    lists:flatten(TosInfo);
format_dispatch(cache_op) -> "cache";
format_dispatch(handler) -> "handler";
format_dispatch({any, #operation{name = Name}}) ->
    AnyInfo = io_lib:format("Any.~s", [Name]),
    lists:flatten(AnyInfo);
format_dispatch({literal, {Type, _Val}}) ->
    LitInfo = io_lib:format("literal->~p", [Type]),
    lists:flatten(LitInfo);
format_dispatch(atom) -> "->Atom".

%%% Internal

get_tos_handler([]) -> none;
get_tos_handler([TosItem | _]) when is_tuple(TosItem), tuple_size(TosItem) >= 2 ->
    TosType = element(1, TosItem),
    case is_atom(TosType) of
        true ->
            case af_type:get_type(TosType) of
                {ok, #af_type{handler = Handler}} when Handler =/= undefined ->
                    {ok, Handler};
                _ -> none
            end;
        false -> none
    end;
get_tos_handler(_) -> none.

get_tos_handler([], _Dict) -> none;
get_tos_handler([TosItem | _], Dict) when is_tuple(TosItem), tuple_size(TosItem) >= 2 ->
    TosType = element(1, TosItem),
    case is_atom(TosType) of
        true ->
            case af_type:dict_get_type(TosType, Dict) of
                {ok, #af_type{handler = Handler}} when Handler =/= undefined ->
                    {ok, Handler};
                _ -> none
            end;
        false -> none
    end;
get_tos_handler(_, _) -> none.
