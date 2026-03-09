-module(af_type_actor).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).
-export([send_cast/3, send_call/3]).  %% Used by compiled word code
-export([compile_actor_loop/1]).       %% Generate specialized receive loop
-export([actor_loop/2, actor_loop/3]). %% Generic loop (exported for compiled loops)
-export([execute_actor_word/4, execute_actor_call/4, separate_reply/2]). %% For compiled loop fallback
-export([to_atom_key/1, to_string_key/1]). %% Key conversion helpers
-export([actor_loop_module_name/1]). %% For testing

%% Actor model:
%%   Any type instance can become an actor via 'server'.
%%   server   — ( TypeInstance -- Actor )  spawn actor from type instance
%%   <<       — ( Actor -- ActorSend )    enter actor-send mode
%%   >>       — ( ActorSend -- Actor )    finish send block
%%   stop     — sent via << stop >> to terminate actor
%%
%% Inside << ... . :
%%   - Non-actor-vocab tokens execute locally on a temp stack
%%   - Actor-vocab tokens pop matching args from temp stack,
%%     package as message, dispatch to actor
%%   - Cast (no return beyond state type) = async
%%   - Call (returns beyond state type) = sync, reply pushed to temp stack
%%
%% Actor state is private. Auto-generated accessors (getters/setters)
%% are NOT callable through <<. Only user-defined words form the
%% remote interface.

init() ->
    af_type:register_type(#af_type{name = 'Actor'}),
    af_type:register_type(#af_type{name = 'ActorSend'}),

    %% server: pops type instance, spawns actor, pushes Actor ref
    af_type:add_op('Any', #operation{
        name = "server",
        sig_in = ['Any'],
        sig_out = ['Actor'],
        impl = fun op_server/1
    }),

    %% <<: enters actor-send mode (in Actor dict)
    af_type:add_op('Actor', #operation{
        name = "<<",
        sig_in = ['Actor'],
        sig_out = ['ActorSend'],
        impl = fun op_send_begin/1
    }),

    %% >> in ActorSend: finishes send block
    af_type:add_op('ActorSend', #operation{
        name = ">>",
        sig_in = ['ActorSend'],
        sig_out = [],
        impl = fun op_send_end/1
    }),

    %% supervised-server: like server but under OTP supervision
    af_type:add_op('Any', #operation{
        name = "supervised-server",
        sig_in = ['Any'],
        sig_out = ['Actor'],
        impl = fun op_supervised_server/1
    }),

    %% self: push current process pid as Actor (no vocab — raw Actor)
    af_type:add_op('Any', #operation{
        name = "self",
        sig_in = [],
        sig_out = ['Actor'],
        impl = fun op_self/1
    }),

    %% Set handler for ActorSend
    af_type:register_type(#af_type{
        name = 'ActorSend',
        ops = get_ops('ActorSend'),
        handler = fun handle_actor_send/2
    }),

    %% --- Milestone 7.3: Typed Actor Messages ---

    %% spawn: ( Atom -- Actor ) spawn process running a named word
    af_type:add_op('Any', #operation{
        name = "spawn",
        sig_in = ['Atom'],
        sig_out = ['Actor'],
        impl = fun op_spawn/1
    }),

    %% send / !: ( Any Actor -- ) send value to actor mailbox
    af_type:add_op('Actor', #operation{
        name = "send",
        sig_in = ['Actor', 'Any'],
        sig_out = [],
        impl = fun op_send/1
    }),
    af_type:add_op('Actor', #operation{
        name = "!",
        sig_in = ['Actor', 'Any'],
        sig_out = [],
        impl = fun op_send/1
    }),

    %% receive: ( -- Any ) blocking receive from mailbox
    af_type:add_op('Any', #operation{
        name = "receive",
        sig_in = [],
        sig_out = ['Any'],
        impl = fun op_receive/1
    }),

    %% receive-timeout: ( Int -- Any Bool ) receive with timeout in ms
    af_type:add_op('Any', #operation{
        name = "receive-timeout",
        sig_in = ['Int'],
        sig_out = ['Any', 'Bool'],
        impl = fun op_receive_timeout/1
    }),

    %% Message type — typed envelope for inter-actor communication
    af_type:register_type(#af_type{name = 'Message'}),

    %% msg: ( Any String -- Message ) create tagged message
    af_type:add_op('Any', #operation{
        name = "msg",
        sig_in = ['String', 'Any'],
        sig_out = ['Message'],
        impl = fun op_msg/1
    }),

    %% msg-tag: ( Message -- Message String ) get tag (non-destructive)
    af_type:add_op('Message', #operation{
        name = "msg-tag",
        sig_in = ['Message'],
        sig_out = ['String', 'Message'],
        impl = fun op_msg_tag/1
    }),

    %% msg-data: ( Message -- Message Any ) get data (non-destructive)
    af_type:add_op('Message', #operation{
        name = "msg-data",
        sig_in = ['Message'],
        sig_out = ['Any', 'Message'],
        impl = fun op_msg_data/1
    }),

    %% receive-match: ( String -- Any ) selective receive by message tag
    af_type:add_op('Any', #operation{
        name = "receive-match",
        sig_in = ['String'],
        sig_out = ['Any'],
        impl = fun op_receive_match/1
    }),

    %% receive-match-timeout: ( String Int -- Any Bool ) selective receive with timeout
    af_type:add_op('Any', #operation{
        name = "receive-match-timeout",
        sig_in = ['Int', 'String'],
        sig_out = ['Any', 'Bool'],
        impl = fun op_receive_match_timeout/1
    }),

    ok.

get_ops(TypeName) ->
    {ok, #af_type{ops = Ops}} = af_type:get_type(TypeName),
    Ops.

%%% --- server: spawn actor from type instance ---

op_server(Cont) ->
    [{TypeName, _Value} = Instance | Rest] = Cont#continuation.data_stack,
    Vocab0 = build_vocab(TypeName),
    Vocab = Vocab0#{"stop" => [#{args => [], returns => []}]},
    LoopFun = get_actor_loop_fun(TypeName, Instance),
    Pid = erlang:spawn_opt(LoopFun, [link, {min_heap_size, 100000}]),
    ActorVal = #{pid => Pid, type_name => TypeName, vocab => Vocab},
    Cont#continuation{data_stack = [{'Actor', ActorVal} | Rest]}.

%% Try to use a compiled actor loop; fall back to generic loop.
get_actor_loop_fun(TypeName, Instance) ->
    case compile_actor_loop(TypeName) of
        {ok, Mod} -> fun() -> Mod:loop(Instance) end;
        error     -> fun() -> actor_loop(TypeName, Instance) end
    end.

actor_loop_module_name(TypeName) ->
    list_to_atom("af_actor_loop_" ++ string:lowercase(atom_to_list(TypeName))).

%% Build the remotely-callable vocabulary for a type.
%% Only includes user-defined words (source =/= auto) whose sig_in includes the state type.
%% Searches ALL type dicts since multi-type words register in the TOS type's dict.
build_vocab(TypeName) ->
    AllTypes = af_type:all_types(),
    lists:foldl(fun(#af_type{ops = Ops}, OuterAcc) ->
        maps:fold(fun(WordName, OpList, InnerAcc) ->
            Entries = lists:filtermap(fun(#operation{source = Source} = Op) ->
                case Source of
                    auto -> false;
                    _ ->
                        case lists:member(TypeName, Op#operation.sig_in) of
                            true ->
                                ArgsIn = remove_first(TypeName, Op#operation.sig_in),
                                Returns = remove_first(TypeName, Op#operation.sig_out),
                                {true, #{args => ArgsIn, returns => Returns}};
                            false ->
                                false
                        end
                end
            end, OpList),
            case Entries of
                [] -> InnerAcc;
                _ ->
                    Existing = maps:get(WordName, InnerAcc, []),
                    maps:put(WordName, Existing ++ Entries, InnerAcc)
            end
        end, OuterAcc, Ops)
    end, #{}, AllTypes).

%% Remove the first occurrence of an element from a list.
remove_first(_Elem, []) -> [];
remove_first(Elem, [Elem | Rest]) -> Rest;
remove_first(Elem, [H | Rest]) -> [H | remove_first(Elem, Rest)].

%%% --- supervised-server: spawn under OTP supervisor ---

op_supervised_server(Cont) ->
    [{TypeName, _Value} = Instance | Rest] = Cont#continuation.data_stack,
    Vocab0 = build_vocab(TypeName),
    Vocab = Vocab0#{"stop" => [#{args => [], returns => []}]},
    %% Ensure supervisor is running
    ensure_supervisor(),
    {ok, Pid} = af_actor_sup:start_actor(TypeName, Instance),
    ActorVal = #{pid => Pid, type_name => TypeName, vocab => Vocab, supervised => true},
    Cont#continuation{data_stack = [{'Actor', ActorVal} | Rest]}.

ensure_supervisor() ->
    case whereis(af_actor_sup) of
        undefined -> af_actor_sup:start_link();
        _ -> ok
    end.

%%% --- << : begin actor send ---

op_send_begin(Cont) ->
    [{'Actor', ActorInfo} | Rest] = Cont#continuation.data_stack,
    SendState = #{
        actor => {'Actor', ActorInfo},
        local_stack => [],
        vocab => maps:get(vocab, ActorInfo)
    },
    Cont#continuation{data_stack = [{'ActorSend', SendState} | Rest]}.

%%% --- . : end actor send ---

op_send_end(Cont) ->
    [{'ActorSend', State} | Rest] = Cont#continuation.data_stack,
    #{actor := ActorVal, local_stack := LocalStack} = State,
    %% Push Actor back, then any remaining local stack values on top
    Cont#continuation{data_stack = LocalStack ++ [ActorVal | Rest]}.

%%% --- ActorSend handler: intercepts all tokens ---

handle_actor_send(TokenValue, Cont) ->
    [{'ActorSend', State} | Rest] = Cont#continuation.data_stack,
    #{actor := {'Actor', ActorInfo}, local_stack := LocalStack, vocab := Vocab} = State,
    case maps:find(TokenValue, Vocab) of
        {ok, Entries} ->
            %% Actor vocabulary word — find matching entry and dispatch
            dispatch_actor_word(TokenValue, Entries, ActorInfo, LocalStack, State, Rest, Cont);
        error ->
            %% Not an actor word — execute locally on temp stack
            TempCont = #continuation{data_stack = LocalStack},
            Token = #token{value = TokenValue},
            NewTempCont = af_interpreter:interpret_token(Token, TempCont),
            NewLocalStack = NewTempCont#continuation.data_stack,
            NewState = State#{local_stack => NewLocalStack},
            Cont#continuation{data_stack = [{'ActorSend', NewState} | Rest]}
    end.

dispatch_actor_word(WordName, Entries, ActorInfo, LocalStack, State, Rest, Cont) ->
    #{pid := Pid} = ActorInfo,
    IsSupervised = maps:get(supervised, ActorInfo, false),
    Entry = find_matching_entry(Entries, LocalStack),
    #{args := ArgsIn, returns := Returns} = Entry,
    NumArgs = length(ArgsIn),
    case length(LocalStack) < NumArgs of
        true ->
            error({actor_arg_error, WordName,
                {expected, NumArgs, args, got, length(LocalStack)}});
        false -> ok
    end,
    {Args, RemainingStack} = lists:split(NumArgs, LocalStack),
    validate_actor_args(WordName, ArgsIn, Args),
    WordAtom = to_atom_key(WordName),
    case Returns of
        [] ->
            %% Cast: async, no reply
            case IsSupervised of
                true  -> gen_server:cast(Pid, {cast, WordAtom, Args});
                false -> Pid ! {cast, WordAtom, Args}
            end,
            NewState = State#{local_stack => RemainingStack},
            Cont#continuation{data_stack = [{'ActorSend', NewState} | Rest]};
        _ ->
            %% Call: sync, wait for reply
            case IsSupervised of
                true ->
                    {ok, ReplyValues} = gen_server:call(Pid, {call, WordAtom, Args}, 5000),
                    NewLocalStack = ReplyValues ++ RemainingStack,
                    NewState = State#{local_stack => NewLocalStack},
                    Cont#continuation{data_stack = [{'ActorSend', NewState} | Rest]};
                false ->
                    Ref = make_ref(),
                    Pid ! {call, WordAtom, Args, self(), Ref},
                    receive
                        {reply, Ref, ReplyValues} ->
                            NewLocalStack = ReplyValues ++ RemainingStack,
                            NewState = State#{local_stack => NewLocalStack},
                            Cont#continuation{data_stack = [{'ActorSend', NewState} | Rest]}
                    after 5000 ->
                        error({actor_call_timeout, WordName})
                    end
            end
    end.

find_matching_entry([Entry], _Stack) ->
    Entry;
find_matching_entry(Entries, Stack) ->
    case find_typed_match(Entries, Stack) of
        {ok, Entry} -> Entry;
        not_found -> hd(Entries)
    end.

find_typed_match([], _Stack) -> not_found;
find_typed_match([#{args := Args} = Entry | Rest], Stack) ->
    case match_args(Args, Stack) of
        true -> {ok, Entry};
        false -> find_typed_match(Rest, Stack)
    end.

match_args([], _Stack) -> true;
match_args(_, []) -> false;
match_args(['Any' | TRest], [_ | SRest]) -> match_args(TRest, SRest);
match_args([Type | TRest], [{Type, _} | SRest]) -> match_args(TRest, SRest);
match_args(_, _) -> false.

%%% --- self ---

op_self(Cont) ->
    Pid = self(),
    ActorVal = #{pid => Pid, type_name => undefined, vocab => #{}},
    Cont#continuation{
        data_stack = [{'Actor', ActorVal} | Cont#continuation.data_stack]
    }.

%%% --- Actor process loop ---

actor_loop(TypeName, StateInstance) ->
    Cache = af_actor_worker:build_native_cache(TypeName),
    actor_loop(TypeName, StateInstance, Cache).

actor_loop(TypeName, StateInstance, Cache) ->
    receive
        {cast, stop, _Args} ->
            ok;
        {cast, "stop", _Args} ->
            ok;
        {cast, WordName, Args} when is_atom(WordName) ->
            NewState = case maps:find(WordName, Cache) of
                {ok, {Mod, Fun}} ->
                    case Args of
                        [] -> hd(Mod:Fun([StateInstance]));
                        _  -> lists:last(Mod:Fun(Args ++ [StateInstance]))
                    end;
                error ->
                    execute_actor_word(to_string_key(WordName), Args, TypeName, StateInstance)
            end,
            actor_loop(TypeName, NewState, Cache);
        {cast, WordName, Args} when is_list(WordName) ->
            %% Legacy string format — convert to atom and re-dispatch
            self() ! {cast, to_atom_key(WordName), Args},
            actor_loop(TypeName, StateInstance, Cache);
        {call, WordName, Args, From, Ref} when is_atom(WordName) ->
            {ReplyValues, NewState} = case maps:find(WordName, Cache) of
                {ok, {Mod, Fun}} ->
                    ResultStack = case Args of
                        [] -> Mod:Fun([StateInstance]);
                        _  -> Mod:Fun(Args ++ [StateInstance])
                    end,
                    separate_reply(TypeName, ResultStack);
                error ->
                    execute_actor_call(to_string_key(WordName), Args, TypeName, StateInstance)
            end,
            From ! {reply, Ref, ReplyValues},
            actor_loop(TypeName, NewState, Cache);
        {call, WordName, Args, From, Ref} when is_list(WordName) ->
            %% Legacy string format — convert to atom and re-dispatch
            self() ! {call, to_atom_key(WordName), Args, From, Ref},
            actor_loop(TypeName, StateInstance, Cache)
    end.

%% Execute a word on the actor's stack (state instance + pushed args).
%% Returns the updated state instance.
execute_actor_word(WordName, Args, TypeName, StateInstance) ->
    %% Build stack: args on top, state below (Forth convention: rightmost sig = TOS)
    Stack = Args ++ [StateInstance],
    Cont = #continuation{data_stack = Stack},
    Token = #token{value = WordName},
    ResultCont = af_interpreter:interpret_token(Token, Cont),
    ResultStack = ResultCont#continuation.data_stack,
    %% Find the state instance by type name (more robust than lists:last)
    {_ReplyValues, NewState} = separate_reply(TypeName, ResultStack),
    case NewState of
        undefined -> StateInstance;  %% fallback: keep original state
        _ -> NewState
    end.

%% Execute a word that returns values (call semantics).
%% Returns {ReplyValues, NewStateInstance}.
execute_actor_call(WordName, Args, TypeName, StateInstance) ->
    %% Build stack: args on top, state below (Forth convention: rightmost sig = TOS)
    Stack = Args ++ [StateInstance],
    Cont = #continuation{data_stack = Stack},
    Token = #token{value = WordName},
    ResultCont = af_interpreter:interpret_token(Token, Cont),
    ResultStack = ResultCont#continuation.data_stack,
    %% Separate reply values from state.
    %% After execution, state type should be on the stack.
    %% Everything above it is reply data.
    separate_reply(TypeName, ResultStack).

%% Split the stack into reply values (above state) and state instance.
separate_reply(TypeName, Stack) ->
    separate_reply(TypeName, Stack, []).

separate_reply(TypeName, [{TypeName, _} = State | _Rest], ReplyAcc) ->
    {lists:reverse(ReplyAcc), State};
separate_reply(TypeName, [Item | Rest], ReplyAcc) ->
    separate_reply(TypeName, Rest, [Item | ReplyAcc]);
separate_reply(_TypeName, [], ReplyAcc) ->
    %% No state found — shouldn't happen with well-behaved words
    {lists:reverse(ReplyAcc), undefined}.

%%% --- Compiled actor loop generation ---

%% compile_actor_loop/1: Generate a specialized BEAM module for a type's actor loop.
%% The generated module pattern-matches word names directly in receive clauses,
%% eliminating the maps:find lookup from the generic actor_loop.
%% Returns {ok, ModuleName} | error.
compile_actor_loop(TypeName) ->
    Cache = af_actor_worker:build_native_cache(TypeName),
    case maps:size(Cache) of
        0 -> error;
        _ ->
            ModName = actor_loop_module_name(TypeName),
            Forms = generate_loop_forms(ModName, TypeName, Cache),
            case compile:forms(Forms, [return_errors]) of
                {ok, ModName, Binary} ->
                    code:load_binary(ModName, atom_to_list(ModName) ++ ".beam", Binary),
                    {ok, ModName};
                {ok, ModName, Binary, _Warnings} ->
                    code:load_binary(ModName, atom_to_list(ModName) ++ ".beam", Binary),
                    {ok, ModName};
                _Error ->
                    error
            end
    end.

generate_loop_forms(ModName, TypeName, Cache) ->
    L = 1,
    ModAttr = {attribute, L, module, ModName},
    ExportAttr = {attribute, L, export, [{loop, 1}]},

    %% Build cast and call clauses for each compiled word
    CastClauses = maps:fold(fun(WordName, {Mod, Fun}, Acc) ->
        [make_cast_clause(L, WordName, Mod, Fun) | Acc]
    end, [], Cache),

    CallClauses = maps:fold(fun(WordName, {Mod, Fun}, Acc) ->
        [make_call_clause(L, WordName, Mod, Fun, TypeName) | Acc]
    end, [], Cache),

    %% Stop clauses (atom and legacy string)
    StopClause = {clause, L,
        [{tuple, L, [{atom, L, cast}, {atom, L, stop}, {var, L, '_'}]}],
        [], [{atom, L, ok}]},
    StopStringClause = {clause, L,
        [{tuple, L, [{atom, L, cast}, {string, L, "stop"}, {var, L, '_'}]}],
        [], [{atom, L, ok}]},

    %% Fallback cast clause (interpreter dispatch)
    FallbackCastClause = {clause, L,
        [{tuple, L, [{atom, L, cast}, {var, L, 'WordName'}, {var, L, 'Args'}]}],
        [],
        [{match, L, {var, L, 'NewState'},
            {call, L,
                {remote, L, {atom, L, af_type_actor}, {atom, L, execute_actor_word}},
                [{call, L, {remote, L, {atom, L, af_type_actor}, {atom, L, to_string_key}},
                    [{var, L, 'WordName'}]},
                 {var, L, 'Args'}, {atom, L, TypeName}, {var, L, 'StateInstance'}]}},
         {call, L, {atom, L, loop}, [{var, L, 'NewState'}]}]},

    %% Fallback call clause
    FallbackCallClause = {clause, L,
        [{tuple, L, [{atom, L, call}, {var, L, 'WordName'}, {var, L, 'Args'},
                     {var, L, 'From'}, {var, L, 'Ref'}]}],
        [],
        [{match, L,
            {tuple, L, [{var, L, 'ReplyValues'}, {var, L, 'NewState'}]},
            {call, L,
                {remote, L, {atom, L, af_type_actor}, {atom, L, execute_actor_call}},
                [{call, L, {remote, L, {atom, L, af_type_actor}, {atom, L, to_string_key}},
                    [{var, L, 'WordName'}]},
                 {var, L, 'Args'}, {atom, L, TypeName}, {var, L, 'StateInstance'}]}},
         {op, L, '!', {var, L, 'From'},
            {tuple, L, [{atom, L, reply}, {var, L, 'Ref'}, {var, L, 'ReplyValues'}]}},
         {call, L, {atom, L, loop}, [{var, L, 'NewState'}]}]},

    AllClauses = [StopClause, StopStringClause | CastClauses] ++ CallClauses ++
                 [FallbackCastClause, FallbackCallClause],

    LoopFun = {function, L, loop, 1,
        [{clause, L, [{var, L, 'StateInstance'}], [],
            [{'receive', L, AllClauses}]}]},

    [ModAttr, ExportAttr, LoopFun].

%% Cast clause: {cast, WordAtom, Args} -> dispatch and loop
make_cast_clause(L, WordAtom, Mod, Fun) ->
    {clause, L,
        [{tuple, L, [{atom, L, cast}, {atom, L, WordAtom}, {var, L, 'Args'}]}],
        [],
        [{match, L, {var, L, 'NewState'},
            {'case', L, {var, L, 'Args'},
                [{clause, L, [{nil, L}], [],
                    [{call, L, {atom, L, hd},
                        [{call, L, {remote, L, {atom, L, Mod}, {atom, L, Fun}},
                            [{cons, L, {var, L, 'StateInstance'}, {nil, L}}]}]}]},
                 {clause, L, [{var, L, '_'}], [],
                    [{call, L, {remote, L, {atom, L, lists}, {atom, L, last}},
                        [{call, L, {remote, L, {atom, L, Mod}, {atom, L, Fun}},
                            [{op, L, '++', {var, L, 'Args'},
                                {cons, L, {var, L, 'StateInstance'}, {nil, L}}}]}]}]}]}},
         {call, L, {atom, L, loop}, [{var, L, 'NewState'}]}]}.

%% Call clause: {call, WordAtom, Args, From, Ref} -> dispatch, reply, loop
make_call_clause(L, WordAtom, Mod, Fun, TypeName) ->
    {clause, L,
        [{tuple, L, [{atom, L, call}, {atom, L, WordAtom}, {var, L, 'Args'},
                     {var, L, 'From'}, {var, L, 'Ref'}]}],
        [],
        [{match, L, {var, L, 'ResultStack'},
            {'case', L, {var, L, 'Args'},
                [{clause, L, [{nil, L}], [],
                    [{call, L, {remote, L, {atom, L, Mod}, {atom, L, Fun}},
                        [{cons, L, {var, L, 'StateInstance'}, {nil, L}}]}]},
                 {clause, L, [{var, L, '_'}], [],
                    [{call, L, {remote, L, {atom, L, Mod}, {atom, L, Fun}},
                        [{op, L, '++', {var, L, 'Args'},
                            {cons, L, {var, L, 'StateInstance'}, {nil, L}}}]}]}]}},
         {match, L,
            {tuple, L, [{var, L, 'ReplyValues'}, {var, L, 'NewState'}]},
            {call, L, {remote, L, {atom, L, af_type_actor}, {atom, L, separate_reply}},
                [{atom, L, TypeName}, {var, L, 'ResultStack'}]}},
         {op, L, '!', {var, L, 'From'},
            {tuple, L, [{atom, L, reply}, {var, L, 'Ref'}, {var, L, 'ReplyValues'}]}},
         {call, L, {atom, L, loop}, [{var, L, 'NewState'}]}]}.

%%% --- Milestone 7.3: Typed Actor Messages ---

%% Validate that args match expected types before sending to actor.
validate_actor_args(_WordName, [], []) -> ok;
validate_actor_args(WordName, [ExpType | ERest], [{ActType, _} | ARest]) ->
    case ExpType =:= 'Any' orelse ExpType =:= ActType of
        true -> validate_actor_args(WordName, ERest, ARest);
        false ->
            error({actor_type_error, WordName,
                {expected, ExpType, got, ActType}})
    end;
validate_actor_args(_WordName, _, _) -> ok.

%%% --- spawn: run a word in a new process ---

op_spawn(Cont) ->
    [{'Atom', WordName} | Rest] = Cont#continuation.data_stack,
    Pid = erlang:spawn_link(fun() ->
        SpawnCont = #continuation{},
        Token = #token{value = WordName},
        af_interpreter:interpret_token(Token, SpawnCont)
    end),
    ActorVal = #{pid => Pid, type_name => undefined, vocab => #{}},
    Cont#continuation{data_stack = [{'Actor', ActorVal} | Rest]}.

%%% --- send / !: send a value to an actor's mailbox ---

op_send(Cont) ->
    [{'Actor', #{pid := Pid}}, Value | Rest] = Cont#continuation.data_stack,
    Pid ! {af_msg, Value},
    Cont#continuation{data_stack = Rest}.

%%% --- receive: blocking receive from mailbox ---

op_receive(Cont) ->
    receive
        {af_msg, Value} ->
            Cont#continuation{data_stack = [Value | Cont#continuation.data_stack]}
    end.

%%% --- receive-timeout: receive with timeout (ms) ---

op_receive_timeout(Cont) ->
    [{'Int', Timeout} | Rest] = Cont#continuation.data_stack,
    receive
        {af_msg, Value} ->
            Cont#continuation{data_stack = [{'Bool', true}, Value | Rest]}
    after Timeout ->
        Cont#continuation{data_stack = [{'Bool', false}, {'Atom', "timeout"} | Rest]}
    end.

%%% --- Message type: tagged envelope ---

op_msg(Cont) ->
    [{'String', Tag}, Data | Rest] = Cont#continuation.data_stack,
    MsgVal = #{tag => Tag, data => Data},
    Cont#continuation{data_stack = [{'Message', MsgVal} | Rest]}.

op_msg_tag(Cont) ->
    [{'Message', #{tag := Tag}} = Msg | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'String', Tag}, Msg | Rest]}.

op_msg_data(Cont) ->
    [{'Message', #{data := Data}} = Msg | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [Data, Msg | Rest]}.

%%% --- receive-match: selective receive by message tag ---

op_receive_match(Cont) ->
    [{'String', Tag} | Rest] = Cont#continuation.data_stack,
    receive
        {af_msg, {'Message', #{tag := Tag, data := Data}}} ->
            Cont#continuation{data_stack = [Data | Rest]}
    end.

%%% --- receive-match-timeout: selective receive with timeout ---

op_receive_match_timeout(Cont) ->
    [{'Int', Timeout}, {'String', Tag} | Rest] = Cont#continuation.data_stack,
    receive
        {af_msg, {'Message', #{tag := Tag, data := Data}}} ->
            Cont#continuation{data_stack = [{'Bool', true}, Data | Rest]}
    after Timeout ->
        Cont#continuation{data_stack = [{'Bool', false}, {'Atom', "timeout"} | Rest]}
    end.

%%% --- Direct send helpers for compiled word code ---

%% Send a cast message to an actor. Used by BEAM-compiled words
%% that contain << word >> blocks.
%% ActorInfo is the map from {Actor, ActorInfo}.
%% WordName can be atom (from compiled code) or string (legacy).
send_cast(ActorInfo, WordName, Args) when is_list(WordName) ->
    send_cast(ActorInfo, to_atom_key(WordName), Args);
send_cast(ActorInfo, WordName, Args) when is_atom(WordName) ->
    Pid = maps:get(pid, ActorInfo),
    case maps:get(supervised, ActorInfo, false) of
        true -> gen_server:cast(Pid, {cast, WordName, Args});
        false -> Pid ! {cast, WordName, Args}
    end,
    ok.

%% Send a call message to an actor and wait for reply.
%% Returns the list of reply values.
send_call(ActorInfo, WordName, Args) when is_list(WordName) ->
    send_call(ActorInfo, to_atom_key(WordName), Args);
send_call(ActorInfo, WordName, Args) when is_atom(WordName) ->
    Pid = maps:get(pid, ActorInfo),
    case maps:get(supervised, ActorInfo, false) of
        true ->
            {ok, ReplyValues} = gen_server:call(Pid, {call, WordName, Args}, 5000),
            ReplyValues;
        false ->
            Ref = make_ref(),
            Pid ! {call, WordName, Args, self(), Ref},
            receive
                {reply, Ref, ReplyValues} -> ReplyValues
            after 5000 ->
                error({actor_call_timeout, WordName})
            end
    end.

%% Key conversion helpers
to_atom_key(Key) when is_atom(Key) -> Key;
to_atom_key(Key) when is_list(Key) -> list_to_atom(Key).

to_string_key(Key) when is_list(Key) -> Key;
to_string_key(Key) when is_atom(Key) -> atom_to_list(Key).
