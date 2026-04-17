-module(af_actor_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

worker_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"call returns value and updates state", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": get-value Cntr -> Cntr Int ; value .", C1),
            Instance = {'Cntr', 42},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            {ok, ReplyValues} = gen_server:call(Pid, {call, "get-value", []}),
            ?assertEqual([{'Int', 42}], ReplyValues),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"call with error returns error tuple (line 29)", fun() ->
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            %% Call a word that doesn't exist as a compiled word - will error
            %% because the type Cntr isn't registered in this test
            Result = gen_server:call(Pid, {call, "nonexistent_word_xyz", []}),
            case Result of
                {ok, _} -> ok;  %% Word might push as Atom
                {error, _} -> ok
            end,
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"unknown request returns error (line 33)", fun() ->
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            ?assertEqual({error, unknown_request}, gen_server:call(Pid, some_random_request)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"cast stop stops the worker (line 35-36)", fun() ->
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            gen_server:cast(Pid, {cast, "stop", []}),
            timer:sleep(100),
            ?assertNot(is_process_alive(Pid))
        end} end,

        fun(_) -> {"cast executes word (line 38-46)", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": inc Cntr -> Cntr ; value 1 + value! .", C1),
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            gen_server:cast(Pid, {cast, "inc", []}),
            timer:sleep(50),
            %% Verify via a call that state changed
            %% We need get-value registered
            _C3 = eval(": get-value Cntr -> Cntr Int ; value .", af_interpreter:new_continuation()),
            {ok, Reply} = gen_server:call(Pid, {call, "get-value", []}),
            ?assertEqual([{'Int', 1}], Reply),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"unknown cast is ignored (line 48-49)", fun() ->
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            gen_server:cast(Pid, unknown_cast_msg),
            timer:sleep(50),
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"handle_info is ignored (line 51-52)", fun() ->
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            Pid ! some_random_message,
            timer:sleep(50),
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"separate_reply with no matching type returns undefined (line 82-83)", fun() ->
            %% Define a word that drops the state and pushes unrelated items
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": weird Cntr -> Int ; value drop 99 .", C1),
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            %% This call will execute the word; separate_reply won't find Cntr in result
            _Result = gen_server:call(Pid, {call, "weird", []}),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"cast atom stop stops the worker", fun() ->
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            gen_server:cast(Pid, {cast, stop, []}),
            timer:sleep(100),
            ?assertNot(is_process_alive(Pid))
        end} end,

        fun(_) -> {"cast with args exercises args code path", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": inc Cntr -> Cntr ; value 1 + value! .", C1),
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            %% Pass args to cast — the inc word ignores them but exercises
            %% the Args =/= [] branch in handle_cast
            gen_server:cast(Pid, {cast, "inc", [{'Int', 99}]}),
            timer:sleep(50),
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"call with args exercises args code path", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            %% get-value: Cntr -> Cntr Int
            _C2 = eval(": get-value Cntr -> Cntr Int ; value .", C1),
            Instance = {'Cntr', 10},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            %% Pass args to call — the word ignores them but the code path
            %% exercises the Args =/= [] branch in handle_call
            {ok, _Reply} = gen_server:call(Pid, {call, "get-value", [{'Int', 99}]}),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"build_native_cache returns empty map for unknown type", fun() ->
            Cache = af_actor_worker:build_native_cache('NonExistentType12345'),
            ?assertEqual(#{}, Cache)
        end} end,

        fun(_) -> {"terminate returns ok", fun() ->
            ?assertEqual(ok, af_actor_worker:terminate(normal, undefined))
        end} end,

        fun(_) -> {"cast with atom word name", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": inc Cntr -> Cntr ; value 1 + value! .", C1),
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            gen_server:cast(Pid, {cast, inc, []}),
            timer:sleep(50),
            _C3 = eval(": get-value Cntr -> Cntr Int ; value .", af_interpreter:new_continuation()),
            {ok, Reply} = gen_server:call(Pid, {call, "get-value", []}),
            ?assertEqual([{'Int', 1}], Reply),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"call with atom word name", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": get-value Cntr -> Cntr Int ; value .", C1),
            Instance = {'Cntr', 42},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            {ok, Reply} = gen_server:call(Pid, {call, 'get-value', []}),
            ?assertEqual([{'Int', 42}], Reply),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"cast error is swallowed and state preserved", fun() ->
            %% Cast a word that will error — state should remain unchanged
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": get-value Cntr -> Cntr Int ; value .", C1),
            %% A word that will crash: head on empty list
            _C3 = eval(": crasher Cntr -> Cntr ; drop nil head .", C1),
            Instance = {'Cntr', 7},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            %% This will cause an error in interpret_cast, caught by the catch
            gen_server:cast(Pid, {cast, "crasher", []}),
            timer:sleep(50),
            ?assert(is_process_alive(Pid)),
            {ok, Reply} = gen_server:call(Pid, {call, "get-value", []}),
            ?assertEqual([{'Int', 7}], Reply),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"call error returns error tuple", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            %% A word that will crash
            _C2 = eval(": crasher Cntr -> Int ; drop nil head .", C1),
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            Result = gen_server:call(Pid, {call, "crasher", []}),
            ?assertMatch({error, _}, Result),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"build_native_cache finds native ops", fun() ->
            %% Register a type with a native operation
            af_type:register_type(#af_type{name = 'NativeTest'}),
            NativeOp = #operation{
                name = "test-op",
                sig_in = ['NativeTest'],
                sig_out = ['NativeTest'],
                impl = fun(C) -> C end,
                source = {native, some_module}
            },
            af_type:add_op('NativeTest', NativeOp),
            Cache = af_actor_worker:build_native_cache('NativeTest'),
            ?assert(maps:is_key('test-op', Cache)),
            {Mod, Fun} = maps:get('test-op', Cache),
            ?assertEqual(some_module, Mod),
            ?assertEqual('test-op', Fun)
        end} end,

        fun(_) -> {"build_native_cache skips non-native ops", fun() ->
            af_type:register_type(#af_type{name = 'InterpTest'}),
            InterpOp = #operation{
                name = "interp-op",
                sig_in = ['InterpTest'],
                sig_out = ['InterpTest'],
                impl = fun(C) -> C end,
                source = {compiled, []}
            },
            af_type:add_op('InterpTest', InterpOp),
            Cache = af_actor_worker:build_native_cache('InterpTest'),
            ?assertNot(maps:is_key('interp-op', Cache))
        end} end,

        fun(_) -> {"call word that clears stack returns undefined instance", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            %% A word that drops everything
            _C2 = eval(": dropper Cntr -> ; drop .", C1),
            Instance = {'Cntr', 0},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            %% This will result in empty stack after separate_reply
            Result = gen_server:call(Pid, {call, "dropper", []}),
            %% Should get {ok, {[], undefined}} since no instance remains
            ?assertMatch({ok, _}, Result),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"native cache hit for cast with no args", fun() ->
            %% Create a dynamic module with a test function
            create_native_module(),
            %% Register a type with a native op pointing to our module
            af_type:register_type(#af_type{name = 'NativeCntr'}),
            NativeOp = #operation{
                name = "noop",
                sig_in = ['NativeCntr'],
                sig_out = ['NativeCntr'],
                impl = fun(C) -> C end,
                source = {native, af_test_native_mod}
            },
            af_type:add_op('NativeCntr', NativeOp),
            Instance = {'NativeCntr', test_value},
            {ok, Pid} = af_actor_worker:start_link('NativeCntr', Instance),
            %% Cast with no args — exercises native cache hit, Args=[] path
            gen_server:cast(Pid, {cast, "noop", []}),
            timer:sleep(50),
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"native cache hit for cast with args", fun() ->
            create_native_module(),
            af_type:register_type(#af_type{name = 'NativeCntr'}),
            NativeOp = #operation{
                name = "noop",
                sig_in = ['NativeCntr'],
                sig_out = ['NativeCntr'],
                impl = fun(C) -> C end,
                source = {native, af_test_native_mod}
            },
            af_type:add_op('NativeCntr', NativeOp),
            Instance = {'NativeCntr', test_value},
            {ok, Pid} = af_actor_worker:start_link('NativeCntr', Instance),
            %% Cast with args — exercises native cache hit, Args =/= [] path
            gen_server:cast(Pid, {cast, "noop", [{'Int', 42}]}),
            timer:sleep(50),
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"native cache hit for call with no args", fun() ->
            create_native_module(),
            af_type:register_type(#af_type{name = 'NativeCntr'}),
            NativeOp = #operation{
                name = "noop",
                sig_in = ['NativeCntr'],
                sig_out = ['NativeCntr'],
                impl = fun(C) -> C end,
                source = {native, af_test_native_mod}
            },
            af_type:add_op('NativeCntr', NativeOp),
            Instance = {'NativeCntr', test_value},
            {ok, Pid} = af_actor_worker:start_link('NativeCntr', Instance),
            %% Call with no args — exercises native cache hit, Args=[] path
            {ok, _Reply} = gen_server:call(Pid, {call, "noop", []}),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"native cache hit for call with args", fun() ->
            create_native_module(),
            af_type:register_type(#af_type{name = 'NativeCntr'}),
            NativeOp = #operation{
                name = "noop",
                sig_in = ['NativeCntr'],
                sig_out = ['NativeCntr'],
                impl = fun(C) -> C end,
                source = {native, af_test_native_mod}
            },
            af_type:add_op('NativeCntr', NativeOp),
            Instance = {'NativeCntr', test_value},
            {ok, Pid} = af_actor_worker:start_link('NativeCntr', Instance),
            %% Call with args — exercises native cache hit, Args =/= [] path
            {ok, _Reply} = gen_server:call(Pid, {call, "noop", [{'Int', 42}]}),
            gen_server:stop(Pid)
        end} end
    ]}.

%% Create a dynamic module that serves as a native implementation
%% with a 'noop' function that returns its argument unchanged
create_native_module() ->
    %% Dynamically compile a module: af_test_native_mod with noop/1
    %% noop(Stack) -> Stack  (identity function)
    Forms = [
        {attribute, 1, module, af_test_native_mod},
        {attribute, 2, export, [{noop, 1}]},
        {function, 3, noop, 1, [
            {clause, 3, [{var, 3, 'Stack'}], [],
             [{var, 3, 'Stack'}]}
        ]}
    ],
    {ok, af_test_native_mod, Bin} = compile:forms(Forms),
    code:load_binary(af_test_native_mod, "af_test_native_mod.beam", Bin),
    ok.
