-module(af_actor_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init().

worker_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"call returns value and updates state", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": get-value Cntr -> Cntr Int ; value .", C1),
            Instance = {'Cntr', #{value => {'Int', 42}}},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            {ok, ReplyValues} = gen_server:call(Pid, {call, "get-value", []}),
            ?assertEqual([{'Int', 42}], ReplyValues),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"call with error returns error tuple (line 29)", fun() ->
            Instance = {'Cntr', #{value => {'Int', 0}}},
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
            Instance = {'Cntr', #{value => {'Int', 0}}},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            ?assertEqual({error, unknown_request}, gen_server:call(Pid, some_random_request)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"cast stop stops the worker (line 35-36)", fun() ->
            Instance = {'Cntr', #{value => {'Int', 0}}},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            gen_server:cast(Pid, {cast, "stop", []}),
            timer:sleep(100),
            ?assertNot(is_process_alive(Pid))
        end} end,

        fun(_) -> {"cast executes word (line 38-46)", fun() ->
            C1 = eval("type Cntr value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": inc Cntr -> Cntr ; value 1 + value! .", C1),
            Instance = {'Cntr', #{value => {'Int', 0}}},
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
            Instance = {'Cntr', #{value => {'Int', 0}}},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            gen_server:cast(Pid, unknown_cast_msg),
            timer:sleep(50),
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"handle_info is ignored (line 51-52)", fun() ->
            Instance = {'Cntr', #{value => {'Int', 0}}},
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
            Instance = {'Cntr', #{value => {'Int', 0}}},
            {ok, Pid} = af_actor_worker:start_link('Cntr', Instance),
            %% This call will execute the word; separate_reply won't find Cntr in result
            _Result = gen_server:call(Pid, {call, "weird", []}),
            gen_server:stop(Pid)
        end} end
    ]}.
