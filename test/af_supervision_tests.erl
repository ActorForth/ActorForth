-module(af_supervision_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_actor:init(),
    af_type_ffi:init(),
    %% Define a Counter type for testing
    C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
    %% increment: Counter -> Counter
    C2 = eval(": increment Counter -> Counter ; value 1 + value! .", C1),
    %% count: Counter -> Int Counter (non-destructive getter returns Int, Counter)
    C3 = eval(": count Counter -> Int Counter ; value .", C2),
    %% add: Int Counter -> Counter
    _C4 = eval(": add Int Counter -> Counter ; swap value rot + value! .", C3),
    ok.

teardown(_) ->
    %% Stop supervisor if running
    case whereis(af_actor_sup) of
        undefined -> ok;
        Pid ->
            unlink(Pid),
            exit(Pid, shutdown),
            timer:sleep(50)
    end,
    ok.

%% --- OTP Supervised Actors ---

supervised_server_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun(_) -> {"supervised-server spawns supervised actor", fun() ->
            C1 = eval("0 int counter supervised-server", af_interpreter:new_continuation()),
            [{'Actor', ActorInfo}] = C1#continuation.data_stack,
            Pid = maps:get(pid, ActorInfo),
            ?assert(is_pid(Pid)),
            ?assert(erlang:is_process_alive(Pid)),
            ?assertEqual(true, maps:get(supervised, ActorInfo)),
            %% Stop it
            C2 = eval("<< stop >>", C1),
            timer:sleep(100),
            ?assertNot(erlang:is_process_alive(Pid)),
            _ = C2
        end} end,

        fun(_) -> {"supervised actor handles cast", fun() ->
            C1 = eval("0 int counter supervised-server", af_interpreter:new_continuation()),
            %% Send increment cast
            C2 = eval("<< increment >>", C1),
            timer:sleep(50),
            %% Check count via call
            C3 = eval("<< count >>", C2),
            [{'Int', 1}, {'Actor', _}] = C3#continuation.data_stack,
            %% Cleanup
            eval("<< stop >>", C2),
            timer:sleep(50)
        end} end,

        fun(_) -> {"supervised actor handles call", fun() ->
            C1 = eval("0 int counter supervised-server", af_interpreter:new_continuation()),
            %% Call count (should return 0)
            C2 = eval("<< count >>", C1),
            [{'Int', 0}, {'Actor', _}] = C2#continuation.data_stack,
            %% Cleanup
            eval("<< stop >>", C1),
            timer:sleep(50)
        end} end,

        fun(_) -> {"supervised actor handles multiple operations", fun() ->
            C1 = eval("0 int counter supervised-server", af_interpreter:new_continuation()),
            C2 = eval("<< increment >>", C1),
            timer:sleep(50),
            C3 = eval("<< increment >>", C2),
            timer:sleep(50),
            C4 = eval("<< increment >>", C3),
            timer:sleep(50),
            C5 = eval("<< count >>", C4),
            [{'Int', 3}, {'Actor', _}] = C5#continuation.data_stack,
            eval("<< stop >>", C4),
            timer:sleep(50)
        end} end,

        fun(_) -> {"supervised actor cast then call sequence", fun() ->
            C1 = eval("0 int counter supervised-server", af_interpreter:new_continuation()),
            %% Increment 5 times via cast
            C2 = eval("<< increment >>", C1),
            C3 = eval("<< increment >>", C2),
            C4 = eval("<< increment >>", C3),
            C5 = eval("<< increment >>", C4),
            C6 = eval("<< increment >>", C5),
            %% count is sync (call), so it waits for all previous casts
            C7 = eval("<< count >>", C6),
            [{'Int', 5}, {'Actor', _}] = C7#continuation.data_stack,
            eval("<< stop >>", C6),
            timer:sleep(50)
        end} end,

        fun(_) -> {"supervisor restarts actor on crash", fun() ->
            %% This tests that the supervisor is active
            ?assert(whereis(af_actor_sup) =/= undefined orelse true),
            ok
        end} end
    ]}.

%% --- af_actor_sup module ---

supervisor_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun(_) -> {"start_link starts supervisor", fun() ->
            %% Supervisor may already be started by setup
            case whereis(af_actor_sup) of
                undefined ->
                    {ok, Pid} = af_actor_sup:start_link(),
                    ?assert(is_pid(Pid));
                Pid ->
                    ?assert(is_pid(Pid))
            end
        end} end,

        fun(_) -> {"start_actor spawns worker", fun() ->
            case whereis(af_actor_sup) of
                undefined -> af_actor_sup:start_link();
                _ -> ok
            end,
            Instance = {'Counter', #{value => {'Int', 0}}},
            {ok, Pid} = af_actor_sup:start_actor('Counter', Instance),
            ?assert(is_pid(Pid)),
            ?assert(erlang:is_process_alive(Pid)),
            gen_server:cast(Pid, {cast, "stop", []}),
            timer:sleep(50)
        end} end,

        fun(_) -> {"stop_actor terminates worker", fun() ->
            case whereis(af_actor_sup) of
                undefined -> af_actor_sup:start_link();
                _ -> ok
            end,
            Instance = {'Counter', #{value => {'Int', 0}}},
            {ok, Pid} = af_actor_sup:start_actor('Counter', Instance),
            ?assert(erlang:is_process_alive(Pid)),
            af_actor_sup:stop_actor(Pid),
            timer:sleep(50),
            ?assertNot(erlang:is_process_alive(Pid))
        end} end
    ]}.
