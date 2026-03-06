-module(af_server_tests).

-include_lib("eunit/include/eunit.hrl").

%% --- Basic lifecycle ---

lifecycle_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
        fun(_) -> {"start and stop", fun() ->
            {ok, Pid} = af_server:start_link("samples/bridge_counter.a4"),
            ?assert(is_process_alive(Pid)),
            af_server:stop(Pid),
            timer:sleep(50),
            ?assertNot(is_process_alive(Pid))
        end} end,
        fun(_) -> {"start with name", fun() ->
            {ok, Pid} = af_server:start_link("samples/bridge_counter.a4",
                                              [{name, test_counter}]),
            ?assertEqual(Pid, whereis(test_counter)),
            af_server:stop(test_counter)
        end} end
    ]}.

%% --- Call/Cast operations ---

operations_test_() ->
    {foreach,
     fun() ->
         {ok, Pid} = af_server:start_link("samples/bridge_counter.a4"),
         Pid
     end,
     fun(Pid) ->
         catch af_server:stop(Pid)
     end,
     [
        fun(Pid) -> {"call count returns 0 initially", fun() ->
            {ok, [0]} = af_server:call(Pid, "count", []),
            ok
        end} end,
        fun(Pid) -> {"cast increment then call count", fun() ->
            ok = af_server:cast(Pid, "increment", []),
            %% Cast is async, give it a moment
            timer:sleep(50),
            {ok, [1]} = af_server:call(Pid, "count", []),
            ok
        end} end,
        fun(Pid) -> {"multiple increments", fun() ->
            ok = af_server:cast(Pid, "increment", []),
            ok = af_server:cast(Pid, "increment", []),
            ok = af_server:cast(Pid, "increment", []),
            timer:sleep(50),
            {ok, [3]} = af_server:call(Pid, "count", []),
            ok
        end} end,
        fun(Pid) -> {"call with argument: add", fun() ->
            {ok, []} = af_server:call(Pid, "add", [10]),
            {ok, [10]} = af_server:call(Pid, "count", []),
            ok
        end} end,
        fun(Pid) -> {"decrement", fun() ->
            af_server:cast(Pid, "increment", []),
            af_server:cast(Pid, "increment", []),
            timer:sleep(50),
            af_server:cast(Pid, "decrement", []),
            timer:sleep(50),
            {ok, [1]} = af_server:call(Pid, "count", []),
            ok
        end} end,
        fun(Pid) -> {"reset", fun() ->
            {ok, []} = af_server:call(Pid, "add", [42]),
            {ok, [42]} = af_server:call(Pid, "count", []),
            {ok, []} = af_server:call(Pid, "reset", []),
            {ok, [0]} = af_server:call(Pid, "count", []),
            ok
        end} end
    ]}.

%% --- Eval ---

eval_test_() ->
    {foreach,
     fun() ->
         {ok, Pid} = af_server:start_link("samples/bridge_counter.a4"),
         Pid
     end,
     fun(Pid) ->
         catch af_server:stop(Pid)
     end,
     [
        fun(Pid) -> {"eval raw line", fun() ->
            {ok, _} = af_server:eval(Pid, "dup value 5 + value!"),
            {ok, [5]} = af_server:call(Pid, "count", []),
            ok
        end} end
    ]}.

%% --- Error handling ---

error_test_() ->
    {foreach,
     fun() ->
         {ok, Pid} = af_server:start_link("samples/bridge_counter.a4"),
         Pid
     end,
     fun(Pid) ->
         catch af_server:stop(Pid)
     end,
     [
        fun(Pid) -> {"unknown word pushes Atom (no syntax errors)", fun() ->
            %% In ActorForth, unknown tokens become Atoms — not errors
            {ok, [nonexistent]} = af_server:call(Pid, "nonexistent", []),
            %% State should be unaffected
            {ok, [0]} = af_server:call(Pid, "count", []),
            ok
        end} end
    ]}.

%% --- Multiple concurrent servers ---

concurrent_test_() ->
    [
        {"two independent servers", fun() ->
            {ok, P1} = af_server:start_link("samples/bridge_counter.a4"),
            {ok, P2} = af_server:start_link("samples/bridge_counter.a4"),
            {ok, []} = af_server:call(P1, "add", [10]),
            {ok, []} = af_server:call(P2, "add", [20]),
            {ok, [10]} = af_server:call(P1, "count", []),
            {ok, [20]} = af_server:call(P2, "count", []),
            af_server:stop(P1),
            af_server:stop(P2)
        end}
    ].
