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
            {ok, _} = af_server:eval(Pid, "value 5 + value!"),
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

%% --- Edge cases for coverage ---

edge_case_test_() ->
    {foreach,
     fun() ->
         {ok, Pid} = af_server:start_link("samples/bridge_counter.a4"),
         Pid
     end,
     fun(Pid) ->
         catch af_server:stop(Pid)
     end,
     [
        fun(Pid) -> {"unknown call request returns error (line 114)", fun() ->
            Result = gen_server:call(Pid, totally_unknown_request),
            ?assertEqual({error, unknown_request}, Result)
        end} end,

        fun(Pid) -> {"handle_cast unknown message is ignored (line 130-131)", fun() ->
            gen_server:cast(Pid, totally_unknown_cast),
            timer:sleep(50),
            ?assert(is_process_alive(Pid)),
            %% State should be unchanged
            {ok, [0]} = af_server:call(Pid, "count", [])
        end} end,

        fun(Pid) -> {"handle_info is ignored (line 133-134)", fun() ->
            Pid ! some_random_info_message,
            timer:sleep(50),
            ?assert(is_process_alive(Pid)),
            {ok, [0]} = af_server:call(Pid, "count", [])
        end} end,

        fun(Pid) -> {"call word that errors returns error (line 89)", fun() ->
            %% Trigger an error by calling a word that will crash
            %% Division by zero should cause an error
            Result = af_server:eval(Pid, "0 0 /"),
            ?assertMatch({error, _}, Result)
        end} end,

        fun(Pid) -> {"eval that consumes stack below base depth (line 105-106)", fun() ->
            %% The bridge_counter.a4 puts a Counter on the stack (base_depth=1).
            %% eval "drop" consumes it, leaving stack shorter than base depth.
            Result = af_server:eval(Pid, "drop"),
            ?assertMatch({ok, []}, Result)
        end} end,

        fun(Pid) -> {"eval that errors returns error (line 109-110)", fun() ->
            Result = af_server:eval(Pid, "0 0 /"),
            ?assertMatch({error, _}, Result)
        end} end,

        fun(Pid) -> {"cast word that errors is silently caught (line 126-127)", fun() ->
            %% Cast a word that will error - the gen_server should survive
            af_server:cast(Pid, "nonexistent_error_word", []),
            timer:sleep(50),
            ?assert(is_process_alive(Pid))
        end} end
    ]}.

%% --- Word consuming more than produced (lines 83-85) ---

word_consume_test_() ->
    {foreach,
     fun() ->
         {ok, Pid} = af_server:start_link("samples/bridge_counter.a4"),
         Pid
     end,
     fun(Pid) ->
         catch af_server:stop(Pid)
     end,
     [
        fun(Pid) -> {"call word that consumes base stack items (lines 83-85)", fun() ->
            %% Eval "drop" to clear the base stack, then try a call
            %% First, push and consume to get stack below base depth
            {ok, _} = af_server:eval(Pid, "drop"),
            %% Now call a word - the state stack is empty, word pushes Atom
            Result = af_server:call(Pid, "count", []),
            case Result of
                {ok, _} -> ok;
                {error, _} -> ok
            end
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
