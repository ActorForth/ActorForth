-module(af_hos_runtime_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

%% Runtime tests for HOS: spawn a system tree, verify messages route
%% along declared parent/child edges, and verify that sibling access
%% is structurally impossible.

setup() ->
    af_repl:init_types(),
    af_type_compiler:clear_pending_checks(),
    af_hos_check:clear_registry().

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

eval_new(Input) ->
    eval(Input, af_interpreter:new_continuation()).

get_log(Pid) ->
    Ref = make_ref(),
    Pid ! {get_log, self(), Ref},
    receive
        {reply, Ref, Log} -> Log
    after 1000 ->
        {error, timeout}
    end.


%% -------------------------------------------------------------------
%% Basic spawn and receive
%% -------------------------------------------------------------------

spawn_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"spawn a single leaf system", fun() ->
            C = eval_new("system Leaf on ping -> ; dup drop end "
                          "\"Leaf\" find-system"),
            [Node | _] = C#continuation.data_stack,
            Pid = af_hos_runtime:spawn_system(Node),
            ?assert(is_pid(Pid)),
            ?assert(is_process_alive(Pid)),
            af_hos_runtime:stop_system(Pid),
            timer:sleep(50)
        end} end,

        fun(_) -> {"event arrives at the target system", fun() ->
            C = eval_new("system Worker on step -> ; dup drop end "
                          "\"Worker\" find-system"),
            [Node | _] = C#continuation.data_stack,
            Pid = af_hos_runtime:spawn_system(Node),
            af_hos_runtime:send_event(Pid, step, []),
            timer:sleep(50),
            Log = get_log(Pid),
            ?assertEqual(["step"], Log),
            af_hos_runtime:stop_system(Pid)
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Parent / child routing
%% -------------------------------------------------------------------

routing_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"parent forwards event to its named child", fun() ->
            %% Child: declares a `step` handler.
            C1 = eval_new("system Child parent Parent on step -> ; dup drop end"),
            %% Parent: references Child as a direct child; its
            %% `kick` handler forwards to `Child step`.
            C2 = eval(
                "system Parent "
                "  children Child "
                "  on kick -> ; "
                "    Child step "
                "end "
                "\"Parent\" find-system",
                C1),
            [ParentNode | _] = C2#continuation.data_stack,
            ParentPid = af_hos_runtime:spawn_system(ParentNode),
            af_hos_runtime:send_event(ParentPid, kick, []),
            timer:sleep(100),
            ParentLog = get_log(ParentPid),
            ?assertEqual(["kick"], ParentLog),
            %% Verify the child got the step event that the parent
            %% forwarded. The child's PID is inside the parent's state
            %% map -- we pull it by asking the parent.
            ChildPid = introspect_child_pid(ParentPid, "Child"),
            ChildLog = get_log(ChildPid),
            ?assertEqual(["step"], ChildLog),
            af_hos_runtime:stop_system(ParentPid)
        end} end
    ]}.

introspect_child_pid(ParentPid, ChildName) ->
    Ref = make_ref(),
    ParentPid ! {introspect_child, ChildName, self(), Ref},
    receive
        {reply, Ref, Pid} -> Pid
    after 1000 ->
        erlang:error(introspect_timeout)
    end.


%% -------------------------------------------------------------------
%% Structural: sibling PIDs are not reachable
%% -------------------------------------------------------------------

topology_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"siblings cannot reach each other directly", fun() ->
            %% Construct a BuildingSystem with two children:
            %% Dispatcher and EmergencySource. Each child's process
            %% holds parent_pid but no reference to its sibling.
            C1 = eval_new("system Dispatcher parent BuildingSystem "
                          "  on tick -> ; dup drop end"),
            C2 = eval("system EmergencySource parent BuildingSystem "
                      "  on trigger -> ; dup drop end", C1),
            _C3 = eval(
                "system BuildingSystem "
                "  children Dispatcher EmergencySource "
                "  on noop -> ; dup drop "
                "end "
                "\"BuildingSystem\" find-system",
                C2),
            [Root | _] = _C3#continuation.data_stack,
            RootPid = af_hos_runtime:spawn_system(Root),
            %% Introspect the two child PIDs.
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            EmergPid = introspect_child_pid(RootPid, "EmergencySource"),
            ?assert(is_pid(DispPid)),
            ?assert(is_pid(EmergPid)),
            ?assertNotEqual(DispPid, EmergPid),
            %% Ask EmergencySource for Dispatcher's PID via the same
            %% introspection mechanism. A sibling is NOT in its child
            %% map, so the lookup should report "not found."
            ?assertEqual(not_found,
                         introspect_child_pid_raw(EmergPid, "Dispatcher")),
            af_hos_runtime:stop_system(RootPid)
        end} end
    ]}.

introspect_child_pid_raw(Pid, ChildName) ->
    Ref = make_ref(),
    Pid ! {introspect_child, ChildName, self(), Ref},
    receive
        {reply, Ref, Result} -> Result
    after 1000 ->
        erlang:error(introspect_timeout)
    end.
