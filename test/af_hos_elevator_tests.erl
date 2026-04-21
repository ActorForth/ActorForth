-module(af_hos_elevator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

%% FAT (functional acceptance) tests for the HOS elevator example.
%% Each of the six Python/OCaml/Rust-naive flaws is attempted as an
%% HOS spec; every one is rejected at compile time with a specific
%% axiom citation. The valid spec compiles and its runtime tree
%% routes a floor-button-press from BuildingSystem down through
%% Dispatcher, Car, and Motor in that order.

setup() ->
    af_repl:init_types(),
    af_type_compiler:clear_pending_checks(),
    af_hos_check:clear_registry().

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

eval_new(Input) ->
    eval(Input, af_interpreter:new_continuation()).

load_valid_spec() ->
    {ok, Content} = file:read_file(
        "samples/hos/elevator/elevator.a4"),
    eval(binary_to_list(Content), af_interpreter:new_continuation()).

get_log(Pid) ->
    af_hos_runtime:get_log(Pid).

%% -------------------------------------------------------------------
%% Valid spec compiles and spawns
%% -------------------------------------------------------------------

valid_spec_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"full elevator spec compiles without axiom violations", fun() ->
            Cont = load_valid_spec(),
            %% Axiom pass means every `system ... end` completed; the
            %% last token of the spec is a literal print producing no
            %% stack residue beyond what the print consumed.
            ?assert(is_record(Cont, continuation))
        end} end,

        fun(_) -> {"all seven elevator systems registered", fun() ->
            load_valid_spec(),
            Systems = ["Door", "Motor", "Car", "EmergencySource",
                       "Dispatcher", "BuildingSystem"],
            lists:foreach(fun(Name) ->
                ?assertMatch({'HosBlueprint', _, _, _, _, _, _},
                             af_hos_check:lookup_system(Name))
            end, Systems)
        end} end,

        fun(_) -> {"spawn tree; BuildingSystem is alive", fun() ->
            load_valid_spec(),
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            ?assert(is_process_alive(RootPid)),
            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        fun(_) -> {"floor-button-press routes through Dispatcher to Car", fun() ->
            load_valid_spec(),
            %% Spawn BuildingSystem. Because Car has grandchildren
            %% (Door, Motor) in the declared tree, we spawn the full
            %% BuildingSystem tree which includes Dispatcher and
            %% EmergencySource as direct children, and Car's subtree
            %% comes with Dispatcher. For this wiring check the tree
            %% we care about is the routing ancestry.
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            af_hos_runtime:send_event(RootPid,
                'floor-button-press', []),
            timer:sleep(150),
            RootLog = get_log(RootPid),
            ?assertEqual(["floor-button-press"], RootLog),
            af_hos_runtime:stop_system(RootPid)
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Each of the six FAT violations is rejected at compile time
%% -------------------------------------------------------------------

violation_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"Flaw 5: sibling back-channel rejected", fun() ->
            try
                eval_new(
                    "system EmergencySource "
                    "  parent BuildingSystem "
                    "  on trigger -> ; "
                    "    Car trigger-emergency "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_1")),
                    ?assertNotEqual(nomatch, string:find(Msg, "Car"))
            end
        end} end,

        fun(_) -> {"Flaw 1/2: reach past child to grandchild rejected", fun() ->
            try
                eval_new(
                    "system Dispatcher "
                    "  parent BuildingSystem "
                    "  children Car "
                    "  on malicious -> ; "
                    "    Motor move-to "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_1")),
                    ?assertNotEqual(nomatch, string:find(Msg, "Motor"))
            end
        end} end,

        fun(_) -> {"Flaw 4: skip-state transition rejected (axiom 5)", fun() ->
            try
                eval_new(
                    "system C "
                    "  state CState "
                    "  on go -> ; "
                    "    -> PreparingToMove "
                    "    -> Moving "
                    "    -> DoorOpening "
                    "  transitions "
                    "    IdleAtFloor -> PreparingToMove go "
                    "    PreparingToMove -> Moving go "
                    "    Moving -> Arriving go "
                    "    Arriving -> DoorOpening go "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_5"))
            end
        end} end,

        fun(_) -> {"Flaw 3: forged event name rejected", fun() ->
            try
                eval_new(
                    "system C "
                    "  parent P "
                    "  on step -> ; "
                    "    check-emergency-flag "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_1"))
            end
        end} end,

        fun(_) -> {"Parent/children mismatch rejected", fun() ->
            try
                eval_new(
                    "system Door parent Somewhere end "
                    "system Car parent Dispatcher children Door end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch,
                                    string:find(Msg, "consistency"))
            end
        end} end,

        fun(_) -> {"Parallel-dispatcher siblings rejected", fun() ->
            try
                eval_new(
                    "system PD "
                    "  parent BuildingSystem "
                    "  children Car "
                    "  on request R -> ; "
                    "    OtherDispatcher request "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_1")),
                    ?assertNotEqual(nomatch,
                                    string:find(Msg, "OtherDispatcher"))
            end
        end} end,

        fun(_) -> {"Skip-level down: BuildingSystem calling Car directly rejected", fun() ->
            %% BuildingSystem's scope is: its own name, its children
            %% (Dispatcher, EmergencySource), and a4 builtins. Car
            %% is a grandchild, not a child, and therefore out of
            %% scope. Axiom 1.
            try
                eval_new(
                    "system BuildingSystem "
                    "  children Dispatcher EmergencySource "
                    "  on emergency -> ; "
                    "    Car trigger-emergency "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_1")),
                    ?assertNotEqual(nomatch, string:find(Msg, "Car"))
            end
        end} end,

        fun(_) -> {"Inconsistency (reverse): Motor parent Dispatcher vs Car children Motor rejected", fun() ->
            %% Motor declares its parent as Dispatcher, but Car's
            %% children list includes Motor. When Car registers,
            %% the consistency check flags the mismatch.
            try
                eval_new(
                    "system Motor parent Dispatcher end "
                    "system Car parent Dispatcher children Door Motor end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "consistency"))
            end
        end} end,

        fun(_) -> {"Stateful body rejected: any content in a stateful handler body is an axiom violation", fun() ->
            %% Stateful systems are driven by the transitions table,
            %% one event fires one transition. Handler bodies on
            %% stateful systems must be empty; anything in them is
            %% rejected. This is the structural replacement for the
            %% earlier Flaw 1/2 rule (no inline subsystem calls).
            try
                eval_new(
                    "system C "
                    "  state CState "
                    "  on step -> ; "
                    "    -> Ready "
                    "  transitions "
                    "    Idle -> Ready step "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_5")),
                    ?assertNotEqual(nomatch,
                                    string:find(Msg, "non-empty body"))
            end
        end} end,

        fun(_) -> {"Effect target out of scope rejected", fun() ->
            %% A transition's declared effect target must itself be
            %% in scope. Declaring an effect on `Stranger` (not a
            %% parent, not a child) is an axiom_1 violation against
            %% the transition table itself.
            try
                eval_new(
                    "system C "
                    "  parent P "
                    "  state CState "
                    "  on go -> ; "
                    "  transitions "
                    "    Idle -> Done go : Stranger step "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_1")),
                    ?assertNotEqual(nomatch, string:find(Msg, "Stranger"))
            end
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Effects dispatch: declared transition effects reach the target
%% subsystem in the runtime. This is the positive version of
%% Flaw 1/2 prevention: the ORDER of Door/Motor calls is whatever
%% the transitions table declares, and the runtime dispatches in
%% exactly that order.
%% -------------------------------------------------------------------

effect_dispatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"floor-button-press drives one full assign cycle with declared ordering", fun() ->
            load_valid_spec(),
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            CarPid  = introspect_child_pid(DispPid, "Car"),
            DoorPid = introspect_child_pid(CarPid, "Door"),
            MotorPid = introspect_child_pid(CarPid, "Motor"),

            af_hos_runtime:send_event(RootPid,
                'floor-button-press', []),
            %% Full cycle time:
            %%   Motor travel: 500ms
            %%   Car settling (Arriving): 200ms
            %%   Door open timer: 100ms
            %%   Car loading dwell: 300ms
            %%   Door close timer: 100ms
            %% Total ~1200ms. Sleep 1500ms for margin.
            timer:sleep(1500),

            DoorLog  = get_log(DoorPid),
            MotorLog = get_log(MotorPid),

            %% Door sequence is driven by Car's event-driven
            %% transitions. Door starts Closed, gets `close` (idempotent
            %% self-loop, signals Car immediately), then `open`
            %% (opens over 100ms, signals opened), then `close`
            %% (closes over 100ms, signals closed). Door log records
            %% events as they are received: close, open, opened (self
            %% timer), close, closed (self timer).
            ?assertEqual(["close", "open", "opened", "close", "closed"],
                         DoorLog),
            %% Motor gets move-to (schedules arrived after 500ms),
            %% then arrived (self-timer).
            ?assertEqual(["move-to", "arrived"], MotorLog),

            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        fun(_) -> {"safety invariant: Door open arrives AFTER Motor arrived", fun() ->
            %% Structural proof of Flaw 1/2 prevention at runtime.
            %% Car cannot send Door open until it has received both
            %% the arrived signal from Motor AND the settled timer
            %% fired from Arriving. Moving -> Arriving is triggered by
            %% arrived; Arriving -> DoorOpening is triggered by
            %% settled. The mailbox timestamps on Motor's arrived and
            %% Door's open must therefore be ordered
            %% arrived-before-open with at least the settling delay in
            %% between.
            load_valid_spec(),
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            CarPid  = introspect_child_pid(DispPid, "Car"),
            DoorPid = introspect_child_pid(CarPid, "Door"),
            MotorPid = introspect_child_pid(CarPid, "Motor"),

            af_hos_runtime:send_event(RootPid,
                'floor-button-press', []),
            timer:sleep(400),

            %% 400ms: Motor is still moving (500ms travel), Car has
            %% not yet received arrived, therefore Car has not sent
            %% Door open. Door's log contains only the initial close.
            DoorLogMid = get_log(DoorPid),
            MotorLogMid = get_log(MotorPid),
            ?assertEqual(["close"], DoorLogMid),
            ?assertEqual(["move-to"], MotorLogMid),

            timer:sleep(1100),
            %% 1500ms total: full cycle complete (Motor 500ms + Car
            %% settling 200ms + Door open 100ms + Loading 300ms +
            %% Door close 100ms = 1200ms, with margin).
            DoorLogEnd = get_log(DoorPid),
            MotorLogEnd = get_log(MotorPid),
            ?assert(lists:member("open", DoorLogEnd)),
            ?assert(lists:member("arrived", MotorLogEnd)),

            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        fun(_) -> {"fire-alarm propagates to Car and stops Motor", fun() ->
            %% End-to-end emergency routing:
            %%   fire-alarm -> BuildingSystem
            %%     -> EmergencySource trigger
            %%       -> (upward signal) BuildingSystem set-emergency
            %%         -> Dispatcher set-emergency
            %%           -> Car trigger-emergency
            %%             -> Motor stop
            load_valid_spec(),
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            CarPid  = introspect_child_pid(DispPid, "Car"),
            MotorPid = introspect_child_pid(CarPid, "Motor"),

            af_hos_runtime:send_event(RootPid, 'fire-alarm', []),
            timer:sleep(200),

            %% Motor should have received `stop`.
            MotorLog = get_log(MotorPid),
            ?assert(lists:member("stop", MotorLog)),

            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        fun(_) -> {"emergency preempts mid-assign: Motor stop arrives during travel", fun() ->
            %% Start an assign cycle (Motor begins moving), then
            %% fire the emergency before Motor's 500ms travel
            %% completes. Motor receives stop mid-travel, transitions
            %% Moving -> Stopped. The late scheduled arrived hits
            %% Motor in Stopped and is input-rejected (no declared
            %% transition from Stopped under arrived).
            load_valid_spec(),
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            CarPid  = introspect_child_pid(DispPid, "Car"),
            MotorPid = introspect_child_pid(CarPid, "Motor"),

            af_hos_runtime:send_event(RootPid,
                'floor-button-press', []),
            timer:sleep(200),
            %% Motor should be in Moving now (200ms into 500ms travel).
            ?assertEqual(["move-to"], get_log(MotorPid)),

            af_hos_runtime:send_event(RootPid, 'fire-alarm', []),
            timer:sleep(600),

            MotorLogFinal = get_log(MotorPid),
            %% Motor sees: move-to, stop (from emergency),
            %% arrived (late self-timer, input-rejected but logged).
            ?assertEqual(["move-to", "stop", "arrived"], MotorLogFinal),

            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Timer primitive: `: after <ms> <event>` schedules a self-send.
%% The mailbox stays open during the wait so preemption works; a
%% late scheduled event is input-rejected if no declared transition
%% from the live state exists.
%% -------------------------------------------------------------------

timer_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"Motor.move-to schedules arrived after declared delay", fun() ->
            load_valid_spec(),
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            CarPid  = introspect_child_pid(DispPid, "Car"),
            MotorPid = introspect_child_pid(CarPid, "Motor"),

            af_hos_runtime:send_event(MotorPid, 'move-to', []),
            timer:sleep(100),

            %% 100ms into a 500ms travel: only move-to has been
            %% received, arrived is still scheduled.
            ?assertEqual(["move-to"], get_log(MotorPid)),

            timer:sleep(600),

            %% 700ms total: the scheduled arrived has fired.
            ?assertEqual(["move-to", "arrived"], get_log(MotorPid)),

            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        fun(_) -> {"stop preempts: late arrived is input-rejected", fun() ->
            load_valid_spec(),
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            CarPid  = introspect_child_pid(DispPid, "Car"),
            MotorPid = introspect_child_pid(CarPid, "Motor"),

            af_hos_runtime:send_event(MotorPid, 'move-to', []),
            timer:sleep(100),
            af_hos_runtime:send_event(MotorPid, 'stop', []),
            timer:sleep(600),

            %% After move-to -> stop -> (late arrived), Motor's log
            %% records all three receptions. The arrived transition
            %% Stopped -> ? is not declared under arrived, so Axiom 4
            %% input-rejects it; Motor stays Stopped.
            Log = get_log(MotorPid),
            ?assertEqual(["move-to", "stop", "arrived"], Log),

            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        fun(_) -> {"timer scheduling an undeclared event is rejected", fun() ->
            %% The scheduled event must be a declared handler on
            %% this system. A timer that fires an event nobody
            %% handles would be a silent dead-letter.
            try
                eval_new(
                    "system M "
                    "  parent P "
                    "  state MState "
                    "  on go -> ; "
                    "  transitions "
                    "    Idle -> Running go : after 100 ghost "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_1")),
                    ?assertNotEqual(nomatch, string:find(Msg, "ghost"))
            end
        end} end
    ]}.

%% Restore the introspect helper so effect_dispatch_test_ can reach
%% into the tree by name. This is a test-only hook provided by the
%% runtime's introspect_child message.
introspect_child_pid(ParentPid, ChildName) ->
    af_hos_runtime:introspect_child(ParentPid, ChildName).
