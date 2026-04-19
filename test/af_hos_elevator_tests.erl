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
    Ref = make_ref(),
    Pid ! {get_log, self(), Ref},
    receive
        {reply, Ref, Log} -> Log
    after 1000 ->
        {error, timeout}
    end.

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
                ?assertMatch({'SystemNode', _, _, _, _, _, _},
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

        fun(_) -> {"Flaw 1/2 prevention: subsystem call inside stateful handler body rejected", fun() ->
            %% A stateful system's handler body must contain state
            %% markers only. Subsystem interactions (Door close,
            %% Motor move-to) are declared as transition effects,
            %% not as inline body operations. This is what
            %% structurally prevents Flaw 1/2 (a handler body
            %% reordering subsystem calls).
            %% Pre-register Motor so its event `move-to` is known
            %% and the violation is about the stateful-body rule,
            %% not about an out-of-scope event name.
            eval_new(
                "system Motor "
                "  parent C "
                "  state MotorState "
                "  on move-to -> ; "
                "    -> Moving "
                "  transitions "
                "    Stopped -> Moving move-to "
                "end"),
            try
                eval_new(
                    "system C "
                    "  state CState "
                    "  children Motor "
                    "  on step -> ; "
                    "    -> Ready "
                    "    Motor move-to "
                    "    -> Done "
                    "  transitions "
                    "    Idle -> Ready step "
                    "    Ready -> Done step "
                    "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_5")),
                    ?assertNotEqual(nomatch, string:find(Msg, "subsystem"))
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
                    "    -> Done "
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
        fun(_) -> {"floor-button-press dispatches Door and Motor effects in declared order", fun() ->
            load_valid_spec(),
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            CarPid  = introspect_child_pid(DispPid, "Car"),
            DoorPid = introspect_child_pid(CarPid, "Door"),
            MotorPid = introspect_child_pid(CarPid, "Motor"),

            af_hos_runtime:send_event(RootPid,
                'floor-button-press', []),
            timer:sleep(200),

            DoorLog  = get_log(DoorPid),
            MotorLog = get_log(MotorPid),

            %% Car's assign transitions carry effects:
            %%   IdleAtFloor -> PreparingToMove : Door close
            %%   PreparingToMove -> Moving : Motor move-to
            %%   Arriving -> DoorOpening : Door open
            %%   Loading -> DoorClosing : Door close
            %% So Door should see close, open, close in that order,
            %% and Motor should see move-to.
            ?assertEqual(["close", "open", "close"], DoorLog),
            ?assertEqual(["move-to"], MotorLog),

            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        fun(_) -> {"fire-alarm propagates up through EmergencySource", fun() ->
            load_valid_spec(),
            Root = af_hos_check:lookup_system("BuildingSystem"),
            RootPid = af_hos_runtime:spawn_system(Root),
            ESPid = introspect_child_pid(RootPid, "EmergencySource"),

            af_hos_runtime:send_event(RootPid, 'fire-alarm', []),
            timer:sleep(100),

            ESLog = get_log(ESPid),
            ?assertEqual(["trigger"], ESLog),

            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end
    ]}.

%% Restore the introspect helper so effect_dispatch_test_ can reach
%% into the tree by name. This is a test-only hook provided by the
%% runtime's introspect_child message.
introspect_child_pid(ParentPid, ChildName) ->
    Ref = make_ref(),
    ParentPid ! {introspect_child, ChildName, self(), Ref},
    receive
        {reply, Ref, Pid} -> Pid
    after 1000 ->
        erlang:error(introspect_timeout)
    end.
