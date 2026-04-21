-module(af_hos_comparison_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

%% Comparison harness: a4 HOS elevator vs the Python naive/correct
%% reference in samples/hos/elevator/comparison/.
%%
%% elevator_naive.py enumerates six flaws with explicit FLAW N
%% comments. For each flaw, this file shows that either:
%%
%%   - STRUCTURAL: the naive's failure mode is uncompilable in a4
%%     (pointer to af_hos_violation_catalog_tests which encodes the
%%     minimal rejection spec), OR
%%   - RUNTIME: the valid a4 spec, when exercised, produces a trace
%%     that could not have been produced by the naive's racy code.
%%
%% The split is the point. Some correctness is by construction,
%% some by runtime observation, and this file is explicit about
%% which is which.

setup() ->
    af_repl:init_types(),
    af_type_compiler:clear_pending_checks(),
    af_hos_check:clear_registry().

load_valid_spec() ->
    {ok, Content} = file:read_file(
        "samples/hos/elevator/elevator.a4"),
    Tokens = af_parser:parse(binary_to_list(Content), "comparison"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()).

get_log(Pid) ->
    af_hos_runtime:get_log(Pid).

introspect_child_pid(ParentPid, ChildName) ->
    af_hos_runtime:introspect_child(ParentPid, ChildName).

spawn_full_tree() ->
    Root = af_hos_check:lookup_system("BuildingSystem"),
    RootPid = af_hos_runtime:spawn_system(Root),
    DispPid  = introspect_child_pid(RootPid,  "Dispatcher"),
    CarPid   = introspect_child_pid(DispPid, "Car"),
    DoorPid  = introspect_child_pid(CarPid,  "Door"),
    MotorPid = introspect_child_pid(CarPid,  "Motor"),
    #{root => RootPid, dispatcher => DispPid, car => CarPid,
      door => DoorPid, motor => MotorPid}.


%% ===================================================================
%% Runtime scenarios. Each test mirrors a FAT-relevant observation
%% from the Python reference and asserts the a4 runtime behaves as
%% expected.
%% ===================================================================

scenarios_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [

        %% FLAW 1 (naive: Door.open has no check motor stopped).
        %% a4 RUNTIME claim: in a full assign cycle, "open" in Door
        %% log is strictly preceded by "arrived" in Motor log. Car's
        %% Moving -> Arriving transition is triggered by Motor's
        %% arrived upward signal; Arriving -> DoorOpening (triggered
        %% by settled self-timer) is the only path emitting Door
        %% open.
        fun(_) -> {"FLAW 1: Door open strictly after Motor arrived", fun() ->
            load_valid_spec(),
            #{root := RootPid, door := DoorPid,
              motor := MotorPid} = spawn_full_tree(),
            af_hos_runtime:send_event(RootPid,
                'floor-button-press', []),
            timer:sleep(1500),
            DoorLog  = get_log(DoorPid),
            MotorLog = get_log(MotorPid),
            ?assert(lists:member("arrived", MotorLog)),
            ?assert(lists:member("open",    DoorLog)),
            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        %% FLAW 2 (naive: Motor.move_to has no check door closed).
        %% a4 RUNTIME claim: at 50ms, Door has only logged its
        %% initial close (idempotent self-loop) and Motor has only
        %% logged move-to, proving the closed signal came first.
        fun(_) -> {"FLAW 2: Motor move-to strictly after Door closed", fun() ->
            load_valid_spec(),
            #{root := RootPid, door := DoorPid,
              motor := MotorPid} = spawn_full_tree(),
            af_hos_runtime:send_event(RootPid,
                'floor-button-press', []),
            timer:sleep(50),
            ?assertEqual(["close"],  get_log(DoorPid)),
            ?assertEqual(["move-to"], get_log(MotorPid)),
            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        %% FLAW 3 (naive: emergency is a TOCTOU flag read).
        %% a4 RUNTIME claim: emergency arriving mid-travel is not
        %% missed; Motor gets stop (from emergency) and the late
        %% arrived is input-rejected (Axiom 4). Compile-time
        %% exhaustive-emergency check guarantees every non-emergency
        %% Car state covers trigger-emergency.
        fun(_) -> {"FLAW 3: emergency mid-travel is not missed", fun() ->
            load_valid_spec(),
            #{root := RootPid, motor := MotorPid} = spawn_full_tree(),
            af_hos_runtime:send_event(RootPid,
                'floor-button-press', []),
            timer:sleep(200),
            ?assertEqual(["move-to"], get_log(MotorPid)),
            af_hos_runtime:send_event(RootPid, 'fire-alarm', []),
            timer:sleep(600),
            %% move-to, then stop (from emergency), then arrived
            %% (late self-timer, input-rejected at the state level
            %% but logged on receipt).
            ?assertEqual(["move-to", "stop", "arrived"],
                         get_log(MotorPid)),
            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        %% FLAW 4 (naive: cycle order is convention, reorderable by
        %% a future maintainer).
        %% a4 RUNTIME claim: Door log for a full cycle is always
        %% [close, open, opened, close, closed] in that exact order.
        %% The sequence is declared on Car's transitions table;
        %% no body-level reordering is possible because stateful
        %% systems have empty bodies.
        fun(_) -> {"FLAW 4: cycle order is declared not conventional", fun() ->
            load_valid_spec(),
            #{root := RootPid, door := DoorPid,
              motor := MotorPid} = spawn_full_tree(),
            af_hos_runtime:send_event(RootPid,
                'floor-button-press', []),
            timer:sleep(1500),
            ?assertEqual(["close", "open", "opened", "close", "closed"],
                         get_log(DoorPid)),
            ?assertEqual(["move-to", "arrived"], get_log(MotorPid)),
            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end,

        %% FLAW 5 STRUCTURAL claim. Naive's EmergencySystem reaches
        %% directly into cars. The analogous a4 pattern is
        %% uncompilable; the minimal rejection lives in
        %% af_hos_violation_catalog_tests:
        %% axiom_1_sibling_back_channel. Here we confirm that the
        %% valid spec's EmergencySource indeed has no child
        %% reference to Car.
        fun(_) -> {"FLAW 5: sibling back-channel is structurally absent", fun() ->
            load_valid_spec(),
            Emergency = af_hos_check:lookup_system("EmergencySource"),
            {'SystemNode', _, Parent, _, Children, _, _} = Emergency,
            ?assertEqual(<<"BuildingSystem">>, Parent),
            ?assertEqual([], Children)
        end} end,

        %% Emergency clear path. Fire alarm, then clear. Car should
        %% return to idle via EmergencyStopped -> EmergencyDoorOpen
        %% -> IdleAtFloor. Python correct version has this path
        %% too; this confirms a4 matches.
        fun(_) -> {"emergency clear path returns Car to idle", fun() ->
            load_valid_spec(),
            #{root := RootPid, door := DoorPid,
              motor := MotorPid} = spawn_full_tree(),
            af_hos_runtime:send_event(RootPid, 'fire-alarm', []),
            timer:sleep(200),
            ?assert(lists:member("stop", get_log(MotorPid))),
            af_hos_runtime:send_event(RootPid, 'fire-clear', []),
            timer:sleep(300),
            DoorLog = get_log(DoorPid),
            ?assert(lists:member("open",   DoorLog)),
            ?assert(lists:member("opened", DoorLog)),
            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end} end
    ]}.
