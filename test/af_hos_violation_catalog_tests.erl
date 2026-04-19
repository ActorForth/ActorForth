-module(af_hos_violation_catalog_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

%% Violation catalog for the HOS DSL.
%%
%% One minimal spec per failure pattern per axiom. Each test is the
%% smallest system text that triggers the violation and asserts the
%% pointed error message. Read end to end, this file is a
%% demonstration: every axiom is violated in the most obvious way,
%% each one is caught with a specific message. A skeptic of the
%% correctness claim can start here.
%%
%% Test naming: axiom_N_pattern / consistency_pattern / sanity_name.

setup() ->
    af_repl:init_types(),
    af_type_compiler:clear_pending_checks(),
    af_hos_check:clear_registry().

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "violation_catalog"),
    af_interpreter:interpret_tokens(Tokens, Cont).

eval_new(Input) ->
    eval(Input, af_interpreter:new_continuation()).

%% Assert that evaluating Spec raises an axiom_violation whose
%% message contains each of the given Needles.
assert_violation(Spec, Needles) ->
    try
        eval_new(Spec),
        ?assert(false)
    catch
        error:{axiom_violation, Msg} ->
            lists:foreach(fun(N) ->
                ?assertNotEqual({nomatch_for, N, Msg},
                                case string:find(Msg, N) of
                                    nomatch -> {nomatch_for, N, Msg};
                                    _       -> found
                                end)
            end, Needles)
    end.


%% ===================================================================
%% Axiom 1: scope (no siblings, no cross-tree, no forged names,
%% effects in scope, timers hit declared events, triggers declared).
%% ===================================================================

axiom_1_sibling_back_channel_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% EmergencySource tries to reach Car, its sibling under
        %% BuildingSystem. Neither is in Emergency's scope; only
        %% BuildingSystem (the parent) is.
        assert_violation(
            "system EmergencySource "
            "  parent BuildingSystem "
            "  on trigger -> ; "
            "    Car trigger-emergency "
            "end",
            ["axiom_1", "Car"])
     end}.

axiom_1_skip_level_down_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% BuildingSystem's scope is its children (Dispatcher,
        %% EmergencySource). Car is a grandchild and out of scope.
        assert_violation(
            "system BuildingSystem "
            "  children Dispatcher EmergencySource "
            "  on emergency -> ; "
            "    Car trigger-emergency "
            "end",
            ["axiom_1", "Car"])
     end}.

axiom_1_body_unknown_name_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Stateless system whose handler body references a name
        %% that is neither parent, child, own event, nor builtin.
        assert_violation(
            "system S "
            "  parent P "
            "  on step -> ; "
            "    check-emergency-flag "
            "end",
            ["axiom_1", "check-emergency-flag"])
     end}.

axiom_1_effect_target_out_of_scope_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Effect target `Stranger` is not parent, not child,
        %% not self. Rejected on the transition itself, even
        %% though the handler body is (correctly) empty.
        assert_violation(
            "system S "
            "  parent P "
            "  state SState "
            "  on go -> ; "
            "  transitions "
            "    Idle -> Done go : Stranger step "
            "end",
            ["axiom_1", "Stranger"])
     end}.

axiom_1_timer_undeclared_event_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% `: after N event` schedules a self-send of `event`. If
        %% `event` is not declared as an `on` handler on this
        %% system, the scheduled event can never fire a transition.
        assert_violation(
            "system S "
            "  state SState "
            "  on go -> ; "
            "  transitions "
            "    Idle -> Waiting go : after 100 ghost "
            "end",
            ["axiom_1", "ghost"])
     end}.

axiom_1_trigger_scope_typo_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Handler declares `strat` but transition references
        %% `start`. The typo used to silently produce no-ops at
        %% runtime; Tier 1 surfaces it at compile time.
        assert_violation(
            "system S "
            "  state SState "
            "  on strat -> ; "
            "  transitions "
            "    Idle -> Busy start "
            "end",
            ["axiom_1", "start", "not a declared handler"])
     end}.


%% ===================================================================
%% Axiom 5: transitions (stateful body empty, body markers match
%% the table, trigger match, reachability, emergency exhaustion).
%% ===================================================================

axiom_5_stateful_body_nonempty_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Stateful system, non-empty handler body. Driving is done
        %% by the transitions table; bodies must be empty.
        assert_violation(
            "system S "
            "  state SState "
            "  on step -> ; "
            "    -> Ready "
            "  transitions "
            "    Idle -> Ready step "
            "end",
            ["axiom_5", "non-empty body"])
     end}.

axiom_5_body_undeclared_transition_step_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Stateless system (no `state`): body markers ARE allowed,
        %% but each must match a declared transition. The body
        %% says Idle -> Ready via `go` but only Ready -> Done is
        %% declared.
        assert_violation(
            "system S "
            "  on go -> ; "
            "    -> Ready "
            "  transitions "
            "    Ready -> Done go "
            "end",
            ["axiom_5"])
     end}.

axiom_5_unreachable_state_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Orphan is named as a From but no other transition
        %% targets it. Unreachable from initial (Idle).
        assert_violation(
            "system S "
            "  state SState "
            "  on go -> ; "
            "  transitions "
            "    Idle -> Ready go "
            "    Orphan -> Ready go "
            "end",
            ["axiom_5", "Orphan", "unreachable"])
     end}.

axiom_5_exhaustive_emergency_missing_state_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Two non-emergency states, Running and Cooldown. `stop`
        %% covers Idle and Running but not Cooldown. An emergency
        %% arriving in Cooldown would be silently dropped.
        assert_violation(
            "system S "
            "  state SState "
            "  on go -> ; "
            "  on cool -> ; "
            "  on stop -> ; "
            "  on clear -> ; "
            "  transitions "
            "    Idle -> Running go "
            "    Running -> Cooldown cool "
            "    Cooldown -> Idle clear "
            "    Idle -> Halted stop "
            "    Running -> Halted stop "
            "    Halted -> Idle clear "
            "end",
            ["axiom_5", "Cooldown", "stop"])
     end}.


%% ===================================================================
%% Parent/children consistency: bidirectional. Either direction
%% of declaration is checked when the second system registers.
%% ===================================================================

consistency_parent_forward_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Door declares parent=Somewhere. Then Car declares
        %% children Door but parent=Dispatcher. The check catches
        %% that Door claims Somewhere as parent while Car lists
        %% Door as a child.
        assert_violation(
            "system Door parent Somewhere end "
            "system Car parent Dispatcher children Door end",
            ["consistency"])
     end}.

consistency_parent_reverse_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Motor declares parent=Dispatcher. Then Car registers
        %% with children Door Motor. Motor's parent does not match
        %% Car, so the consistency check fires.
        assert_violation(
            "system Motor parent Dispatcher end "
            "system Car parent Dispatcher children Door Motor end",
            ["consistency"])
     end}.

consistency_parent_omits_child_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Parent registers first, listing only one child. Second
        %% system declares parent=P but P does not list it.
        assert_violation(
            "system P children A end "
            "system B parent P end",
            ["consistency"])
     end}.


%% ===================================================================
%% Sanity checks: minimal accepted specs. If these ever start
%% failing, the catalog's violation tests are no longer evidence of
%% a working checker -- they might be rejecting good specs too.
%% ===================================================================

sanity_empty_system_ok_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        Cont = eval_new("system S end"),
        ?assert(is_record(Cont, continuation))
     end}.

sanity_stateless_router_ok_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        Cont = eval_new(
            "system Hub "
            "  children A "
            "  on ping -> ; "
            "    A ping "
            "end"),
        ?assert(is_record(Cont, continuation))
     end}.

sanity_stateful_transition_ok_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        Cont = eval_new(
            "system S "
            "  state SState "
            "  on go -> ; "
            "  transitions "
            "    Idle -> Ready go "
            "end"),
        ?assert(is_record(Cont, continuation))
     end}.

sanity_timer_primitive_ok_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        Cont = eval_new(
            "system S "
            "  state SState "
            "  on go -> ; "
            "  on done -> ; "
            "  transitions "
            "    Idle -> Busy go : after 100 done "
            "    Busy -> Idle done "
            "end"),
        ?assert(is_record(Cont, continuation))
     end}.

sanity_upward_signal_ok_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Child declares parent and emits a signal to it via an
        %% effect target == parent name. The checker permits this
        %% without any new syntax.
        Cont = eval_new(
            "system Child "
            "  parent P "
            "  state CState "
            "  on work -> ; "
            "  transitions "
            "    Idle -> Done work : P finished "
            "end"),
        ?assert(is_record(Cont, continuation))
     end}.

sanity_exhaustive_emergency_coverage_ok_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun() ->
        %% Two non-emergency states, both declare the emergency
        %% transition; Recovering is downstream of Halted and
        %% therefore excluded from the coverage requirement.
        Cont = eval_new(
            "system S "
            "  state SState "
            "  on go -> ; "
            "  on stop -> ; "
            "  on clear -> ; "
            "  on done -> ; "
            "  transitions "
            "    Idle -> Running go "
            "    Running -> Idle go "
            "    Idle -> Halted stop "
            "    Running -> Halted stop "
            "    Halted -> Recovering clear "
            "    Recovering -> Idle done "
            "end"),
        ?assert(is_record(Cont, continuation))
     end}.
