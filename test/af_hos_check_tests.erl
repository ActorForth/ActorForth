-module(af_hos_check_tests).

-include_lib("eunit/include/eunit.hrl").

%% Unit tests for the HOS axiom checker.
%%
%% Each test constructs a HosBlueprint tree manually (as the raw tuples
%% a4 product-type instances are stored as) and verifies that the
%% checker either accepts it or reports the expected axiom violation.

setup() ->
    af_repl:init_types().


%% -------------------------------------------------------------------
%% Positive cases  -  no axioms violated.
%% -------------------------------------------------------------------

positive_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"empty root system with no handlers or children", fun() ->
            Root = {'HosBlueprint', <<"Root">>, <<>>, 'None', [], [], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Root))
        end} end,

        fun(_) -> {"handler body uses only a4 builtins (dup + drop)", fun() ->
            Handler = {'HandlerSpec', add_one, [], [],
                       [<<"dup">>, <<"1">>, <<"+">>]},
            Sys = {'HosBlueprint', <<"Calc">>, <<"">>, 'None',
                    [], [Handler], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"handler body references parent and self legally", fun() ->
            Handler = {'HandlerSpec', notify, [], [],
                       [<<"Parent">>, <<"Self">>]},
            Sys = {'HosBlueprint', <<"Self">>, <<"Parent">>, 'None',
                    [], [Handler], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"handler body references declared child", fun() ->
            Child = {'HosBlueprint', <<"ChildA">>, <<"Owner">>, 'None',
                     [], [], []},
            Handler = {'HandlerSpec', dispatch, [], [],
                       [<<"ChildA">>]},
            Sys = {'HosBlueprint', <<"Owner">>, <<>>, 'None',
                    [Child], [Handler], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"handler body references a declared event name", fun() ->
            H1 = {'HandlerSpec', callout, [], [],
                  [<<"sibling-event">>]},
            H2 = {'HandlerSpec', 'sibling-event', [], [], []},
            Sys = {'HosBlueprint', <<"X">>, <<>>, 'None',
                    [], [H1, H2], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"nested child's handler references its parent name", fun() ->
            ChildHandler = {'HandlerSpec', up, [], [],
                            [<<"Owner">>]},
            Child = {'HosBlueprint', <<"ChildB">>, <<"Owner">>, 'None',
                     [], [ChildHandler], []},
            Sys = {'HosBlueprint', <<"Owner">>, <<>>, 'None',
                    [Child], [], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Negative cases  -  axioms violated.
%% -------------------------------------------------------------------

negative_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"handler references an out-of-scope name", fun() ->
            Handler = {'HandlerSpec', boom, [], [],
                       [<<"SomeExternalThing">>]},
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'None',
                    [], [Handler], []},
            ?assertMatch({violations, [_]},
                         af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"violation message cites axiom, system, handler, token", fun() ->
            Handler = {'HandlerSpec', boom, [], [],
                       [<<"ForbiddenName">>]},
            Sys = {'HosBlueprint', <<"MySystem">>, <<>>, 'None',
                    [], [Handler], []},
            {violations, [Msg]} = af_hos_check:check_system(Sys),
            ?assertNotEqual(nomatch, string:find(Msg, "axiom_1")),
            ?assertNotEqual(nomatch, string:find(Msg, "MySystem")),
            ?assertNotEqual(nomatch, string:find(Msg, "boom")),
            ?assertNotEqual(nomatch, string:find(Msg, "ForbiddenName"))
        end} end,

        fun(_) -> {"sibling-reference is rejected (Flaw 5 pattern)", fun() ->
            %% EmergencySource tries to reference Dispatcher (its
            %% sibling under BuildingSystem). Neither is in Emergency's
            %% scope  -  only BuildingSystem (the parent) is.
            EmergencyHandler = {'HandlerSpec', trigger, [], [],
                                [<<"Dispatcher">>, <<"fire-at-motor">>]},
            Emergency = {'HosBlueprint', <<"EmergencySource">>,
                         <<"BuildingSystem">>, 'None',
                         [], [EmergencyHandler], []},
            {violations, Vs} = af_hos_check:check_system(Emergency),
            ?assertEqual(2, length(Vs))
        end} end,

        fun(_) -> {"nested child violation is found under correct path", fun() ->
            BadChildHandler = {'HandlerSpec', go, [], [],
                               [<<"great-aunt">>]},
            BadChild = {'HosBlueprint', <<"DeepChild">>,
                        <<"MidLevel">>, 'None',
                        [], [BadChildHandler], []},
            Mid = {'HosBlueprint', <<"MidLevel">>, <<"Root">>, 'None',
                    [BadChild], [], []},
            Root = {'HosBlueprint', <<"Root">>, <<>>, 'None',
                     [Mid], [], []},
            {violations, [Msg]} = af_hos_check:check_system(Root),
            ?assertNotEqual(nomatch, string:find(Msg, "Root.MidLevel.DeepChild"))
        end} end
    ]}.


%% -------------------------------------------------------------------
%% check_system_raise/1
%% -------------------------------------------------------------------

raise_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"ok system does not raise", fun() ->
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'None', [], [], []},
            ?assertEqual(ok, af_hos_check:check_system_raise(Sys))
        end} end,

        fun(_) -> {"violating system raises axiom_violation", fun() ->
            Handler = {'HandlerSpec', boom, [], [],
                       [<<"NotInScope">>]},
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'None',
                    [], [Handler], []},
            ?assertError({axiom_violation, _},
                         af_hos_check:check_system_raise(Sys))
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Tier 1: trigger-scope check. Every transition's trigger must be a
%% declared `on X -> ;` handler on the system. A typo or missing
%% declaration silently produces a no-op at runtime; we want it
%% rejected at compile time instead.
%% -------------------------------------------------------------------

trigger_scope_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"trigger with declared handler is accepted", fun() ->
            H = {'HandlerSpec', go, [], [], []},
            T = {'TransitionSpec', 'Idle', 'Ready', go, <<>>, 'none', 0},
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'SState', [], [H], [T]},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"trigger without declared handler is rejected", fun() ->
            %% `start` appears as a trigger but there is no `on start`
            %% handler; the spec compiles silently today and the
            %% runtime drops the event. This check surfaces the typo.
            T = {'TransitionSpec', 'Idle', 'Ready', start, <<>>, 'none', 0},
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'SState', [], [], [T]},
            {violations, Vs} = af_hos_check:check_system(Sys),
            ?assert(lists:any(fun(M) ->
                string:find(M, "axiom_1") =/= nomatch andalso
                string:find(M, "start") =/= nomatch andalso
                string:find(M, "not a declared handler") =/= nomatch
            end, Vs))
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Tier 1: state reachability. Every state named in the transitions
%% table must be reachable from the initial state (first transition's
%% From) via some chain. Unreachable states are dead code.
%% -------------------------------------------------------------------

reachability_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"connected state graph is accepted", fun() ->
            H1 = {'HandlerSpec', go, [], [], []},
            H2 = {'HandlerSpec', reset, [], [], []},
            Ts = [
                {'TransitionSpec', 'Idle',    'Ready',   go,    <<>>, 'none', 0},
                {'TransitionSpec', 'Ready',   'Working', go,    <<>>, 'none', 0},
                {'TransitionSpec', 'Working', 'Idle',    reset, <<>>, 'none', 0}
            ],
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'SState',
                    [], [H1, H2], Ts},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"unreachable state is rejected", fun() ->
            %% `Orphan` is named only as the From of a transition that
            %% no other transition targets, so it is unreachable from
            %% the initial state (Idle).
            H = {'HandlerSpec', go, [], [], []},
            Ts = [
                {'TransitionSpec', 'Idle',   'Ready',  go, <<>>, 'none', 0},
                {'TransitionSpec', 'Orphan', 'Ready',  go, <<>>, 'none', 0}
            ],
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'SState',
                    [], [H], Ts},
            {violations, Vs} = af_hos_check:check_system(Sys),
            ?assert(lists:any(fun(M) ->
                string:find(M, "axiom_5") =/= nomatch andalso
                string:find(M, "Orphan") =/= nomatch andalso
                string:find(M, "unreachable") =/= nomatch
            end, Vs))
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Tier 1: exhaustive emergency entries. A trigger whose transitions
%% converge on a single target state is an "emergency trigger": every
%% non-emergency state must have a transition into that target under
%% the trigger, otherwise an emergency arriving in the uncovered
%% state is silently dropped (Axiom 4 input rejection).
%% -------------------------------------------------------------------

exhaustive_emergency_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"exhaustive emergency coverage is accepted", fun() ->
            %% Two non-emergency states, both with a transition into
            %% the emergency target under the emergency trigger.
            H1 = {'HandlerSpec', go,    [], [], []},
            H2 = {'HandlerSpec', stop,  [], [], []},
            H3 = {'HandlerSpec', clear, [], [], []},
            Ts = [
                {'TransitionSpec', 'Idle',    'Running', go,    <<>>, 'none', 0},
                {'TransitionSpec', 'Running', 'Idle',    go,    <<>>, 'none', 0},
                {'TransitionSpec', 'Idle',    'Halted',  stop,  <<>>, 'none', 0},
                {'TransitionSpec', 'Running', 'Halted',  stop,  <<>>, 'none', 0},
                {'TransitionSpec', 'Halted',  'Idle',    clear, <<>>, 'none', 0}
            ],
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'SState',
                    [], [H1, H2, H3], Ts},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"missing emergency entry on one state is rejected", fun() ->
            %% Three non-emergency states (Idle, Running, Cooldown)
            %% but the stop trigger only covers Idle and Running.
            %% An emergency arriving in Cooldown would be lost.
            H1 = {'HandlerSpec', go,    [], [], []},
            H2 = {'HandlerSpec', cool,  [], [], []},
            H3 = {'HandlerSpec', stop,  [], [], []},
            H4 = {'HandlerSpec', clear, [], [], []},
            Ts = [
                {'TransitionSpec', 'Idle',     'Running',  go,    <<>>, 'none', 0},
                {'TransitionSpec', 'Running',  'Cooldown', cool,  <<>>, 'none', 0},
                {'TransitionSpec', 'Cooldown', 'Idle',     clear, <<>>, 'none', 0},
                {'TransitionSpec', 'Idle',     'Halted',   stop,  <<>>, 'none', 0},
                {'TransitionSpec', 'Running',  'Halted',   stop,  <<>>, 'none', 0},
                {'TransitionSpec', 'Halted',   'Idle',     clear, <<>>, 'none', 0}
            ],
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'SState',
                    [], [H1, H2, H3, H4], Ts},
            {violations, Vs} = af_hos_check:check_system(Sys),
            ?assert(lists:any(fun(M) ->
                string:find(M, "axiom_5") =/= nomatch andalso
                string:find(M, "Cooldown") =/= nomatch andalso
                string:find(M, "stop") =/= nomatch
            end, Vs))
        end} end,

        fun(_) -> {"recovery state downstream of emergency is not required to cover", fun() ->
            %% Target = Halted. Halted -> Recovering (via clear) is a
            %% recovery edge. Recovering does not need a `stop`
            %% transition because it is downstream of Halted and does
            %% not return to the main graph except via a final
            %% transition that DOES reach a main-graph state.
            %% Here Recovering -> Idle under `done`; Idle has `stop`
            %% declared, so Recovering is correctly classified as an
            %% emergency-side state and excluded.
            H1 = {'HandlerSpec', go,    [], [], []},
            H2 = {'HandlerSpec', stop,  [], [], []},
            H3 = {'HandlerSpec', clear, [], [], []},
            H4 = {'HandlerSpec', done,  [], [], []},
            Ts = [
                {'TransitionSpec', 'Idle',       'Running',    go,    <<>>, 'none', 0},
                {'TransitionSpec', 'Running',    'Idle',       go,    <<>>, 'none', 0},
                {'TransitionSpec', 'Idle',       'Halted',     stop,  <<>>, 'none', 0},
                {'TransitionSpec', 'Running',    'Halted',     stop,  <<>>, 'none', 0},
                {'TransitionSpec', 'Halted',     'Recovering', clear, <<>>, 'none', 0},
                {'TransitionSpec', 'Recovering', 'Idle',       done,  <<>>, 'none', 0}
            ],
            Sys = {'HosBlueprint', <<"S">>, <<>>, 'SState',
                    [], [H1, H2, H3, H4], Ts},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end
    ]}.
