-module(af_hos_check_tests).

-include_lib("eunit/include/eunit.hrl").

%% Unit tests for the HOS axiom checker.
%%
%% Each test constructs a SystemNode tree manually (as the raw tuples
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
            Root = {'SystemNode', <<"Root">>, <<>>, 'None', [], [], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Root))
        end} end,

        fun(_) -> {"handler body uses only a4 builtins (dup + drop)", fun() ->
            Handler = {'HandlerSpec', add_one, [], [],
                       [<<"dup">>, <<"1">>, <<"+">>]},
            Sys = {'SystemNode', <<"Calc">>, <<"">>, 'None',
                    [], [Handler], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"handler body references parent and self legally", fun() ->
            Handler = {'HandlerSpec', notify, [], [],
                       [<<"Parent">>, <<"Self">>]},
            Sys = {'SystemNode', <<"Self">>, <<"Parent">>, 'None',
                    [], [Handler], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"handler body references declared child", fun() ->
            Child = {'SystemNode', <<"ChildA">>, <<"Owner">>, 'None',
                     [], [], []},
            Handler = {'HandlerSpec', dispatch, [], [],
                       [<<"ChildA">>]},
            Sys = {'SystemNode', <<"Owner">>, <<>>, 'None',
                    [Child], [Handler], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"handler body references a declared event name", fun() ->
            H1 = {'HandlerSpec', callout, [], [],
                  [<<"sibling-event">>]},
            H2 = {'HandlerSpec', 'sibling-event', [], [], []},
            Sys = {'SystemNode', <<"X">>, <<>>, 'None',
                    [], [H1, H2], []},
            ?assertEqual({ok, []}, af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"nested child's handler references its parent name", fun() ->
            ChildHandler = {'HandlerSpec', up, [], [],
                            [<<"Owner">>]},
            Child = {'SystemNode', <<"ChildB">>, <<"Owner">>, 'None',
                     [], [ChildHandler], []},
            Sys = {'SystemNode', <<"Owner">>, <<>>, 'None',
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
            Sys = {'SystemNode', <<"S">>, <<>>, 'None',
                    [], [Handler], []},
            ?assertMatch({violations, [_]},
                         af_hos_check:check_system(Sys))
        end} end,

        fun(_) -> {"violation message cites axiom, system, handler, token", fun() ->
            Handler = {'HandlerSpec', boom, [], [],
                       [<<"ForbiddenName">>]},
            Sys = {'SystemNode', <<"MySystem">>, <<>>, 'None',
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
            Emergency = {'SystemNode', <<"EmergencySource">>,
                         <<"BuildingSystem">>, 'None',
                         [], [EmergencyHandler], []},
            {violations, Vs} = af_hos_check:check_system(Emergency),
            ?assertEqual(2, length(Vs))
        end} end,

        fun(_) -> {"nested child violation is found under correct path", fun() ->
            BadChildHandler = {'HandlerSpec', go, [], [],
                               [<<"great-aunt">>]},
            BadChild = {'SystemNode', <<"DeepChild">>,
                        <<"MidLevel">>, 'None',
                        [], [BadChildHandler], []},
            Mid = {'SystemNode', <<"MidLevel">>, <<"Root">>, 'None',
                    [BadChild], [], []},
            Root = {'SystemNode', <<"Root">>, <<>>, 'None',
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
            Sys = {'SystemNode', <<"S">>, <<>>, 'None', [], [], []},
            ?assertEqual(ok, af_hos_check:check_system_raise(Sys))
        end} end,

        fun(_) -> {"violating system raises axiom_violation", fun() ->
            Handler = {'HandlerSpec', boom, [], [],
                       [<<"NotInScope">>]},
            Sys = {'SystemNode', <<"S">>, <<>>, 'None',
                    [], [Handler], []},
            ?assertError({axiom_violation, _},
                         af_hos_check:check_system_raise(Sys))
        end} end
    ]}.
