-module(af_hos_dsl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

%% Tests for the HOS `system ... end` DSL surface.

setup() ->
    af_repl:init_types(),
    af_type_compiler:clear_pending_checks().

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

eval_new(Input) ->
    eval(Input, af_interpreter:new_continuation()).

%% -------------------------------------------------------------------
%% Parser  -  positive cases
%% -------------------------------------------------------------------

parse_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"minimal system compiles", fun() ->
            C = eval_new("system Foo end \"Foo\" find-system"),
            [Node | _] = C#continuation.data_stack,
            ?assertMatch({'HosBlueprint', <<"Foo">>, _, _, _, _, _}, Node)
        end} end,

        fun(_) -> {"parent section populates parent_name", fun() ->
            C = eval_new("system Child parent Parent end "
                          "\"Child\" find-system"),
            [Node | _] = C#continuation.data_stack,
            ?assertEqual(<<"Parent">>, element(3, Node))
        end} end,

        fun(_) -> {"state section populates state_type", fun() ->
            C = eval_new("system S state MyState end \"S\" find-system"),
            [Node | _] = C#continuation.data_stack,
            ?assertEqual('MyState', element(4, Node))
        end} end,

        fun(_) -> {"children section creates name-stub entries", fun() ->
            C = eval_new("system Parent children A B C end "
                          "\"Parent\" find-system"),
            [Node | _] = C#continuation.data_stack,
            Children = element(5, Node),
            ?assertEqual(3, length(Children)),
            [First | _] = Children,
            ?assertEqual(<<"A">>, element(2, First))
        end} end,

        fun(_) -> {"on-handler captured with event name, sigs, body", fun() ->
            C = eval_new(
                "system S "
                "  on ping Ping -> Pong ; "
                "      dup drop "
                "end "
                "\"S\" find-system"),
            [Node | _] = C#continuation.data_stack,
            [Handler | _] = element(6, Node),
            ?assertEqual(ping, element(2, Handler)),
            ?assertEqual(['Ping'], element(3, Handler)),
            ?assertEqual(['Pong'], element(4, Handler))
        end} end,

        fun(_) -> {"transitions section collects entries", fun() ->
            %% Triggers must be declared as `on X -> ;` handlers to
            %% pass the trigger-scope check (Tier 1).
            C = eval_new(
                "system S "
                "  on start -> ; "
                "  on finish -> ; "
                "  transitions "
                "    Idle -> Busy start "
                "    Busy -> Idle finish "
                "end "
                "\"S\" find-system"),
            [Node | _] = C#continuation.data_stack,
            Transitions = element(7, Node),
            ?assertEqual(2, length(Transitions)),
            [T1, T2] = Transitions,
            ?assertEqual({'TransitionSpec', 'Idle', 'Busy', start, <<>>, none, 0}, T1),
            ?assertEqual({'TransitionSpec', 'Busy', 'Idle', finish, <<>>, none, 0}, T2)
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Axiom enforcement  -  negative cases
%% -------------------------------------------------------------------

axiom_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"well-formed system passes axiom check", fun() ->
            ?assertMatch(#continuation{},
                         eval_new("system S on ping -> ; dup drop end"))
        end} end,

        fun(_) -> {"cross-tree reference is rejected at compile time", fun() ->
            try
                eval_new("system EmergencySource "
                         "  parent BuildingSystem "
                         "  on trigger -> ; "
                         "    SomeDistantCousin reach-through "
                         "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_1")),
                    ?assertNotEqual(nomatch,
                                    string:find(Msg, "EmergencySource")),
                    ?assertNotEqual(nomatch, string:find(Msg, "trigger"))
            end
        end} end,

        fun(_) -> {"parent reference is allowed", fun() ->
            %% `drop` is an a4 builtin; `Parent` is in scope as the
            %% declared parent name.
            ?assertMatch(#continuation{},
                         eval_new("system S "
                                  "  parent Parent "
                                  "  on up -> ; "
                                  "    Parent drop "
                                  "end"))
        end} end,

        fun(_) -> {"child reference is allowed", fun() ->
            ?assertMatch(#continuation{},
                         eval_new("system S "
                                  "  children Kid "
                                  "  on dispatch -> ; "
                                  "    Kid drop "
                                  "end"))
        end} end,

        fun(_) -> {"parent/children consistency: child says wrong parent", fun() ->
            %% The parent is registered first declaring Kid as a child.
            %% Then Kid is registered claiming a different parent.
            try
                eval_new("system Owner children Kid end "
                         "system Kid parent Stranger end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch,
                                    string:find(Msg, "consistency"))
            end
        end} end,

        fun(_) -> {"parent/children consistency: parent omits child", fun() ->
            %% Child registered declaring Owner as parent. Then Owner
            %% is registered but does NOT list Child.
            try
                eval_new("system Child parent Owner end "
                         "system Owner end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch,
                                    string:find(Msg, "consistency"))
            end
        end} end,

        fun(_) -> {"sibling-of-sibling axiom_1 violation (Flaw 5)", fun() ->
            %% EmergencySource has only `Parent` in its upward scope.
            %% Referencing Car (a sibling) should be rejected.
            try
                eval_new("system EmergencySource "
                         "  parent BuildingSystem "
                         "  on trigger -> ; "
                         "    Car halt "
                         "end"),
                ?assert(false)
            catch
                error:{axiom_violation, _} -> ok
            end
        end} end,

        fun(_) -> {"axiom_5: declared transition is allowed", fun() ->
            %% The body walks along a declared transition edge.
            ?assertMatch(#continuation{},
                         eval_new("system D "
                                  "  on open -> ; "
                                  "    -> Opening "
                                  "    -> Open "
                                  "  transitions "
                                  "    Closed -> Opening open "
                                  "    Opening -> Open open "
                                  "end"))
        end} end,

        fun(_) -> {"axiom_5: undeclared transition is rejected", fun() ->
            %% Body attempts `Closed -> Open` in one step; the table
            %% only permits Closed -> Opening (via `open`). Axiom 5
            %% raises.
            try
                eval_new("system D "
                         "  on open -> ; "
                         "    -> Open "
                         "  transitions "
                         "    Closed -> Opening open "
                         "    Opening -> Open open "
                         "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_5"))
            end
        end} end,

        fun(_) -> {"axiom_5: transition with wrong trigger is rejected", fun() ->
            %% Closed -> Opening is declared under trigger `open`, not
            %% `shutdown`. The `on shutdown` body cannot use it.
            try
                eval_new("system D "
                         "  on shutdown -> ; "
                         "    -> Opening "
                         "  transitions "
                         "    Closed -> Opening open "
                         "end"),
                ?assert(false)
            catch
                error:{axiom_violation, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "axiom_5"))
            end
        end} end
    ]}.
