-module(af_hos_check_a4_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("actorforth/include/token.hrl").
-include_lib("actorforth/include/continuation.hrl").

%% Sanity tests for the a4 axiom checker in src/bootstrap/hos/check.a4.
%% These run at the Erlang level: load runtime.a4 + check.a4, call
%% check-blueprint-a4 through the a4 interpreter, assert the violation
%% strings match.

setup() ->
    af_repl:init_types(),
    af_hos_check:clear_registry(),
    af_type_compiler:clear_pending_checks(),
    {ok, RtSrc} = file:read_file("src/bootstrap/hos/runtime.a4"),
    _ = af_interpreter:interpret_tokens(
        af_parser:parse(binary_to_list(RtSrc), "runtime.a4"),
        af_interpreter:new_continuation()),
    af_type_compiler:finalize_pending_checks(),
    {ok, CheckSrc} = file:read_file("src/bootstrap/hos/check.a4"),
    _ = af_interpreter:interpret_tokens(
        af_parser:parse(binary_to_list(CheckSrc), "check.a4"),
        af_interpreter:new_continuation()),
    af_type_compiler:finalize_pending_checks(),
    ok.

check(Blueprint) ->
    C0 = (af_interpreter:new_continuation())#continuation{
        data_stack = [Blueprint]
    },
    C1 = af_interpreter:interpret_token(
        #token{value = "check-blueprint-a4"}, C0),
    [{'List', Items}] = C1#continuation.data_stack,
    [binary_to_list(S) || {'String', S} <- Items].

%% Direct call of check-transition-effects against a transitions list,
%% handlers list, children list, and path. Returns plain strings.
check_effects(Transitions, Handlers, Children, PathBin) ->
    C0 = (af_interpreter:new_continuation())#continuation{
        data_stack = [{'String', PathBin},
                      {'List', [{'Tuple', C} || C <- Children]},
                      {'List', [{'Tuple', H} || H <- Handlers]},
                      {'List', [{'Tuple', T} || T <- Transitions]}]
    },
    C1 = af_interpreter:interpret_token(
        #token{value = "check-transition-effects"}, C0),
    [{'List', Items}] = C1#continuation.data_stack,
    [binary_to_list(S) || {'String', S} <- Items].

a4_checker_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"stateless leaf produces no violations", fun() ->
            BP = {'HosBlueprint', <<"Leaf">>, <<>>, 'None',
                  [], [], []},
            ?assertEqual([], check(BP))
        end} end,

        fun(_) -> {"stateful handler body triggers axiom 5", fun() ->
            Handler = {'HandlerSpec', do_thing, [], [],
                       [{'String', <<"junk">>}]},
            BP = {'HosBlueprint', <<"Calc">>, <<>>, 'Counting',
                  [], [Handler], []},
            Msgs = check(BP),
            ?assert(lists:any(fun(M) ->
                string:find(M, "axiom_5") =/= nomatch
                    andalso string:find(M, "has a non-empty body") =/= nomatch
            end, Msgs))
        end} end,

        fun(_) -> {"undeclared trigger triggers axiom 1", fun() ->
            Trans = {'TransitionSpec', idle, busy, start, <<>>, none, 0},
            Handler = {'HandlerSpec', wake, [], [], []},
            BP = {'HosBlueprint', <<"Calc">>, <<>>, 'None',
                  [], [Handler], [Trans]},
            Msgs = check(BP),
            ?assert(lists:any(fun(M) ->
                string:find(M, "axiom_1") =/= nomatch andalso
                string:find(M, "is not a declared handler") =/= nomatch
            end, Msgs))
        end} end,

        fun(_) -> {"unreachable state triggers axiom 5", fun() ->
            T1 = {'TransitionSpec', idle, busy, start, <<>>, none, 0},
            T2 = {'TransitionSpec', busy, idle, finish, <<>>, none, 0},
            T3 = {'TransitionSpec', orphan, idle, reset, <<>>, none, 0},
            BP = {'HosBlueprint', <<"Calc">>, <<>>, 'Counting',
                  [],
                  [{'HandlerSpec', start, [], [], []},
                   {'HandlerSpec', finish, [], [], []},
                   {'HandlerSpec', reset, [], [], []}],
                  [T1, T2, T3]},
            Msgs = check(BP),
            ?assert(lists:any(fun(M) ->
                string:find(M, "is unreachable from initial state") =/= nomatch
            end, Msgs))
        end} end,

        fun(_) -> {"walk-children recurses into child blueprints (#178)", fun() ->
            %% Child blueprint with a stateful-body violation of its own.
            ChildHandler = {'HandlerSpec', do, [], [],
                            [{'String', <<"junk">>}]},
            Child = {'HosBlueprint', <<"Child">>, <<"Parent">>,
                     'Counting', [], [ChildHandler], []},
            C0 = (af_interpreter:new_continuation())#continuation{
                data_stack = [{'List', [Child]}]
            },
            C1 = af_interpreter:interpret_token(
                #token{value = "walk-children"}, C0),
            [{'List', Items}] = C1#continuation.data_stack,
            Msgs = [binary_to_list(S) || {'String', S} <- Items],
            ?assert(lists:any(fun(M) ->
                string:find(M, "has a non-empty body") =/= nomatch
            end, Msgs))
        end} end,

        fun(_) -> {"effect target not a child triggers axiom 1 (#176)", fun() ->
            T = {'TransitionSpec', idle, busy, start,
                 <<"Stranger">>, go, 0},
            Handlers = [{'HandlerSpec', start, [], [], []}],
            C0 = (af_interpreter:new_continuation())#continuation{
                data_stack = [{'String', <<"Mysys">>},
                              {'List', []},
                              {'List', [{'Tuple', H} || H <- Handlers]},
                              {'List', [{'Tuple', T}]}]
            },
            C1 = af_interpreter:interpret_token(
                #token{value = "check-transition-effects"}, C0),
            [{'List', Items}] = C1#continuation.data_stack,
            Msgs = [binary_to_list(S) || {'String', S} <- Items],
            ?assert(lists:any(fun(M) ->
                string:find(M, "transition effect target") =/= nomatch
            end, Msgs))
        end} end,

        fun(_) -> {"timer effect scheduling undeclared event triggers axiom 1 (#176)", fun() ->
            T = {'TransitionSpec', idle, busy, start,
                 <<"after">>, zap, 100},
            Handlers = [{'HandlerSpec', start, [], [], []}],
            C0 = (af_interpreter:new_continuation())#continuation{
                data_stack = [{'String', <<"Mysys">>},
                              {'List', []},
                              {'List', [{'Tuple', H} || H <- Handlers]},
                              {'List', [{'Tuple', T}]}]
            },
            C1 = af_interpreter:interpret_token(
                #token{value = "check-transition-effects"}, C0),
            [{'List', Items}] = C1#continuation.data_stack,
            Msgs = [binary_to_list(S) || {'String', S} <- Items],
            ?assert(lists:any(fun(M) ->
                string:find(M, "timer effect schedules event") =/= nomatch
            end, Msgs))
        end} end,

        fun(_) -> {"handler body with out-of-scope token triggers axiom 1 (#174)", fun() ->
            %% Body token "mystery" isn't a parent/child/state/handler/builtin.
            Handler = {'HandlerSpec', ping, [], [],
                       [{'String', <<"mystery">>}]},
            BP = {'HosBlueprint', <<"Sys">>, <<>>, 'None',
                  [], [Handler], []},
            C0 = (af_interpreter:new_continuation())#continuation{
                data_stack = [{'String', <<"Sys">>}, BP]
            },
            C1 = af_interpreter:interpret_token(
                #token{value = "check-handler-scope"}, C0),
            [{'List', Items}] = C1#continuation.data_stack,
            Msgs = [binary_to_list(S) || {'String', S} <- Items],
            ?assert(lists:any(fun(M) ->
                string:find(M, "references 'mystery'") =/= nomatch
            end, Msgs))
        end} end,

        fun(_) -> {"handler body with undeclared transition triggers axiom 5 (#175)", fun() ->
            %% Body has `-> Busy` but no transition idle -> Busy under 'go'.
            Handler = {'HandlerSpec', go, [], [],
                       [{'String', <<"->">>}, {'String', <<"Busy">>}]},
            BP = {'HosBlueprint', <<"Sys">>, <<>>, 'Mode',
                  [], [Handler], []},
            C0 = (af_interpreter:new_continuation())#continuation{
                data_stack = [{'String', <<"Sys">>}, BP]
            },
            C1 = af_interpreter:interpret_token(
                #token{value = "check-handler-transitions"}, C0),
            [{'List', Items}] = C1#continuation.data_stack,
            Msgs = [binary_to_list(S) || {'String', S} <- Items],
            ?assert(lists:any(fun(M) ->
                string:find(M, "axiom_5") =/= nomatch
            end, Msgs))
        end} end,

        fun(_) -> {"parent/children inconsistency detected (#177)", fun() ->
            %% Register Parent that claims child 'Child', then check
            %% another blueprint 'Child' that declares a DIFFERENT parent.
            Parent = {'HosBlueprint', <<"Parent">>, <<>>, 'None',
                      [{'HosBlueprint', <<"Child">>, <<"Parent">>, 'None',
                        [], [], []}], [], []},
            af_hos_check:register_system(Parent),
            BadChild = {'HosBlueprint', <<"Child">>, <<"Elsewhere">>,
                        'None', [], [], []},
            C0 = (af_interpreter:new_continuation())#continuation{
                data_stack = [BadChild]
            },
            C1 = af_interpreter:interpret_token(
                #token{value = "check-parent-children"}, C0),
            [{'List', Items}] = C1#continuation.data_stack,
            Msgs = [binary_to_list(S) || {'String', S} <- Items],
            ?assert(length(Msgs) >= 1)
        end} end,

        fun(_) -> {"uncovered emergency state triggers axiom 5 (#173)", fun() ->
            %% Two transitions converge on 'stopped' under 'emergency'
            %% but state 'busy' has no transition under 'emergency'.
            T1 = {'TransitionSpec', idle, stopped, emergency,
                  <<>>, none, 0},
            T2 = {'TransitionSpec', running, stopped, emergency,
                  <<>>, none, 0},
            T3 = {'TransitionSpec', idle, running, start,
                  <<>>, none, 0},
            T4 = {'TransitionSpec', running, busy, work,
                  <<>>, none, 0},
            C0 = (af_interpreter:new_continuation())#continuation{
                data_stack = [{'String', <<"Lift">>},
                              {'Atom', "Mode"},
                              {'List', [{'Tuple', T}
                                        || T <- [T1, T2, T3, T4]]}]
            },
            C1 = af_interpreter:interpret_token(
                #token{value = "check-emergency-coverage"}, C0),
            [{'List', Items}] = C1#continuation.data_stack,
            Msgs = [binary_to_list(S) || {'String', S} <- Items],
            ?assert(lists:any(fun(M) ->
                string:find(M, "has emergency trigger") =/= nomatch
                    andalso string:find(M, "busy") =/= nomatch
            end, Msgs))
        end} end
    ]}.
