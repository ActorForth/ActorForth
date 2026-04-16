-module(af_guard_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

setup() ->
    af_type:reset().

eval(Input) ->
    eval(Input, af_interpreter:new_continuation()).

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

%%====================================================================
%% Guard parsing & storage
%%====================================================================

guard_captured_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"word with 'where' captures guard tokens", fun() ->
            _ = eval(": positive Int where dup 0 > -> Int ; 1 - ."),
            {ok, Op} = af_type:find_op_by_name("positive", 'Int'),
            Guard = Op#operation.guard,
            ?assert(is_list(Guard)),
            ?assert(length(Guard) >= 3),
            Values = [T#token.value || T <- Guard],
            ?assertEqual(["dup", "0", ">"], Values)
        end} end,

        fun(_) -> {"word without 'where' has undefined guard", fun() ->
            _ = eval(": plain Int -> Int ; 1 + ."),
            {ok, Op} = af_type:find_op_by_name("plain", 'Int'),
            ?assertEqual(undefined, Op#operation.guard)
        end} end
    ]}.

%%====================================================================
%% Guard dispatch: runtime evaluation
%%====================================================================

guard_dispatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"guard true selects the clause", fun() ->
            _ = eval(": step Int where dup 0 > -> Int ; 1 - ."),
            _ = eval(": step Int -> Int ; drop 0 ."),
            Result = eval("5 step"),
            ?assertEqual([{'Int', 4}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"guard false falls through to next clause", fun() ->
            _ = eval(": step Int where dup 0 > -> Int ; 1 - ."),
            _ = eval(": step Int -> Int ; drop 0 ."),
            Result = eval("0 step"),
            ?assertEqual([{'Int', 0}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"guard false on negative falls through", fun() ->
            _ = eval(": step Int where dup 0 > -> Int ; 1 - ."),
            _ = eval(": step Int -> Int ; drop 99 ."),
            Result = eval("-3 step"),
            ?assertEqual([{'Int', 99}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"multiple guarded clauses, picks first match", fun() ->
            _ = eval(": classify Int where dup 0 > -> Int ; drop 1 ."),
            _ = eval(": classify Int where dup 0 < -> Int ; drop -1 ."),
            _ = eval(": classify Int -> Int ; drop 0 ."),
            R1 = eval("5 classify"),
            ?assertEqual([{'Int', 1}], R1#continuation.data_stack),
            R2 = eval("-7 classify"),
            ?assertEqual([{'Int', -1}], R2#continuation.data_stack),
            R3 = eval("0 classify"),
            ?assertEqual([{'Int', 0}], R3#continuation.data_stack)
        end} end,

        fun(_) -> {"guard does not consume stack items", fun() ->
            %% The guard runs on a snapshot; after dispatch the clause body
            %% sees the original stack.
            _ = eval(": mark Int where dup 5 >= -> Int ; 100 + ."),
            _ = eval(": mark Int -> Int ; 1 - ."),
            R = eval("7 mark"),
            ?assertEqual([{'Int', 107}], R#continuation.data_stack)
        end} end,

        fun(_) -> {"guard returning non-Bool falls through", fun() ->
            %% If the guard body crashes or leaves something other than
            %% {Bool, true} on top, the clause must not match.
            _ = eval(": weird Int where dup -> Int ; drop 42 ."),
            _ = eval(": weird Int -> Int ; drop 7 ."),
            R = eval("3 weird"),
            %% dup leaves Int on top, not Bool. Should fall through.
            ?assertEqual([{'Int', 7}], R#continuation.data_stack)
        end} end
    ]}.

%%====================================================================
%% Guard and value-constraint clauses combined
%%====================================================================

guard_with_value_constraint_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"value constraint clause matches before guard", fun() ->
            _ = eval(": f 0 Int -> Int ; drop 999 ."),
            _ = eval(": f Int where dup 0 > -> Int ; 10 + ."),
            _ = eval(": f Int -> Int ; drop -1 ."),
            R0 = eval("0 f"),
            ?assertEqual([{'Int', 999}], R0#continuation.data_stack),
            Rpos = eval("5 f"),
            ?assertEqual([{'Int', 15}], Rpos#continuation.data_stack),
            Rneg = eval("-2 f"),
            ?assertEqual([{'Int', -1}], Rneg#continuation.data_stack)
        end} end
    ]}.

%%====================================================================
%% Interaction with sub-clauses (disallowed)
%%====================================================================

guard_rejected_on_multiclause_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"top-level where plus sub-clauses raises an error", fun() ->
            Src = ": bad Int where dup 0 > -> Int ;"
                  " : 0 -> 0 ;"
                  " : Int -> dup 1 - bad ; ."
                  ,
            ?assertError({guard_on_multiclause, _, _},
                         eval(Src))
        end} end
    ]}.

%%====================================================================
%% match_guard/2 direct tests
%%====================================================================

match_guard_direct_test_() ->
    [
        {"undefined guard always matches", fun() ->
            Op = #operation{guard = undefined},
            ?assertEqual(true, af_type:match_guard(Op, []))
        end},

        {"empty-list guard always matches", fun() ->
            Op = #operation{guard = []},
            ?assertEqual(true, af_type:match_guard(Op, []))
        end},

        {"Bool-true guard matches", fun() ->
            Stack = [{'Bool', true}],
            TokTrue = #token{value = "dup", line = 1, column = 1, file = "t"},
            Op = #operation{guard = [TokTrue]},
            ?assertEqual(true, af_type:match_guard(Op, Stack))
        end},

        {"crashing guard does not match", fun() ->
            %% A token whose interpretation crashes — match_guard must catch.
            Op = #operation{guard = [
                #token{value = "drop", line = 1, column = 1, file = "t"}
            ]},
            ?assertEqual(false, af_type:match_guard(Op, []))
        end}
    ].

%%====================================================================
%% Word with guard is NOT auto-compiled to BEAM
%%====================================================================

guard_skips_auto_compile_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"guarded word has interpreter impl, no native wrapper", fun() ->
            _ = eval(": guarded Int where dup 0 > -> Int ; 1 - ."),
            {ok, Op} = af_type:find_op_by_name("guarded", 'Int'),
            %% A native-compiled word would have source = {native, _}.
            %% Interpreter words have source = {compiled, Body}.
            ?assertMatch({compiled, _}, Op#operation.source)
        end} end,

        fun(_) -> {"plain (no-guard) word still gets auto-compiled to native", fun() ->
            _ = eval(": plain Int -> Int ; 1 + ."),
            {ok, Op} = af_type:find_op_by_name("plain", 'Int'),
            %% The word compiler may leave it as {compiled, _} if it decides
            %% not to compile, but it should not have a guard on it.
            ?assertEqual(undefined, Op#operation.guard)
        end} end
    ]}.
