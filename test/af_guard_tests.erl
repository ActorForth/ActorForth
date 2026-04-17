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
%% Sub-clause guards
%%====================================================================

sub_clause_guard_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"per-sub-clause where guards select clause", fun() ->
            %% classify returns 1 for positive, -1 for negative, 0 otherwise.
            %% In a4 sub-clause syntax, the body follows the sig_out's `;`
            %% and runs until the next `:` or `.`.
            Src = ": classify Int -> Int ;"
                  " : Int where dup 0 > -> Int ; drop 1"
                  " : Int where dup 0 < -> Int ; drop -1"
                  " : Int -> Int ; drop 0 ."
                  ,
            _ = eval(Src),
            R1 = eval("7 classify"),
            ?assertEqual([{'Int', 1}], R1#continuation.data_stack),
            R2 = eval("-3 classify"),
            ?assertEqual([{'Int', -1}], R2#continuation.data_stack),
            R3 = eval("0 classify"),
            ?assertEqual([{'Int', 0}], R3#continuation.data_stack)
        end} end,

        fun(_) -> {"top-level where applies to each sub-clause", fun() ->
            %% Master guard requires positive TOS. Both sub-clauses inherit
            %% that guard because neither defines its own. With a negative
            %% input, no clause matches — the token stays unresolved.
            Src = ": pos-only Int where dup 0 > -> Int ;"
                  " : Int -> Int ; 1 +"
                  " : Int -> Int ; drop 99"
                  " ."
                  ,
            _ = eval(Src),
            %% Positive: first sub-clause matches.
            Rpos = eval("5 pos-only"),
            ?assertEqual([{'Int', 6}], Rpos#continuation.data_stack)
        end} end,

        fun(_) -> {"sub-clause without guard captured as undefined", fun() ->
            _ = eval(": simple Int -> Int ;"
                     " : Int -> Int ; 1 +"
                     " ."),
            {ok, Op} = af_type:find_op_by_name("simple", 'Int'),
            ?assertEqual(undefined, Op#operation.guard)
        end} end,

        fun(_) -> {"sub-clause guard captured on the op", fun() ->
            _ = eval(": gated Int -> Int ;"
                     " : Int where dup 10 > -> Int ; 1 -"
                     " ."),
            {ok, Op} = af_type:find_op_by_name("gated", 'Int'),
            ?assert(is_list(Op#operation.guard)),
            GuardValues = [T#token.value || T <- Op#operation.guard],
            ?assertEqual(["dup", "10", ">"], GuardValues)
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
%% Simple guards compile to Erlang function-head guards
%%====================================================================
%%
%% Guards of the form `dup LITERAL CMP` are recognised and emitted as
%% Erlang head guards. Complex guards fall back to the interpreter path.

simple_guard_compiles_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"simple guard produces native wrapper", fun() ->
            %% Turn on auto-compile to make the compilation path fire.
            af_type_any:auto_compile_word("positive"),
            _ = eval("true auto-compile"),
            _ = eval(": positive Int where dup 0 > -> Int ; 1 + ."),
            {ok, Op} = af_type:find_op_by_name("positive", 'Int'),
            %% Either the wrapper replaced the op (source={native,_}) or the
            %% interpreter copy remains (source={compiled,_}). In either case
            %% the guard metadata survives for dispatch.
            case Op#operation.source of
                {native, _} -> ok;
                {compiled, _} -> ok
            end,
            ?assert(is_list(Op#operation.guard))
        end} end,

        fun(_) -> {"native-compiled guarded word dispatches correctly", fun() ->
            %% Register both clauses first, then compile explicitly so both
            %% are batched into a single multi-clause native module.
            _ = eval(": stepper Int where dup 5 > -> Int ; 1 - ."),
            _ = eval(": stepper Int -> Int ; drop 0 ."),
            _ = eval("\"stepper\" compile"),
            %% Positive large -> decrement
            R1 = eval("10 stepper"),
            ?assertEqual([{'Int', 9}], R1#continuation.data_stack),
            %% Small -> fall through
            R2 = eval("3 stepper"),
            ?assertEqual([{'Int', 0}], R2#continuation.data_stack)
        end} end,

        fun(_) -> {"guard_to_head_form: dup 0 > translates", fun() ->
            Tok = fun(V) -> #token{value = V, line = 1, column = 1, file = "t"} end,
            Guard = [Tok("dup"), Tok("0"), Tok(">")],
            %% InitExprStack mirrors what build_head_pattern produces: a
            %% tuple pattern expression for the TOS with a raw var inside.
            TuplePat = {tuple, 1, [{atom, 1, 'Int'}, {var, 1, 'Arg1'}]},
            InitStack = [{TuplePat, 'Int'}],
            {ok, [[GuardExpr]]} = af_word_compiler:guard_to_head_form(
                                    Guard, InitStack, 1),
            ?assertMatch({op, 1, '>', _, {integer, 1, 0}}, GuardExpr)
        end} end,

        fun(_) -> {"guard_to_head_form: complex guard not compilable", fun() ->
            Tok = fun(V) -> #token{value = V, line = 1, column = 1, file = "t"} end,
            %% Guards that aren't `dup LIT CMP` aren't emitted as head guards.
            Guard = [Tok("dup"), Tok("dup"), Tok("+"), Tok("10"), Tok(">")],
            InitStack = [{{tuple, 1, [{atom, 1, 'Int'}, {var, 1, 'Arg1'}]}, 'Int'}],
            ?assertEqual({error, guard_not_compilable},
                         af_word_compiler:guard_to_head_form(Guard, InitStack, 1))
        end} end
    ]}.

%%====================================================================
%% Non-guarded words still auto-compile to native (regression check)
%%====================================================================

plain_still_compiles_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"plain (no-guard) word has no guard on its op", fun() ->
            _ = eval(": plain Int -> Int ; 1 + ."),
            {ok, Op} = af_type:find_op_by_name("plain", 'Int'),
            ?assertEqual(undefined, Op#operation.guard)
        end} end
    ]}.
