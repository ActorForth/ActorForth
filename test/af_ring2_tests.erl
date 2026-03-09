-module(af_ring2_tests).

-include_lib("eunit/include/eunit.hrl").
-include("operation.hrl").

setup() ->
    af_type:reset(),
    af_repl:init_types().

%%% === Compilation Tests ===

compile_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile double", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": double Int -> Int ; dup + .", "test", "double"),
            ?assertEqual(af_r2_double, Mod),
            ?assertEqual([{'Int', 42}], Mod:double([{'Int', 21}]))
        end} end,

        fun(_) -> {"compile square", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": square Int -> Int ; dup * .", "test", "square"),
            ?assertEqual([{'Int', 49}], Mod:square([{'Int', 7}]))
        end} end,

        fun(_) -> {"compile with subtraction", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": dec Int -> Int ; 1 - .", "test", "dec"),
            ?assertEqual([{'Int', 4}], Mod:dec([{'Int', 5}]))
        end} end,

        fun(_) -> {"compile with division", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": half Int -> Int ; 2 / .", "test", "half"),
            ?assertEqual([{'Int', 5}], Mod:half([{'Int', 10}]))
        end} end,

        fun(_) -> {"compile with mod", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": remainder Int -> Int ; 3 mod .", "test", "rem"),
            ?assertEqual([{'Int', 1}], Mod:remainder([{'Int', 10}]))
        end} end,

        fun(_) -> {"compile comparison ==", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": is_five Int -> Bool ; 5 == .", "test", "eq"),
            ?assertEqual([{'Bool', true}], Mod:is_five([{'Int', 5}])),
            ?assertEqual([{'Bool', false}], Mod:is_five([{'Int', 3}]))
        end} end,

        fun(_) -> {"compile comparison <", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": lt_ten Int -> Bool ; 10 < .", "test", "lt"),
            ?assertEqual([{'Bool', true}], Mod:lt_ten([{'Int', 5}])),
            ?assertEqual([{'Bool', false}], Mod:lt_ten([{'Int', 15}]))
        end} end,

        fun(_) -> {"compile comparison >", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": gt_ten Int -> Bool ; 10 > .", "test", "gt"),
            ?assertEqual([{'Bool', true}], Mod:gt_ten([{'Int', 15}])),
            ?assertEqual([{'Bool', false}], Mod:gt_ten([{'Int', 5}]))
        end} end,

        fun(_) -> {"compile comparison !=", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": not_five Int -> Bool ; 5 != .", "test", "neq"),
            ?assertEqual([{'Bool', true}], Mod:not_five([{'Int', 3}])),
            ?assertEqual([{'Bool', false}], Mod:not_five([{'Int', 5}]))
        end} end,

        fun(_) -> {"compile comparison <=", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": lte_ten Int -> Bool ; 10 <= .", "test", "lte"),
            ?assertEqual([{'Bool', true}], Mod:lte_ten([{'Int', 5}])),
            ?assertEqual([{'Bool', true}], Mod:lte_ten([{'Int', 10}])),
            ?assertEqual([{'Bool', false}], Mod:lte_ten([{'Int', 15}]))
        end} end,

        fun(_) -> {"compile comparison >=", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": gte_ten Int -> Bool ; 10 >= .", "test", "gte"),
            ?assertEqual([{'Bool', true}], Mod:gte_ten([{'Int', 15}])),
            ?assertEqual([{'Bool', true}], Mod:gte_ten([{'Int', 10}])),
            ?assertEqual([{'Bool', false}], Mod:gte_ten([{'Int', 5}]))
        end} end,

        fun(_) -> {"compile not", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": negate Bool -> Bool ; not .", "test", "not"),
            ?assertEqual([{'Bool', false}], Mod:negate([{'Bool', true}])),
            ?assertEqual([{'Bool', true}], Mod:negate([{'Bool', false}]))
        end} end,

        fun(_) -> {"compile rot", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": myrot Int Int Int -> Int Int Int ; rot .", "test", "rot"),
            ?assertEqual([{'Int', 1}, {'Int', 3}, {'Int', 2}],
                         Mod:myrot([{'Int', 3}, {'Int', 2}, {'Int', 1}]))
        end} end,

        fun(_) -> {"compile over", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": myover Int Int -> Int Int Int ; over .", "test", "over"),
            ?assertEqual([{'Int', 1}, {'Int', 2}, {'Int', 1}],
                         Mod:myover([{'Int', 2}, {'Int', 1}]))
        end} end,

        fun(_) -> {"compile 2dup", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": my2dup Int Int -> Int Int Int Int ; 2dup .", "test", "2dup"),
            ?assertEqual([{'Int', 2}, {'Int', 1}, {'Int', 2}, {'Int', 1}],
                         Mod:my2dup([{'Int', 2}, {'Int', 1}]))
        end} end,

        fun(_) -> {"compile with string literal", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": greeting Any -> String ; drop \"Hello\" .", "test", "greet"),
            ?assertEqual([{'String', <<"Hello">>}],
                         Mod:greeting([{'Int', 0}]))
        end} end,

        fun(_) -> {"compile with float literal", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": pi Any -> Float ; drop 3.14 .", "test", "pi"),
            ?assertEqual([{'Float', 3.14}], Mod:pi([{'Int', 0}]))
        end} end,

        fun(_) -> {"compile with bool literal", fun() ->
            {ok, Mod} = af_ring2:compile(
                ": yes Any -> Bool ; drop true .", "test", "yes"),
            ?assertEqual([{'Bool', true}], Mod:yes([{'Int', 0}]))
        end} end
    ]}.

%%% === Multi-Word Tests ===

multi_word_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"inter-word calls", fun() ->
            Source = ": double Int -> Int ; dup + .\n"
                     ": quadruple Int -> Int ; double double .",
            {ok, Mod} = af_ring2:compile(Source, "test", "multi"),
            ?assertEqual([{'Int', 40}], Mod:quadruple([{'Int', 10}]))
        end} end,

        fun(_) -> {"three word chain", fun() ->
            Source = ": inc Int -> Int ; 1 + .\n"
                     ": inc2 Int -> Int ; inc inc .\n"
                     ": inc4 Int -> Int ; inc2 inc2 .",
            {ok, Mod} = af_ring2:compile(Source, "test", "chain"),
            ?assertEqual([{'Int', 14}], Mod:inc4([{'Int', 10}]))
        end} end
    ]}.

%%% === Pattern Matching Tests ===

pattern_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"value-constrained factorial", fun() ->
            Source = ": factorial 0 Int -> Int ; drop 1 .\n"
                     ": factorial Int -> Int ; dup 1 - factorial * .",
            {ok, Mod} = af_ring2:compile(Source, "test", "fact"),
            ?assertEqual([{'Int', 1}], Mod:factorial([{'Int', 0}])),
            ?assertEqual([{'Int', 120}], Mod:factorial([{'Int', 5}])),
            ?assertEqual([{'Int', 3628800}], Mod:factorial([{'Int', 10}]))
        end} end,

        fun(_) -> {"fibonacci", fun() ->
            Source = ": fib 0 Int -> Int ; drop 0 .\n"
                     ": fib 1 Int -> Int ; drop 1 .\n"
                     ": fib Int -> Int ; dup 1 - fib swap 2 - fib + .",
            {ok, Mod} = af_ring2:compile(Source, "test", "fib"),
            ?assertEqual([{'Int', 0}], Mod:fib([{'Int', 0}])),
            ?assertEqual([{'Int', 1}], Mod:fib([{'Int', 1}])),
            ?assertEqual([{'Int', 55}], Mod:fib([{'Int', 10}]))
        end} end
    ]}.

%%% === Runtime Dispatch Tests ===

runtime_dispatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"concat via runtime dispatch", fun() ->
            Source = ": greet String -> String ; \"Hello, \" swap concat \"!\" concat .",
            {ok, Mod} = af_ring2:compile(Source, "test", "greet2"),
            ?assertEqual([{'String', <<"Hello, World!">>}],
                         Mod:greet([{'String', <<"World">>}]))
        end} end,

        fun(_) -> {"length via runtime dispatch", fun() ->
            Source = ": slen String -> Int ; length .",
            {ok, Mod} = af_ring2:compile(Source, "test", "slen"),
            ?assertEqual([{'Int', 5}], Mod:slen([{'String', <<"hello">>}]))
        end} end
    ]}.

%%% === File Compilation Tests ===

file_compile_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile from file", fun() ->
            {ok, Mod} = af_ring2:compile_file(
                "samples/lib_math.a4", "math_r2"),
            ?assertEqual(af_r2_math_r2, Mod),
            ?assertEqual([{'Int', 25}], Mod:square([{'Int', 5}]))
        end} end,

        fun(_) -> {"compile greet from file", fun() ->
            {ok, Mod} = af_ring2:compile_file(
                "test/selfhost/greet.a4", "greet_r2"),
            ?assertEqual([{'String', <<"Hello, World!">>}],
                         Mod:greet([{'String', <<"World">>}]))
        end} end,

        fun(_) -> {"compile nonexistent file", fun() ->
            Result = af_ring2:compile_file("nonexistent.a4", "bad"),
            ?assertMatch({error, {file_error, _}}, Result)
        end} end
    ]}.

%%% === Translation Unit Tests ===

translate_primitive_test() ->
    %% Test individual primitive translations
    Op = #operation{name = "dup"},
    ?assertEqual([dup], af_ring2:translate_body([Op], [])).

translate_arithmetic_test() ->
    Ops = [#operation{name = "dup"}, #operation{name = "+"}],
    ?assertEqual([dup, add], af_ring2:translate_body(Ops, [])).

translate_comparison_test() ->
    Ops = [#operation{name = "=="}, #operation{name = "not"}],
    ?assertEqual([eq, not_op], af_ring2:translate_body(Ops, [])).

translate_string_literal_test() ->
    Op = #operation{name = "hello", source = quoted_string},
    ?assertEqual([{lit, {'String', <<"hello">>}}],
                 af_ring2:translate_body([Op], [])).

translate_int_literal_test() ->
    Op = #operation{name = "42"},
    ?assertEqual([{lit, {'Int', 42}}], af_ring2:translate_body([Op], [])).

translate_float_literal_test() ->
    Op = #operation{name = "3.14"},
    ?assertEqual([{lit, {'Float', 3.14}}], af_ring2:translate_body([Op], [])).

translate_bool_literal_test() ->
    Op = #operation{name = "true"},
    ?assertEqual([{lit, {'Bool', true}}], af_ring2:translate_body([Op], [])).

translate_word_call_test() ->
    Op = #operation{name = "double"},
    ?assertEqual([{call, "double"}],
                 af_ring2:translate_body([Op], ["double"])).

translate_runtime_dispatch_test() ->
    Op = #operation{name = "concat"},
    ?assertEqual([{apply_impl, "concat"}],
                 af_ring2:translate_body([Op], [])).

translate_rot_test() ->
    Op = #operation{name = "rot"},
    ?assertEqual([to_r, swap, from_r, swap],
                 af_ring2:translate_body([Op], [])).

translate_over_test() ->
    Op = #operation{name = "over"},
    ?assertEqual([to_r, dup, from_r, swap],
                 af_ring2:translate_body([Op], [])).

translate_list_ops_test() ->
    Ops = [#operation{name = "nil"}, #operation{name = "cons"},
           #operation{name = "head"}, #operation{name = "tail"}],
    ?assertEqual([nil, cons, head, tail],
                 af_ring2:translate_body(Ops, [])).

translate_all_comparisons_test() ->
    Ops = [#operation{name = "<"}, #operation{name = ">"},
           #operation{name = "<="}, #operation{name = ">="},
           #operation{name = "!="}],
    Expected = [lt, swap, lt, swap, lt, not_op, lt, not_op, eq, not_op],
    ?assertEqual(Expected, af_ring2:translate_body(Ops, [])).

%%% === Error Cases ===

no_words_test() ->
    af_type:reset(),
    af_repl:init_types(),
    Result = af_ring2:compile("1 2 +", "test", "empty"),
    ?assertEqual({error, no_words_defined}, Result).

%%% === Preserves Stack ===

preserves_stack_test() ->
    setup(),
    {ok, Mod} = af_ring2:compile(
        ": inc Int -> Int ; 1 + .", "test", "pres"),
    ?assertEqual([{'Int', 6}, {'String', <<"below">>}],
                 Mod:inc([{'Int', 5}, {'String', <<"below">>}])).
