-module(af_ring2_tests).

-include_lib("eunit/include/eunit.hrl").
-include("operation.hrl").

setup() ->
    af_type:reset().

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
    %% concat is now a Ring 0 primitive (str_concat)
    Op = #operation{name = "concat"},
    ?assertEqual([str_concat],
                 af_ring2:translate_body([Op], [])).

translate_unknown_dispatch_test() ->
    %% Unknown ops still go through apply_impl
    Op = #operation{name = "some-unknown-op"},
    ?assertEqual([{apply_impl, "some-unknown-op"}],
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
    Result = af_ring2:compile("1 2 +", "test", "empty"),
    ?assertEqual({error, no_words_defined}, Result).

%%% === Preserves Stack ===

preserves_stack_test() ->
    setup(),
    {ok, Mod} = af_ring2:compile(
        ": inc Int -> Int ; 1 + .", "test", "pres"),
    ?assertEqual([{'Int', 6}, {'String', <<"below">>}],
                 Mod:inc([{'Int', 5}, {'String', <<"below">>}])).

%%% === Self-Hosted Parser Tests ===

selfhosted_parse_simple_test() ->
    Tokens = af_r0_parser:parse(<<"dup + 42">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"dup">>, <<"+">>, <<"42">>], Values).

selfhosted_parse_word_def_test() ->
    Tokens = af_r0_parser:parse(<<": double Int -> Int ; dup + .">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<":">>, <<"double">>, <<"Int">>, <<"->">>,
                  <<"Int">>, <<";">>, <<"dup">>, <<"+">>, <<".">>], Values).

selfhosted_parse_quoted_string_test() ->
    Tokens = af_r0_parser:parse(<<"\"hello world\"">>, <<"test">>),
    ?assertEqual(1, length(Tokens)),
    T = hd(Tokens),
    ?assertEqual(<<"hello world">>, maps:get(value, T)),
    ?assertEqual(true, maps:get(quoted, T)).

selfhosted_parse_comment_test() ->
    Tokens = af_r0_parser:parse(<<"a # comment\nb">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>, <<"b">>], Values).

selfhosted_parse_float_test() ->
    Tokens = af_r0_parser:parse(<<"3.14">>, <<"test">>),
    ?assertEqual(1, length(Tokens)),
    ?assertEqual(<<"3.14">>, maps:get(value, hd(Tokens))).

selfhosted_parse_dot_delim_test() ->
    Tokens = af_r0_parser:parse(<<"foo .">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"foo">>, <<".">>], Values).

selfhosted_parse_lines_test() ->
    Tokens = af_r0_parser:parse(<<"a\nb\nc">>, <<"test">>),
    Lines = [maps:get(line, T) || T <- Tokens],
    ?assertEqual([1, 2, 3], Lines).

selfhosted_parse_empty_test() ->
    Tokens = af_r0_parser:parse(<<>>, <<"test">>),
    ?assertEqual([], Tokens).

%%% === Self-Hosted Compilation Tests ===

selfhosted_compile_test_() ->
    [{Description, fun() -> Body() end} || {Description, Body} <- [
        {"selfhosted double", fun() ->
            {ok, Mod} = af_ring2:compile_selfhosted(
                ": double Int -> Int ; dup + .", "test", "sh_double"),
            ?assertEqual(af_r2_sh_double, Mod),
            ?assertEqual([{'Int', 42}], Mod:double([{'Int', 21}]))
        end},

        {"selfhosted square", fun() ->
            {ok, Mod} = af_ring2:compile_selfhosted(
                ": square Int -> Int ; dup * .", "test", "sh_square"),
            ?assertEqual([{'Int', 49}], Mod:square([{'Int', 7}]))
        end},

        {"selfhosted subtraction", fun() ->
            {ok, Mod} = af_ring2:compile_selfhosted(
                ": dec Int -> Int ; 1 - .", "test", "sh_dec"),
            ?assertEqual([{'Int', 4}], Mod:dec([{'Int', 5}]))
        end},

        {"selfhosted comparison", fun() ->
            {ok, Mod} = af_ring2:compile_selfhosted(
                ": is_five Int -> Bool ; 5 == .", "test", "sh_eq"),
            ?assertEqual([{'Bool', true}], Mod:is_five([{'Int', 5}])),
            ?assertEqual([{'Bool', false}], Mod:is_five([{'Int', 3}]))
        end},

        {"selfhosted string literal", fun() ->
            {ok, Mod} = af_ring2:compile_selfhosted(
                ": greeting Any -> String ; drop \"Hello\" .", "test", "sh_greet"),
            ?assertEqual([{'String', <<"Hello">>}],
                         Mod:greeting([{'Int', 0}]))
        end},

        {"selfhosted inter-word calls", fun() ->
            Source = ": double Int -> Int ; dup + .\n"
                     ": quadruple Int -> Int ; double double .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_multi"),
            ?assertEqual([{'Int', 40}], Mod:quadruple([{'Int', 10}]))
        end},

        {"selfhosted bool literal", fun() ->
            {ok, Mod} = af_ring2:compile_selfhosted(
                ": yes Any -> Bool ; drop true .", "test", "sh_yes"),
            ?assertEqual([{'Bool', true}], Mod:yes([{'Int', 0}]))
        end},

        {"selfhosted float literal", fun() ->
            {ok, Mod} = af_ring2:compile_selfhosted(
                ": pi Any -> Float ; drop 3.14 .", "test", "sh_pi"),
            ?assertEqual([{'Float', 3.14}], Mod:pi([{'Int', 0}]))
        end},

        {"selfhosted pattern matching factorial", fun() ->
            Source = ": factorial 0 Int -> Int ; drop 1 .\n"
                     ": factorial Int -> Int ; dup 1 - factorial * .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_fact"),
            ?assertEqual([{'Int', 1}], Mod:factorial([{'Int', 0}])),
            ?assertEqual([{'Int', 120}], Mod:factorial([{'Int', 5}]))
        end},

        {"selfhosted fibonacci", fun() ->
            Source = ": fib 0 Int -> Int ; drop 0 .\n"
                     ": fib 1 Int -> Int ; drop 1 .\n"
                     ": fib Int -> Int ; dup 1 - fib swap 2 - fib + .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_fib"),
            ?assertEqual([{'Int', 0}], Mod:fib([{'Int', 0}])),
            ?assertEqual([{'Int', 1}], Mod:fib([{'Int', 1}])),
            ?assertEqual([{'Int', 55}], Mod:fib([{'Int', 10}]))
        end},

        {"selfhosted no words", fun() ->
            Result = af_ring2:compile_selfhosted("1 2 +", "test", "sh_empty"),
            ?assertEqual({error, no_words_defined}, Result)
        end},

        {"selfhosted file compilation", fun() ->
            {ok, Mod} = af_ring2:compile_file_selfhosted(
                "samples/lib_math.a4", "sh_math"),
            ?assertEqual(af_r2_sh_math, Mod),
            ?assertEqual([{'Int', 25}], Mod:square([{'Int', 5}]))
        end},

        {"selfhosted product getter", fun() ->
            Source = "type Point\n  x Int\n  y Int\n.\n"
                     ": get-x Point -> Int Point ; x .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_prod"),
            %% Flat tuple: {'Point', x_val, y_val}.
            Instance = {'Point', {'Int', 3}, {'Int', 4}},
            Result = Mod:'get-x'([Instance]),
            ?assertEqual([{'Int', 3}, Instance], Result)
        end},

        {"selfhosted product setter", fun() ->
            Source = "type Counter\n  count Int\n.\n"
                     ": set-count Int Counter -> Counter ; swap count! .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_set"),
            Instance = {'Counter', {'Int', 0}},
            %% Rightmost in source = TOS: Counter is TOS, Int below
            [Updated] = Mod:'set-count'([Instance, {'Int', 99}]),
            ?assertEqual('Counter', element(1, Updated)),
            ?assertEqual({'Int', 99}, element(2, Updated))
        end},

        {"selfhosted product constructor", fun() ->
            Source = "type Counter\n  count Int\n.\n"
                     ": make-counter Int -> Counter ; counter .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_ctor"),
            [Instance] = Mod:'make-counter'([{'Int', 0}]),
            ?assertEqual('Counter', element(1, Instance)),
            ?assertEqual({'Int', 0}, element(2, Instance))
        end},

        {"selfhosted sub-clause fib", fun() ->
            Source = ": fib Int -> Int ;\n"
                     "    : 0 -> 0 ;\n"
                     "    : 1 -> 1 ;\n"
                     "    : Int -> Int ;\n"
                     "        dup 1 - fib swap 2 - fib + .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_subcl"),
            ?assertEqual([{'Int', 0}], Mod:fib([{'Int', 0}])),
            ?assertEqual([{'Int', 1}], Mod:fib([{'Int', 1}])),
            ?assertEqual([{'Int', 55}], Mod:fib([{'Int', 10}]))
        end},

        {"selfhosted sub-clause countdown", fun() ->
            Source = ": countdown Int -> Int ;\n"
                     "    : 0 -> 0 ;\n"
                     "    : Int -> Int ;\n"
                     "        1 - countdown .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_cd"),
            ?assertEqual([{'Int', 0}], Mod:countdown([{'Int', 5}]))
        end},

        {"selfhosted load file", fun() ->
            %% lib_math.a4 defines square, we load it and use it
            Source = "load \"samples/lib_math.a4\"\n"
                     ": cube Int -> Int ; dup square * .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_load"),
            ?assertEqual([{'Int', 125}], Mod:cube([{'Int', 5}]))
        end},

        {"selfhosted preserves stack", fun() ->
            {ok, Mod} = af_ring2:compile_selfhosted(
                ": inc Int -> Int ; 1 + .", "test", "sh_pres"),
            ?assertEqual([{'Int', 6}, {'String', <<"below">>}],
                         Mod:inc([{'Int', 5}, {'String', <<"below">>}]))
        end},

        {"selfhosted sub-clause bool dispatch", fun() ->
            Source = ": to-int Bool -> Int ;\n"
                     "    : True -> Int ; drop 1\n"
                     "    : False -> Int ; drop 0 .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_bool"),
            ?assertEqual([{'Int', 1}], Mod:'to-int'([{'Bool', true}])),
            ?assertEqual([{'Int', 0}], Mod:'to-int'([{'Bool', false}]))
        end},

        {"selfhosted sub-clause string dispatch", fun() ->
            Source = ": greet String -> String ;\n"
                     "    : \"hello\" -> String ; drop \"Hello World!\"\n"
                     "    : \"goodbye\" -> String ; drop \"Farewell!\"\n"
                     "    : String -> String ; .",
            {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_str"),
            ?assertEqual([{'String', <<"Hello World!">>}],
                         Mod:greet([{'String', <<"hello">>}])),
            ?assertEqual([{'String', <<"Farewell!">>}],
                         Mod:greet([{'String', <<"goodbye">>}])),
            ?assertEqual([{'String', <<"hey">>}],
                         Mod:greet([{'String', <<"hey">>}]))
        end},

        {"selfhosted all sample files compile", fun() ->
            Files = [
                "samples/square.a4", "samples/lib_math.a4",
                "samples/fib.a4", "samples/countdown.a4",
                "samples/func.a4", "samples/pattern_demo.a4",
                "samples/string_demo.a4", "samples/map_demo.a4",
                "samples/testloop.a4", "samples/testloop2.a4"
            ],
            lists:foreach(fun(F) ->
                Mod = filename:basename(F, ".a4"),
                Result = af_ring2:compile_file_selfhosted(F, Mod),
                ?assertMatch({ok, _}, Result)
            end, Files)
        end}
    ]].

%%% === Translate Primitive: Logic Ops ===

translate_logic_ops_test() ->
    Ops = [#operation{name = "and"}, #operation{name = "or"}],
    ?assertEqual([and_op, or_op], af_ring2:translate_body(Ops, [])).

%%% === Translate Primitive: Extended List Ops ===

translate_extended_list_ops_test() ->
    Ops = [#operation{name = "length"}, #operation{name = "append"},
           #operation{name = "reverse"}, #operation{name = "nth"},
           #operation{name = "last"}, #operation{name = "take"},
           #operation{name = "empty?"}, #operation{name = "contains?"},
           #operation{name = "flatten"}, #operation{name = "zip"}],
    ?assertEqual([generic_len, list_append, list_reverse, list_nth,
                  list_last, list_take, list_empty, list_contains,
                  list_flatten, list_zip],
                 af_ring2:translate_body(Ops, [])).

%%% === Translate Primitive: Map Ops ===

translate_map_ops_test() ->
    Ops = [#operation{name = "map-new"}, #operation{name = "map-put"},
           #operation{name = "map-get"}, #operation{name = "map-delete"},
           #operation{name = "map-keys"}, #operation{name = "map-has?"},
           #operation{name = "map-values"}, #operation{name = "map-size"},
           #operation{name = "map-merge"}, #operation{name = "map-get-or"}],
    ?assertEqual([map_new, map_put, map_get, map_delete,
                  map_keys, map_has, map_values, map_size,
                  map_merge, map_get_or],
                 af_ring2:translate_body(Ops, [])).

%%% === Translate Primitive: String Ops ===

translate_string_ops_test() ->
    Ops = [#operation{name = "split"}, #operation{name = "contains"},
           #operation{name = "starts-with"}, #operation{name = "ends-with"},
           #operation{name = "trim"}, #operation{name = "to-upper"},
           #operation{name = "to-lower"}, #operation{name = "substring"},
           #operation{name = "replace"}, #operation{name = "str-byte-at"}],
    ?assertEqual([str_split, str_contains, str_starts_with, str_ends_with,
                  str_trim, str_upper, str_lower, str_substring,
                  str_replace, str_byte_at],
                 af_ring2:translate_body(Ops, [])).

translate_str_nth_test() ->
    Op = #operation{name = "str-nth"},
    ?assertEqual([str_nth], af_ring2:translate_body([Op], [])).

%%% === Translate Primitive: Conversion Ops ===

translate_conversion_ops_test() ->
    Ops = [#operation{name = "to-string"}, #operation{name = "to-int"},
           #operation{name = "to-float"}, #operation{name = "to-atom"}],
    ?assertEqual([to_string, to_int, to_float, to_atom],
                 af_ring2:translate_body(Ops, [])).

%%% === Translate Primitive: Tuple Ops ===

translate_tuple_ops_test() ->
    Ops = [#operation{name = "make-tuple"}, #operation{name = "from-tuple"},
           #operation{name = "tuple-size"}, #operation{name = "ok-tuple"},
           #operation{name = "error-tuple"}, #operation{name = "is-ok"},
           #operation{name = "unwrap-ok"}],
    ?assertEqual([tuple_make, tuple_to_list, tuple_size_op,
                  ok_tuple, error_tuple, is_ok, unwrap_ok],
                 af_ring2:translate_body(Ops, [])).

%%% === Translate Primitive: I/O Ops ===

translate_io_ops_test() ->
    Ops = [#operation{name = "print"}, #operation{name = "stack"},
           #operation{name = "assert"}, #operation{name = "assert-eq"}],
    ?assertEqual([print_tos, print_stack, assert_true, assert_eq],
                 af_ring2:translate_body(Ops, [])).

%%% === Translate Primitive: File Ops ===

translate_file_ops_test() ->
    Ops = [#operation{name = "read-file"}, #operation{name = "write-file"},
           #operation{name = "file-exists?"}],
    ?assertEqual([file_read, file_write, file_exists],
                 af_ring2:translate_body(Ops, [])).

%%% === Translate Primitive: FFI Ops ===

translate_ffi_ops_test() ->
    Ops = [#operation{name = "erlang-apply"}, #operation{name = "erlang-apply0"},
           #operation{name = "erlang-call"}, #operation{name = "erlang-call0"},
           #operation{name = "erlang-new"}],
    ?assertEqual([{apply_impl, "erlang-apply"}, {apply_impl, "erlang-apply0"},
                  {apply_impl, "erlang-call"}, {apply_impl, "erlang-call0"},
                  {apply_impl, "erlang-new"}],
                 af_ring2:translate_body(Ops, [])).

%%% === Translate Primitive: Actor Ops ===

translate_actor_ops_test() ->
    Ops = [#operation{name = "spawn"}, #operation{name = "send"},
           #operation{name = "!"}, #operation{name = "receive"},
           #operation{name = "receive-timeout"}],
    ?assertEqual([spawn_actor, send_msg, send_msg, receive_msg, receive_timeout],
                 af_ring2:translate_body(Ops, [])).

%%% === try_literal: false case ===

translate_false_literal_test() ->
    Op = #operation{name = "false"},
    ?assertEqual([{lit, {'Bool', false}}], af_ring2:translate_body([Op], [])).

%%% === to_str/to_bin with atom inputs ===

to_str_atom_test() ->
    %% to_str is exercised indirectly through compile with atom module name
    %% Verify atom module name works in compile
    af_type:reset(),
    {ok, Mod} = af_ring2:compile(
        ": double Int -> Int ; dup + .", test, atom_mod),
    ?assertEqual(af_r2_atom_mod, Mod),
    ?assertEqual([{'Int', 10}], Mod:double([{'Int', 5}])).

to_bin_atom_test() ->
    %% to_bin is exercised indirectly through compile_selfhosted with atom args
    {ok, Mod} = af_ring2:compile_selfhosted(
        ": triple Int -> Int ; dup dup + + .", test, sh_atom_mod),
    ?assertEqual(af_r2_sh_atom_mod, Mod),
    ?assertEqual([{'Int', 15}], Mod:triple([{'Int', 5}])).

%%% === End-to-End Selfhosted: Logic Ops ===

selfhosted_logic_ops_test_() ->
    [{"selfhosted and/or ops", fun() ->
        Source = ": both-true Bool Bool -> Bool ; and .\n"
                 ": either-true Bool Bool -> Bool ; or .",
        {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_logic"),
        ?assertEqual([{'Bool', true}], Mod:'both-true'([{'Bool', true}, {'Bool', true}])),
        ?assertEqual([{'Bool', false}], Mod:'both-true'([{'Bool', false}, {'Bool', true}])),
        ?assertEqual([{'Bool', true}], Mod:'either-true'([{'Bool', true}, {'Bool', false}])),
        ?assertEqual([{'Bool', false}], Mod:'either-true'([{'Bool', false}, {'Bool', false}]))
    end}].

%%% === End-to-End Selfhosted: Map Ops ===

selfhosted_map_ops_test_() ->
    [{"selfhosted map operations", fun() ->
        Source = ": make-map Any -> Map ; drop map-new .\n"
                 ": map-count Map -> Int Map ; dup map-size .",
        {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_map"),
        [{'Map', M}] = Mod:'make-map'([{'Int', 0}]),
        ?assertEqual(#{}, M),
        Result = Mod:'map-count'([{'Map', #{}}]),
        ?assertEqual([{'Int', 0}, {'Map', #{}}], Result)
    end}].

%%% === End-to-End Selfhosted: List Ops ===

selfhosted_list_ops_test_() ->
    [{"selfhosted list operations", fun() ->
        Source = ": make-empty Any -> List ; drop nil .\n"
                 ": prepend Any List -> List ; swap cons .",
        {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_list"),
        [{'List', L1}] = Mod:'make-empty'([{'Int', 0}]),
        ?assertEqual([], L1),
        [{'List', L2}] = Mod:prepend([{'List', []}, {'Int', 42}]),
        ?assertEqual([{'Int', 42}], L2)
    end}].

%%% === End-to-End Selfhosted: String Ops ===

selfhosted_string_ops_test_() ->
    [{"selfhosted string operations", fun() ->
        Source = ": shout String -> String ; to-upper .\n"
                 ": whisper String -> String ; to-lower .\n"
                 ": clean String -> String ; trim .",
        {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_strop"),
        ?assertEqual([{'String', <<"HELLO">>}],
                     Mod:shout([{'String', <<"hello">>}])),
        ?assertEqual([{'String', <<"hello">>}],
                     Mod:whisper([{'String', <<"HELLO">>}])),
        ?assertEqual([{'String', <<"hello">>}],
                     Mod:clean([{'String', <<"  hello  ">>}]))
    end}].

%%% === merge_multi_clause direct test ===

merge_multi_clause_single_test() ->
    %% Single clause should pass through unchanged
    Defs = [{"foo", [{'Int'}], [{'Int'}], [add]}],
    %% Use compile_selfhosted which calls merge_multi_clause internally
    %% Instead, test via translate_body + compile round-trip
    Source = ": single Int -> Int ; 1 + .",
    {ok, Mod} = af_ring2:compile_selfhosted(Source, "test", "sh_single_mc"),
    ?assertEqual([{'Int', 6}], Mod:single([{'Int', 5}])).
