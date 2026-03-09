-module(af_r0_compiler_tests).

-include_lib("eunit/include/eunit.hrl").

%%% === Token Compilation ===

compile_simple_word_test() ->
    Tokens = af_r0_parser:parse(<<": double Int -> Int ; dup + .">>, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(1, length(Defs)),
    {Name, SigIn, SigOut, Body} = hd(Defs),
    ?assertEqual("double", Name),
    ?assertEqual(['Int'], SigIn),
    ?assertEqual(['Int'], SigOut),
    ?assertEqual([dup, add], Body).

compile_with_literal_test() ->
    Tokens = af_r0_parser:parse(<<": inc Int -> Int ; 1 + .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{lit, {'Int', 1}}, add], Body).

compile_with_string_test() ->
    Tokens = af_r0_parser:parse(<<": greet Any -> String ; drop \"Hello\" .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([drop, {lit, {'String', <<"Hello">>}}], Body).

compile_with_bool_test() ->
    Tokens = af_r0_parser:parse(<<": yes Any -> Bool ; drop true .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([drop, {lit, {'Bool', true}}], Body).

compile_with_float_test() ->
    Tokens = af_r0_parser:parse(<<": pi Any -> Float ; drop 3.14 .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([drop, {lit, {'Float', 3.14}}], Body).

%%% === Multi-Word ===

compile_multi_word_test() ->
    Src = <<": inc Int -> Int ; 1 + .\n: dec Int -> Int ; 1 - .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(2, length(Defs)),
    Names = [N || {N, _, _, _} <- Defs],
    ?assertEqual(["inc", "dec"], Names).

%%% === Signature Parsing ===

value_constraint_int_test() ->
    Tokens = af_r0_parser:parse(<<": f 0 Int -> Int ; drop 1 .">>, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'Int', 0}], SigIn).

multi_input_sig_test() ->
    Tokens = af_r0_parser:parse(<<": f Int Int -> Int ; + .">>, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(['Int', 'Int'], SigIn).

any_sig_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; .">>, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(['Any'], SigIn).

multi_output_sig_test() ->
    Tokens = af_r0_parser:parse(<<": f Int -> Int Int ; dup .">>, <<"test">>),
    [{_, _, SigOut, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(['Int', 'Int'], SigOut).

%%% === Pattern Matching ===

multi_clause_test() ->
    Src = <<": f 0 Int -> Int ; drop 1 .\n: f Int -> Int ; dup * .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(2, length(Defs)),
    [{_, SigIn1, _, _}, {_, SigIn2, _, _}] = Defs,
    ?assertEqual([{'Int', 0}], SigIn1),
    ?assertEqual(['Int'], SigIn2).

%%% === Primitive Translation ===

all_stack_ops_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any Any -> Any Any Any ; dup drop swap rot over .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([dup, drop, swap, to_r, swap, from_r, swap, to_r, dup, from_r, swap], Body).

all_arith_ops_test() ->
    Tokens = af_r0_parser:parse(<<": f Int Int -> Int ; + - * / mod .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([add, sub, mul, divop, modop], Body).

all_comparison_ops_test() ->
    Tokens = af_r0_parser:parse(<<": f Int Int -> Bool ; == != < > <= >= .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([eq, eq, not_op, lt, swap, lt, swap, lt, not_op, lt, not_op], Body).

list_ops_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> List ; nil swap cons .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([nil, swap, cons], Body).

map_ops_test() ->
    Tokens = af_r0_parser:parse(<<": f -> Map ; map-new .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_new], Body).

%%% === Unknown Ops ===

unknown_op_becomes_apply_impl_test() ->
    %% length is now a primitive (generic_len), use truly unknown op
    Tokens = af_r0_parser:parse(<<": f String -> Int ; some-unknown-op .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{apply_impl, "some-unknown-op"}], Body).

length_is_primitive_test() ->
    Tokens = af_r0_parser:parse(<<": f String -> Int ; length .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([generic_len], Body).

%%% === Empty ===

no_words_test() ->
    Tokens = af_r0_parser:parse(<<"1 2 +">>, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([], Defs).

empty_body_test() ->
    Tokens = af_r0_parser:parse(<<": f Int -> Int ; .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([], Body).

%%% === Sub-Clause Compilation ===

sub_clause_int_test() ->
    Src = <<": f Int -> Int ;\n"
            "    : 0 -> 0 ;\n"
            "    : 1 -> 1 ;\n"
            "    : Int -> Int ; dup + .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(1, length(Defs)),
    {_, SigIn, _, Body} = hd(Defs),
    ?assertEqual(['Int'], SigIn),
    %% Body should contain select_clause
    ?assertMatch([{lit, {'List', _}}, select_clause], Body).

sub_clause_bool_test() ->
    Src = <<": f Bool -> Int ;\n"
            "    : True -> Int ; drop 1\n"
            "    : False -> Int ; drop 0 .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    [{lit, {'List', Clauses}}, select_clause] = Body,
    ?assertEqual(2, length(Clauses)),
    [{SigIn1, _}, {SigIn2, _}] = Clauses,
    ?assertEqual([{'Bool', true}], SigIn1),
    ?assertEqual([{'Bool', false}], SigIn2).

sub_clause_string_test() ->
    Src = <<": f String -> String ;\n"
            "    : \"hello\" -> String ; drop \"world\"\n"
            "    : String -> String ; .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    [{lit, {'List', Clauses}}, select_clause] = Body,
    ?assertEqual(2, length(Clauses)),
    [{SigIn1, _}, {SigIn2, _}] = Clauses,
    ?assertEqual([{'String', <<"hello">>}], SigIn1),
    ?assertEqual(['String'], SigIn2).

%%% === Product Type Compilation ===

product_type_test() ->
    Src = <<"type Point\n  x Int\n  y Int\n.">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    Names = [N || {N, _, _, _} <- Defs],
    ?assert(lists:member("point", Names)),
    ?assert(lists:member("x", Names)),
    ?assert(lists:member("y", Names)),
    ?assert(lists:member("x!", Names)),
    ?assert(lists:member("y!", Names)).

product_constructor_sig_test() ->
    Src = <<"type Point\n  x Int\n  y Int\n.">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    {_, SigIn, SigOut, Body} = lists:keyfind("point", 1, Defs),
    ?assertEqual(['Int', 'Int'], SigIn),
    ?assertEqual(['Point'], SigOut),
    ?assertMatch([{product_new, 'Point', [x, y]}], Body).

%%% === Value Constraint Inference ===

value_constraint_infer_int_test() ->
    %% Bare integer in sub-clause sig followed by -> should infer Int type
    Src = <<": f Int -> Int ;\n"
            "    : 0 -> 0 ;\n"
            "    : Int -> Int ; .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    [{lit, {'List', Clauses}}, select_clause] = Body,
    [{SigIn1, _}, _] = Clauses,
    ?assertEqual([{'Int', 0}], SigIn1).

value_constraint_bool_infer_test() ->
    %% Bool value followed by -> should infer Bool type
    Src = <<": f Bool -> Int ;\n"
            "    : True -> Int ; drop 1\n"
            "    : False -> Int ; drop 0 .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    [{lit, {'List', Clauses}}, select_clause] = Body,
    [{SigIn1, _}, {SigIn2, _}] = Clauses,
    ?assertEqual([{'Bool', true}], SigIn1),
    ?assertEqual([{'Bool', false}], SigIn2).
