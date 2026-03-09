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
    Tokens = af_r0_parser:parse(<<": f String -> Int ; length .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{apply_impl, "length"}], Body).

%%% === Empty ===

no_words_test() ->
    Tokens = af_r0_parser:parse(<<"1 2 +">>, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([], Defs).

empty_body_test() ->
    Tokens = af_r0_parser:parse(<<": f Int -> Int ; .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([], Body).
