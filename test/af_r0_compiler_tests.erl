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
    ?assertMatch([{product_new, 'Point', 2}], Body).

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

%%% === Logic Ops Translation ===

logic_and_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Bool Bool -> Bool ; and .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([and_op], Body).

logic_or_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Bool Bool -> Bool ; or .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([or_op], Body).

logic_not_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Bool -> Bool ; not .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([not_op], Body).

%%% === Map Ops Translation ===

map_put_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Map String Any -> Map ; map-put .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_put], Body).

map_get_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Map String -> Any ; map-get .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_get], Body).

map_delete_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Map String -> Map ; map-delete .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_delete], Body).

map_has_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Map String -> Bool ; map-has? .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_has], Body).

map_values_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Map -> List ; map-values .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_values], Body).

map_size_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Map -> Int ; map-size .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_size], Body).

map_merge_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Map Map -> Map ; map-merge .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_merge], Body).

map_get_or_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Map String Any -> Any ; map-get-or .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_get_or], Body).

%%% === String Ops Translation ===

string_split_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String String -> List ; split .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_split], Body).

string_contains_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String String -> Bool ; contains .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_contains], Body).

string_starts_with_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String String -> Bool ; starts-with .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_starts_with], Body).

string_ends_with_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String String -> Bool ; ends-with .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_ends_with], Body).

string_trim_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String -> String ; trim .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_trim], Body).

string_to_upper_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String -> String ; to-upper .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_upper], Body).

string_to_lower_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String -> String ; to-lower .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_lower], Body).

string_substring_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String Int Int -> String ; substring .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_substring], Body).

string_replace_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String String String -> String ; replace .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_replace], Body).

string_nth_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String Int -> String ; str-nth .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_nth], Body).

string_byte_at_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String Int -> Int ; str-byte-at .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_byte_at], Body).

string_concat_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String String -> String ; concat .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([str_concat], Body).

%%% === Conversion Ops Translation ===

to_string_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Int -> String ; to-string .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([to_string], Body).

to_int_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String -> Int ; to-int .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([to_int], Body).

to_float_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Int -> Float ; to-float .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([to_float], Body).

to_atom_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String -> Any ; to-atom .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([to_atom], Body).

%%% === Tuple Ops Translation ===

make_tuple_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List -> Any ; make-tuple .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([tuple_make], Body).

from_tuple_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> List ; from-tuple .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([tuple_to_list], Body).

tuple_size_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Int ; tuple-size .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([tuple_size_op], Body).

ok_tuple_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; ok-tuple .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([ok_tuple], Body).

error_tuple_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; error-tuple .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([error_tuple], Body).

is_ok_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Bool ; is-ok .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([is_ok], Body).

unwrap_ok_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; unwrap-ok .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([unwrap_ok], Body).

%%% === I/O Ops Translation ===

print_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; print .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([print_tos], Body).

stack_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; stack .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([print_stack], Body).

assert_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Bool -> Bool ; assert .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([assert_true], Body).

assert_eq_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any -> Any ; assert-eq .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([assert_eq], Body).

%%% === File Ops Translation ===

read_file_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String -> String ; read-file .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([file_read], Body).

write_file_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String String -> Any ; write-file .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([file_write], Body).

file_exists_op_test() ->
    Tokens = af_r0_parser:parse(<<": f String -> Bool ; file-exists? .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([file_exists], Body).

%%% === FFI Ops Translation ===

erlang_apply_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any Any -> Any ; erlang-apply .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{apply_impl, "erlang-apply"}], Body).

erlang_apply0_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any -> Any ; erlang-apply0 .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{apply_impl, "erlang-apply0"}], Body).

erlang_call_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any Int -> Any ; erlang-call .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{apply_impl, "erlang-call"}], Body).

erlang_call0_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any -> Any ; erlang-call0 .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{apply_impl, "erlang-call0"}], Body).

erlang_new_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; erlang-new .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{apply_impl, "erlang-new"}], Body).

%%% === Actor Ops Translation ===

spawn_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; spawn .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([spawn_actor], Body).

send_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any -> Any ; send .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([send_msg], Body).

bang_send_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any -> Any ; ! .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([send_msg], Body).

receive_op_test() ->
    Tokens = af_r0_parser:parse(<<": f -> Any ; receive .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([receive_msg], Body).

receive_timeout_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Int -> Any ; receive-timeout .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([receive_timeout], Body).

%%% === Extended List Ops Translation ===

list_append_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List List -> List ; append .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_append], Body).

list_reverse_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List -> List ; reverse .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_reverse], Body).

list_nth_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List Int -> Any ; nth .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_nth], Body).

list_last_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List -> Any ; last .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_last], Body).

list_take_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List Int -> List ; take .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_take], Body).

list_empty_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List -> Bool ; empty? .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_empty], Body).

list_contains_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List Any -> Bool ; contains? .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_contains], Body).

list_flatten_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List -> List ; flatten .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_flatten], Body).

list_zip_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List List -> List ; zip .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_zip], Body).

list_head_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List -> Any ; head .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([head], Body).

list_tail_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List -> List ; tail .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([tail], Body).

%%% === 2dup Translation ===

two_dup_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any -> Any Any Any Any ; 2dup .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([to_r, dup, from_r, swap, to_r, dup, from_r, swap], Body).

%%% === Product Type with Multiple Fields ===

product_type_three_fields_test() ->
    Src = <<"type Color\n  r Int\n  g Int\n  b Int\n.">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    Names = [N || {N, _, _, _} <- Defs],
    %% Constructor
    ?assert(lists:member("color", Names)),
    %% Getters
    ?assert(lists:member("r", Names)),
    ?assert(lists:member("g", Names)),
    ?assert(lists:member("b", Names)),
    %% Setters
    ?assert(lists:member("r!", Names)),
    ?assert(lists:member("g!", Names)),
    ?assert(lists:member("b!", Names)),
    %% Verify constructor signature (reversed field types for sig_in)
    {_, CtorSigIn, CtorSigOut, CtorBody} = lists:keyfind("color", 1, Defs),
    ?assertEqual(['Int', 'Int', 'Int'], CtorSigIn),
    ?assertEqual(['Color'], CtorSigOut),
    ?assertMatch([{product_new, 'Color', 3}], CtorBody).

product_getter_sig_test() ->
    Src = <<"type Point\n  x Int\n  y Int\n.">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    %% Getter x: Point -> Int Point (non-destructive)
    {_, GetSigIn, GetSigOut, GetBody} = lists:keyfind("x", 1, Defs),
    ?assertEqual(['Point'], GetSigIn),
    ?assertEqual(['Int', 'Point'], GetSigOut),
    ?assertEqual([{product_get, 2}], GetBody).

product_setter_sig_test() ->
    Src = <<"type Point\n  x Int\n  y Int\n.">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    %% Setter x!: Int Point -> Point (TOS-first: Int=TOS, Point below)
    {_, SetSigIn, SetSigOut, SetBody} = lists:keyfind("x!", 1, Defs),
    ?assertEqual(['Int', 'Point'], SetSigIn),
    ?assertEqual(['Point'], SetSigOut),
    ?assertEqual([{product_set, 2}], SetBody).

%%% === Load Directive ===

load_directive_test() ->
    %% Use lib_math.a4 which defines square, double, cube
    Src = <<"load \"samples/lib_math.a4\"\n: use-square Int -> Int ; square .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    Names = [N || {N, _, _, _} <- Defs],
    %% Loaded file definitions should be included
    ?assert(lists:member("square", Names)),
    ?assert(lists:member("double", Names)),
    ?assert(lists:member("cube", Names)),
    %% Our word that uses loaded word
    ?assert(lists:member("use-square", Names)).

%%% === try_literal Edge Cases ===

try_literal_negative_int_test() ->
    Tokens = af_r0_parser:parse(<<": f -> Int ; -42 .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{lit, {'Int', -42}}], Body).

try_literal_false_test() ->
    Tokens = af_r0_parser:parse(<<": f -> Bool ; false .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{lit, {'Bool', false}}], Body).

try_literal_float_body_test() ->
    Tokens = af_r0_parser:parse(<<": f -> Float ; 2.718 .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{lit, {'Float', 2.718}}], Body).

try_literal_non_numeric_becomes_apply_impl_test() ->
    %% A token that is not a literal and not a primitive becomes apply_impl
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; foobar .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{apply_impl, "foobar"}], Body).

%%% === String Constraint in Signature ===

string_constraint_in_main_sig_test() ->
    %% Quoted string followed by -> infers String type constraint
    Src = <<": f \"hello\" -> String ; drop \"world\" .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'String', <<"hello">>}], SigIn).

string_constraint_with_explicit_type_test() ->
    %% Quoted string followed by explicit String type
    Src = <<": f \"hello\" String -> String ; drop \"world\" .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'String', <<"hello">>}], SigIn).

%%% === Explicit Value + Type in Signature ===

explicit_int_value_type_in_sig_test() ->
    %% 0 Int explicitly specifying Int type for value constraint
    Tokens = af_r0_parser:parse(<<": f 0 Int -> Int ; 1 .">>, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'Int', 0}], SigIn).

explicit_int_value_inferred_before_arrow_test() ->
    %% 0 followed directly by -> should infer Int
    Tokens = af_r0_parser:parse(<<": f 0 -> Int ; 1 .">>, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'Int', 0}], SigIn).

explicit_int_value_inferred_before_semi_test() ->
    %% 0 followed directly by ; should infer Int (no output sig)
    Tokens = af_r0_parser:parse(<<": f 0 ; 1 .">>, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'Int', 0}], SigIn).

%%% === Multiple Primitives in One Body ===

mixed_ops_body_test() ->
    %% Test a body with a mix of different primitive categories
    Tokens = af_r0_parser:parse(<<": f Int Int -> String ; + to-string .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([add, to_string], Body).

%%% === Map Keys Op ===

map_keys_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Map -> List ; map-keys .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([map_keys], Body).

%%% === Non-quoted load path ===

load_unquoted_path_test() ->
    %% Non-quoted load path
    Src = <<"load samples/lib_math.a4\n: use-it Int -> Int ; square .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    Names = [N || {N, _, _, _} <- Defs],
    ?assert(lists:member("square", Names)),
    ?assert(lists:member("use-it", Names)).

load_at_end_of_file_test() ->
    %% load with no path token following
    Src = <<"load">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([], Defs).

%%% === Empty sig_in (parse_sig_in([]) path) ===

empty_sig_in_test() ->
    %% Word with no input types: : f -> Int ; 42 .
    Tokens = af_r0_parser:parse(<<": f -> Int ; 42 .">>, <<"test">>),
    [{_, SigIn, SigOut, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([], SigIn),
    ?assertEqual(['Int'], SigOut).

%%% === String constraint before ; in sig ===

string_constraint_before_semi_test() ->
    %% Quoted string followed by ; (no output sig, infer String)
    Src = <<": f \"hello\" ; drop .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'String', <<"hello">>}], SigIn).

string_constraint_at_end_test() ->
    %% Quoted string at end of tokens (no following token)
    %% This is edge case: parse_sig_entry with quoted=true, Rest=[]
    Src = <<": f \"hello\"">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    %% This won't form a complete word def, but let's test compile_tokens handles it
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(1, length(Defs)),
    {_, SigIn, _, _} = hd(Defs),
    ?assertEqual([{'String', <<"hello">>}], SigIn).

%%% === Bool constraint before ; ===

bool_constraint_before_semi_test() ->
    Src = <<": f true ; drop 1 .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'Bool', true}], SigIn).

bool_constraint_with_explicit_type_test() ->
    %% Bool value followed by explicit Bool type
    Src = <<": f true Bool -> Int ; drop 1 .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'Bool', true}], SigIn).

bool_constraint_at_end_test() ->
    %% Bool value at end of tokens
    Src = <<": f false">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(1, length(Defs)),
    {_, SigIn, _, _} = hd(Defs),
    ?assertEqual([{'Bool', false}], SigIn).

%%% === Int value constraint at end of tokens ===

int_constraint_at_end_test() ->
    Src = <<": f 42">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(1, length(Defs)),
    {_, SigIn, _, _} = hd(Defs),
    ?assertEqual([{'Int', 42}], SigIn).

%%% === Int constraint with explicit non-Int type ===

int_constraint_custom_type_test() ->
    %% 0 followed by explicit type name
    Src = <<": f 0 MyType -> Int ; drop 1 .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    [{_, SigIn, _, _}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{'MyType', 0}], SigIn).

%%% === Sub-clause edge cases ===

sub_clause_empty_body_test() ->
    %% Sub-clauses with empty parse_sub_body (edge case)
    Src = <<": f Int -> Int ;\n"
            "    : 0 -> 0 ; .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual(1, length(Defs)).

%%% === Tuple ops: list-to-raw and deep-to-raw ===

list_to_raw_op_test() ->
    Tokens = af_r0_parser:parse(<<": f List -> Any ; list-to-raw .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([list_to_raw], Body).

deep_to_raw_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any -> Any ; deep-to-raw .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([deep_to_raw], Body).

%%% === xcall FFI op ===

xcall_op_test() ->
    Tokens = af_r0_parser:parse(<<": f Any Any Any -> Any ; xcall .">>, <<"test">>),
    [{_, _, _, Body}] = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([{apply_impl, "xcall"}], Body).

%%% === Load with .a4 extension fallback ===

load_without_extension_test() ->
    %% Load a path without .a4 extension — should try with .a4 appended
    Src = <<"load \"samples/lib_math\"">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    Names = [N || {N, _, _, _} <- Defs],
    ?assert(lists:member("square", Names)).

load_nonexistent_file_test() ->
    %% Load a file that doesn't exist at all
    Src = <<"load \"nonexistent_file_xyz\"">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    ?assertEqual([], Defs).

%%% === Product type with single field ending at EOF ===

product_type_field_at_eof_test() ->
    %% Type with field but no closing dot — parse_type_fields([], Acc)
    Src = <<"type Foo bar Int">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Defs = af_r0_compiler:compile_tokens(Tokens),
    Names = [N || {N, _, _, _} <- Defs],
    ?assert(lists:member("foo", Names)).
