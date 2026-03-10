-module(af_word_compiler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("operation.hrl").
-include("af_type.hrl").
-include("continuation.hrl").

%% Helper to build a word definition tuple from components.
word_def(Name, SigIn, SigOut, BodyNames) ->
    Body = [#operation{name = N} || N <- BodyNames],
    {Name, SigIn, SigOut, Body}.

%% Full type system setup (needed for interpreter-driven and product type tests).
setup() ->
    af_type:reset().

%% Minimal type system setup (Int, Bool, Any).
setup_types() ->
    af_type:reset().

%% ===================================================================
%% compile_words_to_module tests
%% ===================================================================

simple_double_test() ->
    Def = word_def("double", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_double} = af_word_compiler:compile_words_to_module(af_wc_test_double, [Def]),
    ?assertEqual([{'Int', 10}], af_wc_test_double:double([{'Int', 5}])),
    ?assertEqual([{'Int', 0}], af_wc_test_double:double([{'Int', 0}])),
    ?assertEqual([{'Int', -6}], af_wc_test_double:double([{'Int', -3}])).

simple_square_test() ->
    Def = word_def("square", ['Int'], ['Int'], ["dup", "*"]),
    {ok, af_wc_test_square} = af_word_compiler:compile_words_to_module(af_wc_test_square, [Def]),
    ?assertEqual([{'Int', 49}], af_wc_test_square:square([{'Int', 7}])),
    ?assertEqual([{'Int', 100}], af_wc_test_square:square([{'Int', 10}])),
    ?assertEqual([{'Int', 1}], af_wc_test_square:square([{'Int', -1}])).

identity_test() ->
    Def = word_def("id", ['Int'], ['Int'], []),
    {ok, af_wc_test_id} = af_word_compiler:compile_words_to_module(af_wc_test_id, [Def]),
    ?assertEqual([{'Int', 42}], af_wc_test_id:id([{'Int', 42}])).

two_arg_add_test() ->
    Def = word_def("add", ['Int', 'Int'], ['Int'], ["+"]),
    {ok, af_wc_test_add} = af_word_compiler:compile_words_to_module(af_wc_test_add, [Def]),
    ?assertEqual([{'Int', 15}], af_wc_test_add:add([{'Int', 7}, {'Int', 8}])),
    ?assertEqual([{'Int', 0}], af_wc_test_add:add([{'Int', 5}, {'Int', -5}])).

subtract_test() ->
    Def = word_def("sub", ['Int', 'Int'], ['Int'], ["-"]),
    {ok, af_wc_test_sub} = af_word_compiler:compile_words_to_module(af_wc_test_sub, [Def]),
    ?assertEqual([{'Int', 3}], af_wc_test_sub:sub([{'Int', 2}, {'Int', 5}])),
    ?assertEqual([{'Int', -1}], af_wc_test_sub:sub([{'Int', 3}, {'Int', 2}])).

multiply_test() ->
    Def = word_def("mul", ['Int', 'Int'], ['Int'], ["*"]),
    {ok, af_wc_test_mul} = af_word_compiler:compile_words_to_module(af_wc_test_mul, [Def]),
    ?assertEqual([{'Int', 35}], af_wc_test_mul:mul([{'Int', 5}, {'Int', 7}])).

divide_test() ->
    Def = word_def("divv", ['Int', 'Int'], ['Int'], ["/"]),
    {ok, af_wc_test_div} = af_word_compiler:compile_words_to_module(af_wc_test_div, [Def]),
    ?assertEqual([{'Int', 5}], af_wc_test_div:divv([{'Int', 2}, {'Int', 10}])).

stack_swap_test() ->
    Def = word_def("swap_sub", ['Int', 'Int'], ['Int'], ["swap", "-"]),
    {ok, af_wc_test_swap} = af_word_compiler:compile_words_to_module(af_wc_test_swap, [Def]),
    ?assertEqual([{'Int', -3}], af_wc_test_swap:swap_sub([{'Int', 2}, {'Int', 5}])),
    ?assertEqual([{'Int', 1}], af_wc_test_swap:swap_sub([{'Int', 3}, {'Int', 2}])).

stack_over_test() ->
    Def = word_def("over_add", ['Int', 'Int'], ['Int', 'Int'], ["over", "+"]),
    {ok, af_wc_test_over} = af_word_compiler:compile_words_to_module(af_wc_test_over, [Def]),
    ?assertEqual([{'Int', 12}, {'Int', 5}], af_wc_test_over:over_add([{'Int', 7}, {'Int', 5}])).

drop_test() ->
    Def = word_def("second", ['Int', 'Int'], ['Int'], ["drop"]),
    {ok, af_wc_test_drop} = af_word_compiler:compile_words_to_module(af_wc_test_drop, [Def]),
    ?assertEqual([{'Int', 99}], af_wc_test_drop:second([{'Int', 1}, {'Int', 99}])).

comparison_test() ->
    Def = word_def("gt", ['Int', 'Int'], ['Bool'], [">"]),
    {ok, af_wc_test_cmp} = af_word_compiler:compile_words_to_module(af_wc_test_cmp, [Def]),
    ?assertEqual([{'Bool', true}], af_wc_test_cmp:gt([{'Int', 3}, {'Int', 10}])),
    ?assertEqual([{'Bool', false}], af_wc_test_cmp:gt([{'Int', 10}, {'Int', 3}])).

not_test() ->
    Def = word_def("invert", ['Bool'], ['Bool'], ["not"]),
    {ok, af_wc_test_not} = af_word_compiler:compile_words_to_module(af_wc_test_not, [Def]),
    ?assertEqual([{'Bool', false}], af_wc_test_not:invert([{'Bool', true}])),
    ?assertEqual([{'Bool', true}], af_wc_test_not:invert([{'Bool', false}])).

literal_in_body_test() ->
    Def = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    {ok, af_wc_test_lit} = af_word_compiler:compile_words_to_module(af_wc_test_lit, [Def]),
    ?assertEqual([{'Int', 6}], af_wc_test_lit:inc([{'Int', 5}])),
    ?assertEqual([{'Int', 0}], af_wc_test_lit:inc([{'Int', -1}])).

complex_body_test() ->
    Def = word_def("sum_sq", ['Int', 'Int'], ['Int'],
                   ["swap", "dup", "*", "swap", "dup", "*", "+"]),
    {ok, af_wc_test_ssq} = af_word_compiler:compile_words_to_module(af_wc_test_ssq, [Def]),
    ?assertEqual([{'Int', 25}], af_wc_test_ssq:sum_sq([{'Int', 3}, {'Int', 4}])),
    ?assertEqual([{'Int', 2}], af_wc_test_ssq:sum_sq([{'Int', 1}, {'Int', 1}])).

multiple_functions_test() ->
    Def1 = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    Def2 = word_def("dec", ['Int'], ['Int'], ["1", "-"]),
    {ok, af_wc_test_multi} = af_word_compiler:compile_words_to_module(af_wc_test_multi, [Def1, Def2]),
    ?assertEqual([{'Int', 6}], af_wc_test_multi:inc([{'Int', 5}])),
    ?assertEqual([{'Int', 4}], af_wc_test_multi:dec([{'Int', 5}])).

no_compilable_words_test() ->
    ?assertEqual({error, no_compilable_words},
                 af_word_compiler:compile_words_to_module(af_wc_test_empty, [])).

two_dup_test() ->
    Def = word_def("both", ['Int', 'Int'], ['Int', 'Int', 'Int', 'Int'], ["2dup"]),
    {ok, af_wc_test_2dup} = af_word_compiler:compile_words_to_module(af_wc_test_2dup, [Def]),
    ?assertEqual([{'Int', 3}, {'Int', 7}, {'Int', 3}, {'Int', 7}],
                 af_wc_test_2dup:both([{'Int', 3}, {'Int', 7}])).

rot_test() ->
    Def = word_def("myrot", ['Int', 'Int', 'Int'], ['Int', 'Int', 'Int'], ["rot"]),
    {ok, af_wc_test_rot} = af_word_compiler:compile_words_to_module(af_wc_test_rot, [Def]),
    ?assertEqual([{'Int', 3}, {'Int', 1}, {'Int', 2}],
                 af_wc_test_rot:myrot([{'Int', 1}, {'Int', 2}, {'Int', 3}])).

%% ===================================================================
%% compile_words_to_binary tests
%% ===================================================================

compile_words_to_binary_test() ->
    Def = word_def("bin_id", ['Int'], ['Int'], []),
    {ok, af_wc_test_bin, Bin} = af_word_compiler:compile_words_to_binary(af_wc_test_bin, [Def]),
    ?assert(is_binary(Bin)).

compile_words_to_binary_empty_test() ->
    ?assertEqual({error, no_compilable_words},
                 af_word_compiler:compile_words_to_binary(af_wc_test_empty2, [])).

%% Verify binary can be loaded separately after compile_words_to_binary
compile_words_to_binary_loadable_test() ->
    Def = word_def("binload", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_binload, Bin} = af_word_compiler:compile_words_to_binary(af_wc_test_binload, [Def]),
    ?assert(is_binary(Bin)),
    %% Load manually and verify it works
    {module, af_wc_test_binload} = code:load_binary(af_wc_test_binload, "af_wc_test_binload.beam", Bin),
    ?assertEqual([{'Int', 10}], af_wc_test_binload:binload([{'Int', 5}])).

%% ===================================================================
%% get_module_binary / store_module_binary tests
%% ===================================================================

get_module_binary_not_found_test() ->
    af_word_compiler:store_module_binary(af_wc_test_bintable_sentinel, <<>>),
    ?assertEqual(not_found, af_word_compiler:get_module_binary(af_wc_nonexistent_mod)).

get_module_binary_found_test() ->
    Def = word_def("id", ['Int'], ['Int'], []),
    {ok, af_wc_test_binget} = af_word_compiler:compile_words_to_module(af_wc_test_binget, [Def]),
    {ok, Bin} = af_word_compiler:get_module_binary(af_wc_test_binget),
    ?assert(is_binary(Bin)).

store_module_binary_test() ->
    af_word_compiler:store_module_binary(af_wc_test_manual_store, <<"fake_binary">>),
    ?assertEqual({ok, <<"fake_binary">>}, af_word_compiler:get_module_binary(af_wc_test_manual_store)).

%% store_module_binary overwrites previous value
store_module_binary_overwrite_test() ->
    af_word_compiler:store_module_binary(af_wc_test_overwrite, <<"first">>),
    af_word_compiler:store_module_binary(af_wc_test_overwrite, <<"second">>),
    ?assertEqual({ok, <<"second">>}, af_word_compiler:get_module_binary(af_wc_test_overwrite)).

%% ===================================================================
%% group_by_name tests
%% ===================================================================

group_by_name_single_test() ->
    Defs = [{"a", ['Int'], ['Int'], []}],
    Groups = af_word_compiler:group_by_name(Defs),
    ?assertEqual(1, length(Groups)),
    ?assertMatch({"a", [{"a", ['Int'], ['Int'], []}]}, hd(Groups)).

group_by_name_multiple_distinct_test() ->
    Defs = [{"a", ['Int'], ['Int'], []},
            {"b", ['Bool'], ['Bool'], []},
            {"c", [], ['Int'], []}],
    Groups = af_word_compiler:group_by_name(Defs),
    ?assertEqual(3, length(Groups)),
    %% Order preserved
    ?assertMatch({"a", _}, lists:nth(1, Groups)),
    ?assertMatch({"b", _}, lists:nth(2, Groups)),
    ?assertMatch({"c", _}, lists:nth(3, Groups)).

group_by_name_same_name_grouped_test() ->
    Defs = [{"a", ['Int'], ['Int'], []},
            {"b", [], [], []},
            {"a", [{'Int', 0}], ['Int'], []}],
    Groups = af_word_compiler:group_by_name(Defs),
    ?assertEqual(2, length(Groups)),
    {"a", ADefs} = lists:keyfind("a", 1, Groups),
    ?assertEqual(2, length(ADefs)),
    {"b", BDefs} = lists:keyfind("b", 1, Groups),
    ?assertEqual(1, length(BDefs)).

group_by_name_preserves_order_test() ->
    %% First occurrence determines group position
    Defs = [{"x", [], [], []},
            {"y", [], [], []},
            {"x", ['Int'], [], []},
            {"z", [], [], []},
            {"y", ['Bool'], [], []}],
    Groups = af_word_compiler:group_by_name(Defs),
    ?assertEqual(3, length(Groups)),
    ?assertMatch({"x", _}, lists:nth(1, Groups)),
    ?assertMatch({"y", _}, lists:nth(2, Groups)),
    ?assertMatch({"z", _}, lists:nth(3, Groups)),
    {"x", XDefs} = lists:keyfind("x", 1, Groups),
    ?assertEqual(2, length(XDefs)),
    {"y", YDefs} = lists:keyfind("y", 1, Groups),
    ?assertEqual(2, length(YDefs)).

group_by_name_empty_test() ->
    ?assertEqual([], af_word_compiler:group_by_name([])).

%% ===================================================================
%% Value-constrained patterns (float, boolean) in sig_in
%% ===================================================================

float_value_constraint_test() ->
    Def = {"fzero", [{'Float', 0.0}], ['Int'], [#operation{name = "drop"}, #operation{name = "1"}]},
    {ok, af_wc_test_fval} = af_word_compiler:compile_words_to_module(af_wc_test_fval, [Def]),
    ?assertEqual([{'Int', 1}], af_wc_test_fval:fzero([{'Float', 0.0}])).

bool_value_constraint_test() ->
    Def = {"btrue", [{'Bool', true}], ['Int'], [#operation{name = "drop"}, #operation{name = "1"}]},
    {ok, af_wc_test_bval} = af_word_compiler:compile_words_to_module(af_wc_test_bval, [Def]),
    ?assertEqual([{'Int', 1}], af_wc_test_bval:btrue([{'Bool', true}])).

%% ===================================================================
%% Comparison ops (==, !=, <, <=, >=)
%% ===================================================================

eq_comparison_test() ->
    Def = word_def("eq", ['Int', 'Int'], ['Bool'], ["=="]),
    {ok, af_wc_test_eq} = af_word_compiler:compile_words_to_module(af_wc_test_eq, [Def]),
    ?assertEqual([{'Bool', true}], af_wc_test_eq:eq([{'Int', 5}, {'Int', 5}])),
    ?assertEqual([{'Bool', false}], af_wc_test_eq:eq([{'Int', 5}, {'Int', 3}])).

neq_comparison_test() ->
    Def = word_def("neq", ['Int', 'Int'], ['Bool'], ["!="]),
    {ok, af_wc_test_neq} = af_word_compiler:compile_words_to_module(af_wc_test_neq, [Def]),
    ?assertEqual([{'Bool', false}], af_wc_test_neq:neq([{'Int', 5}, {'Int', 5}])),
    ?assertEqual([{'Bool', true}], af_wc_test_neq:neq([{'Int', 5}, {'Int', 3}])).

lt_comparison_test() ->
    Def = word_def("lt", ['Int', 'Int'], ['Bool'], ["<"]),
    {ok, af_wc_test_lt} = af_word_compiler:compile_words_to_module(af_wc_test_lt, [Def]),
    ?assertEqual([{'Bool', true}], af_wc_test_lt:lt([{'Int', 5}, {'Int', 3}])),
    ?assertEqual([{'Bool', false}], af_wc_test_lt:lt([{'Int', 3}, {'Int', 5}])).

le_comparison_test() ->
    Def = word_def("le", ['Int', 'Int'], ['Bool'], ["<="]),
    {ok, af_wc_test_le} = af_word_compiler:compile_words_to_module(af_wc_test_le, [Def]),
    ?assertEqual([{'Bool', true}], af_wc_test_le:le([{'Int', 5}, {'Int', 3}])),
    ?assertEqual([{'Bool', true}], af_wc_test_le:le([{'Int', 5}, {'Int', 5}])),
    ?assertEqual([{'Bool', false}], af_wc_test_le:le([{'Int', 3}, {'Int', 5}])).

ge_comparison_test() ->
    Def = word_def("ge", ['Int', 'Int'], ['Bool'], [">="]),
    {ok, af_wc_test_ge} = af_word_compiler:compile_words_to_module(af_wc_test_ge, [Def]),
    ?assertEqual([{'Bool', true}], af_wc_test_ge:ge([{'Int', 3}, {'Int', 5}])),
    ?assertEqual([{'Bool', true}], af_wc_test_ge:ge([{'Int', 5}, {'Int', 5}])),
    ?assertEqual([{'Bool', false}], af_wc_test_ge:ge([{'Int', 5}, {'Int', 3}])).

%% ===================================================================
%% Float and boolean literals in body
%% ===================================================================

float_literal_in_body_test() ->
    Def = word_def("addfloat", ['Int'], ['Int'], ["1.5"]),
    {ok, af_wc_test_flit} = af_word_compiler:compile_words_to_module(af_wc_test_flit, [Def]),
    Result = af_wc_test_flit:addfloat([{'Int', 5}]),
    ?assertEqual([{'Float', 1.5}, {'Int', 5}], Result).

true_literal_in_body_test() ->
    Def = word_def("pushtrue", ['Int'], ['Bool', 'Int'], ["true"]),
    {ok, af_wc_test_tlit} = af_word_compiler:compile_words_to_module(af_wc_test_tlit, [Def]),
    ?assertEqual([{'Bool', true}, {'Int', 5}], af_wc_test_tlit:pushtrue([{'Int', 5}])).

false_literal_in_body_test() ->
    Def = word_def("pushfalse", ['Int'], ['Bool', 'Int'], ["false"]),
    {ok, af_wc_test_flit2} = af_word_compiler:compile_words_to_module(af_wc_test_flit2, [Def]),
    ?assertEqual([{'Bool', false}, {'Int', 5}], af_wc_test_flit2:pushfalse([{'Int', 5}])).

%% ===================================================================
%% Multi-clause word compilation
%% ===================================================================

multi_clause_integer_pattern_test() ->
    Def1 = {"absval", [{'Int', 0}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "0"}]},
    Def2 = {"absval", ['Int'], ['Int'], [#operation{name = "dup"}, #operation{name = "*"}]},
    {ok, af_wc_test_mclause} = af_word_compiler:compile_words_to_module(af_wc_test_mclause, [Def1, Def2]),
    ?assertEqual([{'Int', 0}], af_wc_test_mclause:absval([{'Int', 0}])),
    ?assertEqual([{'Int', 25}], af_wc_test_mclause:absval([{'Int', 5}])).

%% Multi-clause with boolean value constraints
multi_clause_bool_pattern_test() ->
    Def1 = {"bcheck", [{'Bool', true}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "1"}]},
    Def2 = {"bcheck", [{'Bool', false}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "0"}]},
    {ok, af_wc_test_mbool} = af_word_compiler:compile_words_to_module(af_wc_test_mbool, [Def1, Def2]),
    ?assertEqual([{'Int', 1}], af_wc_test_mbool:bcheck([{'Bool', true}])),
    ?assertEqual([{'Int', 0}], af_wc_test_mbool:bcheck([{'Bool', false}])).

%% Multi-clause factorial via interpreter (recursive, tests compile_clause + inter-word call)
multi_clause_factorial_test() ->
    setup(),
    Tokens = af_parser:parse(
        ": factorial 0 Int -> Int ; drop 1 .\n"
        ": factorial Int -> Int ; dup 1 - factorial * .", "test"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    Defs = af_word_compiler:find_compiled_word_defs("factorial"),
    ?assertMatch([{_, _, _, _}, {_, _, _, _}], Defs),
    {ok, af_wc_test_fact} = af_word_compiler:compile_words_to_module(af_wc_test_fact, Defs),
    ?assertEqual([{'Int', 1}], af_wc_test_fact:factorial([{'Int', 0}])),
    ?assertEqual([{'Int', 1}], af_wc_test_fact:factorial([{'Int', 1}])),
    ?assertEqual([{'Int', 120}], af_wc_test_fact:factorial([{'Int', 5}])),
    ?assertEqual([{'Int', 720}], af_wc_test_fact:factorial([{'Int', 6}])).

%% Three clauses for a single word
multi_clause_three_test() ->
    Def1 = {"classify", [{'Int', 0}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "0"}]},
    Def2 = {"classify", [{'Int', 1}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "10"}]},
    Def3 = {"classify", ['Int'], ['Int'],
            [#operation{name = "drop"}, #operation{name = "99"}]},
    {ok, af_wc_test_3clause} = af_word_compiler:compile_words_to_module(
        af_wc_test_3clause, [Def1, Def2, Def3]),
    ?assertEqual([{'Int', 0}], af_wc_test_3clause:classify([{'Int', 0}])),
    ?assertEqual([{'Int', 10}], af_wc_test_3clause:classify([{'Int', 1}])),
    ?assertEqual([{'Int', 99}], af_wc_test_3clause:classify([{'Int', 7}])).

%% ===================================================================
%% Inter-word call tests
%% ===================================================================

inter_word_call_test() ->
    setup_types(),
    Def1 = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    Def2 = word_def("double_inc", ['Int'], ['Int'], ["inc", "inc"]),
    {ok, af_wc_test_inter} = af_word_compiler:compile_words_to_module(af_wc_test_inter, [Def1, Def2]),
    ?assertEqual([{'Int', 6}], af_wc_test_inter:inc([{'Int', 5}])),
    ?assertEqual([{'Int', 7}], af_wc_test_inter:double_inc([{'Int', 5}])),
    ?assertEqual([{'Int', 2}], af_wc_test_inter:double_inc([{'Int', 0}])).

inter_word_chain_test() ->
    setup_types(),
    Def1 = word_def("double", ['Int'], ['Int'], ["dup", "+"]),
    Def2 = word_def("triple", ['Int'], ['Int'], ["dup", "double", "+"]),
    {ok, af_wc_test_chain} = af_word_compiler:compile_words_to_module(af_wc_test_chain, [Def1, Def2]),
    ?assertEqual([{'Int', 10}], af_wc_test_chain:double([{'Int', 5}])),
    ?assertEqual([{'Int', 15}], af_wc_test_chain:triple([{'Int', 5}])),
    ?assertEqual([{'Int', 0}], af_wc_test_chain:triple([{'Int', 0}])).

inter_word_complex_test() ->
    setup_types(),
    Def1 = word_def("square", ['Int'], ['Int'], ["dup", "*"]),
    Def2 = word_def("sum_sq", ['Int', 'Int'], ['Int'], ["swap", "square", "swap", "square", "+"]),
    {ok, af_wc_test_complex} = af_word_compiler:compile_words_to_module(af_wc_test_complex, [Def1, Def2]),
    ?assertEqual([{'Int', 9}], af_wc_test_complex:square([{'Int', 3}])),
    ?assertEqual([{'Int', 25}], af_wc_test_complex:sum_sq([{'Int', 3}, {'Int', 4}])),
    ?assertEqual([{'Int', 2}], af_wc_test_complex:sum_sq([{'Int', 1}, {'Int', 1}])).

%% ===================================================================
%% Runtime dispatch (unknown ops go through apply_impl)
%% ===================================================================

runtime_dispatch_test() ->
    setup_types(),
    Def = word_def("show", ['Int'], ['Int'], ["print"]),
    {ok, af_wc_test_rt} = af_word_compiler:compile_words_to_module(af_wc_test_rt, [Def]),
    Result = af_wc_test_rt:show([{'Int', 42}]),
    ?assertEqual([{'Int', 42}], Result).

runtime_dispatch_unknown_op_test() ->
    setup_types(),
    Def = word_def("mystery", ['Int'], ['Int'], ["totally_unknown_xyz"]),
    {ok, af_wc_test_rt2} = af_word_compiler:compile_words_to_module(af_wc_test_rt2, [Def]),
    Result = af_wc_test_rt2:mystery([{'Int', 42}]),
    ?assertEqual([{'Atom', "totally_unknown_xyz"}, {'Int', 42}], Result).

%% ===================================================================
%% find_native_word tests
%% ===================================================================

find_native_word_not_found_test() ->
    setup_types(),
    ?assertEqual(not_found, af_word_compiler:find_native_word("no_such_native_word_xyz")).

find_native_word_found_test() ->
    setup_types(),
    Def = word_def("nfind", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_nfind} = af_word_compiler:compile_words_to_module(af_wc_test_nfind, [Def]),
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_nfind, nfind, ['Int'], ['Int']),
    af_type:add_op('Int', Wrapper),
    ?assertMatch({ok, af_wc_test_nfind, _, _}, af_word_compiler:find_native_word("nfind")).

%% find_native_word skips non-native ops
find_native_word_skips_compiled_test() ->
    setup_types(),
    Body = [#operation{name = "dup"}, #operation{name = "+"}],
    Op = #operation{
        name = "only_compiled_xyz",
        sig_in = ['Int'],
        sig_out = ['Int'],
        impl = fun(_) -> ok end,
        source = {compiled, Body}
    },
    af_type:add_op('Int', Op),
    ?assertEqual(not_found, af_word_compiler:find_native_word("only_compiled_xyz")).

%% ===================================================================
%% find_compiled_word_defs tests
%% ===================================================================

find_compiled_word_defs_empty_test() ->
    setup_types(),
    ?assertEqual([], af_word_compiler:find_compiled_word_defs("nonexistent_compiled_word")).

find_compiled_word_defs_found_test() ->
    setup_types(),
    Body = [#operation{name = "dup"}, #operation{name = "+"}],
    Op = #operation{
        name = "cdef_test",
        sig_in = ['Int'],
        sig_out = ['Int'],
        impl = fun(_) -> ok end,
        source = {compiled, Body}
    },
    af_type:add_op('Int', Op),
    Defs = af_word_compiler:find_compiled_word_defs("cdef_test"),
    ?assertEqual(1, length(Defs)),
    [{_, ['Int'], ['Int'], _}] = Defs.

%% find_compiled_word_defs via interpreter (real word definition)
find_compiled_word_defs_interpreter_test() ->
    setup(),
    Tokens = af_parser:parse(": double Int -> Int ; dup + .", "test"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    Defs = af_word_compiler:find_compiled_word_defs("double"),
    ?assertMatch([{_, _, _, _}], Defs),
    [{Name, SigIn, SigOut, Body}] = Defs,
    ?assertEqual("double", Name),
    ?assert(is_list(SigIn)),
    ?assert(is_list(SigOut)),
    ?assert(is_list(Body)),
    ?assert(length(Body) > 0).

%% find_compiled_word_defs skips non-compiled ops (native, auto, etc.)
find_compiled_word_defs_skips_native_test() ->
    setup_types(),
    Op = #operation{
        name = "native_only_xyz",
        sig_in = ['Int'],
        sig_out = ['Int'],
        impl = fun(_) -> ok end,
        source = {native, some_module}
    },
    af_type:add_op('Int', Op),
    ?assertEqual([], af_word_compiler:find_compiled_word_defs("native_only_xyz")).

%% ===================================================================
%% Cross-module (remote) word call
%% ===================================================================

cross_module_native_call_test() ->
    setup_types(),
    HelperDef = word_def("helper", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_helper} = af_word_compiler:compile_words_to_module(af_wc_test_helper, [HelperDef]),
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_helper, helper, ['Int'], ['Int']),
    af_type:add_op('Int', Wrapper),
    CallerDef = word_def("use_helper", ['Int'], ['Int'], ["helper"]),
    {ok, af_wc_test_caller} = af_word_compiler:compile_words_to_module(af_wc_test_caller, [CallerDef]),
    ?assertEqual([{'Int', 10}], af_wc_test_caller:use_helper([{'Int', 5}])).

%% ===================================================================
%% arg_name for N > 3
%% ===================================================================

four_arg_word_test() ->
    Def = word_def("sum4", ['Int', 'Int', 'Int', 'Int'], ['Int'],
                   ["+", "+", "+"]),
    {ok, af_wc_test_4arg} = af_word_compiler:compile_words_to_module(af_wc_test_4arg, [Def]),
    ?assertEqual([{'Int', 10}], af_wc_test_4arg:sum4([{'Int', 1}, {'Int', 2}, {'Int', 3}, {'Int', 4}])).

%% Five args to exercise arg_name(5)
five_arg_word_test() ->
    Def = word_def("sum5", ['Int', 'Int', 'Int', 'Int', 'Int'], ['Int'],
                   ["+", "+", "+", "+"]),
    {ok, af_wc_test_5arg} = af_word_compiler:compile_words_to_module(af_wc_test_5arg, [Def]),
    ?assertEqual([{'Int', 15}], af_wc_test_5arg:sum5([{'Int', 1}, {'Int', 2}, {'Int', 3}, {'Int', 4}, {'Int', 5}])).

%% ===================================================================
%% Opaque stack dispatch (simulate_body with stack type)
%% ===================================================================

opaque_stack_local_call_test() ->
    setup_types(),
    Def1 = word_def("dbl", ['Int'], ['Int'], ["dup", "+"]),
    Def2 = word_def("dbl_dbl", ['Int'], ['Int'], ["dbl", "dbl"]),
    {ok, af_wc_test_opaque} = af_word_compiler:compile_words_to_module(af_wc_test_opaque, [Def1, Def2]),
    ?assertEqual([{'Int', 20}], af_wc_test_opaque:dbl_dbl([{'Int', 5}])).

%% Opaque stack with apply_impl fallback (op after inter-word call)
opaque_stack_apply_impl_test() ->
    setup_types(),
    %% After calling inc, stack is opaque. "dup" dispatches through apply_impl
    Def1 = word_def("inc2", ['Int'], ['Int'], ["1", "+"]),
    Def2 = word_def("inc_then_dup", ['Int'], ['Int'], ["inc2", "dup", "+"]),
    {ok, af_wc_test_opq2} = af_word_compiler:compile_words_to_module(af_wc_test_opq2, [Def1, Def2]),
    ?assertEqual([{'Int', 12}], af_wc_test_opq2:inc_then_dup([{'Int', 5}])).

%% ===================================================================
%% make_wrapper tests
%% ===================================================================

make_wrapper_test() ->
    Def = word_def("wrap_test", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_wrap} = af_word_compiler:compile_words_to_module(af_wc_test_wrap, [Def]),
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_wrap, wrap_test, ['Int'], ['Int']),
    ?assertEqual("wrap_test", Wrapper#operation.name),
    ?assertEqual(['Int'], Wrapper#operation.sig_in),
    ?assertEqual(['Int'], Wrapper#operation.sig_out),
    ?assertMatch({native, af_wc_test_wrap}, Wrapper#operation.source).

%% make_wrapper impl actually works when called
make_wrapper_impl_test() ->
    Def = word_def("wrapexec", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_wexec} = af_word_compiler:compile_words_to_module(af_wc_test_wexec, [Def]),
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_wexec, wrapexec, ['Int'], ['Int']),
    Cont = #continuation{data_stack = [{'Int', 5}]},
    Result = (Wrapper#operation.impl)(Cont),
    ?assertEqual([{'Int', 10}], Result#continuation.data_stack).

%% make_wrapper preserves rest of stack
make_wrapper_preserves_rest_test() ->
    Def = word_def("wraprest", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_wrest} = af_word_compiler:compile_words_to_module(af_wc_test_wrest, [Def]),
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_wrest, wraprest, ['Int'], ['Int']),
    Cont = #continuation{data_stack = [{'Int', 3}, {'Int', 99}]},
    Result = (Wrapper#operation.impl)(Cont),
    ?assertEqual([{'Int', 6}, {'Int', 99}], Result#continuation.data_stack).

%% ===================================================================
%% String operations in compilation
%% ===================================================================

string_concat_test() ->
    Def = {"strcat", ['String', 'String'], ['String'],
           [#operation{name = "concat"}]},
    {ok, af_wc_test_strcat} = af_word_compiler:compile_words_to_module(af_wc_test_strcat, [Def]),
    Result = af_wc_test_strcat:strcat([{'String', <<"world">>}, {'String', <<"hello ">>}]),
    ?assertEqual([{'String', <<"hello world">>}], Result).

%% Note: string_length compilation generates erlang:length/1 on a binary,
%% which crashes at runtime. This is a known limitation in af_word_compiler.
%% The List length path works correctly (see list_length_test).

%% Quoted string literal push
quoted_string_literal_test() ->
    Def = {"pushstr", ['Int'], ['String', 'Int'],
           [#operation{name = "hello", source = quoted_string}]},
    {ok, af_wc_test_qstr} = af_word_compiler:compile_words_to_module(af_wc_test_qstr, [Def]),
    Result = af_wc_test_qstr:pushstr([{'Int', 42}]),
    ?assertEqual([{'String', <<"hello">>}, {'Int', 42}], Result).

%% Quoted string on opaque stack
quoted_string_on_opaque_stack_test() ->
    setup_types(),
    %% After a local call, stack is opaque. Then push a quoted string.
    Def1 = word_def("inc3", ['Int'], ['Int'], ["1", "+"]),
    Def2 = {"tag_it", ['Int'], ['String', 'Int'],
            [#operation{name = "inc3"},
             #operation{name = "tagged", source = quoted_string}]},
    {ok, af_wc_test_qopq} = af_word_compiler:compile_words_to_module(af_wc_test_qopq, [Def1, Def2]),
    Result = af_wc_test_qopq:tag_it([{'Int', 5}]),
    ?assertEqual([{'String', <<"tagged">>}, {'Int', 6}], Result).

%% ===================================================================
%% List operations in compilation
%% ===================================================================

list_nil_test() ->
    Def = {"mknil", [], ['List'], [#operation{name = "nil"}]},
    {ok, af_wc_test_nil} = af_word_compiler:compile_words_to_module(af_wc_test_nil, [Def]),
    Result = af_wc_test_nil:mknil([]),
    ?assertEqual([{'List', []}], Result).

list_cons_test() ->
    %% cons expects: [Item, {ListExpr, List} | Rest]
    %% Build a word that takes a List and an Int, conses the Int onto the List
    %% Stack: [Int, List | Rest] -> cons needs [Item, List]
    %% So sig_in = [Int, List] (TOS-first: Int is TOS, List is below)
    %% Body: swap cons (put List on top... wait, cons expects Item on TOS, List below)
    %% Actually looking at translate_op("cons"): [Item, {ListExpr, 'List'} | Rest]
    %% Item = TOS, List = second. So sig_in [Int, List] means Int=TOS, List=below
    %% which is exactly what cons expects.
    Def = {"mycons", ['Int', 'List'], ['List'],
           [#operation{name = "cons"}]},
    {ok, af_wc_test_cons} = af_word_compiler:compile_words_to_module(af_wc_test_cons, [Def]),
    %% Stack is TOS-first: [TOS=Int, List]
    Result = af_wc_test_cons:mycons([{'Int', 42}, {'List', []}]),
    ?assertMatch([{'List', [{'Int', 42}]}], Result).

list_head_test() ->
    Def = {"myhead", ['List'], ['Any'],
           [#operation{name = "head"}]},
    {ok, af_wc_test_head} = af_word_compiler:compile_words_to_module(af_wc_test_head, [Def]),
    Result = af_wc_test_head:myhead([{'List', [{'Int', 1}, {'Int', 2}]}]),
    ?assertEqual([{'Int', 1}], Result).

list_tail_test() ->
    Def = {"mytail", ['List'], ['List'],
           [#operation{name = "tail"}]},
    {ok, af_wc_test_tail} = af_word_compiler:compile_words_to_module(af_wc_test_tail, [Def]),
    Result = af_wc_test_tail:mytail([{'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}]),
    ?assertEqual([{'List', [{'Int', 2}, {'Int', 3}]}], Result).

list_reverse_test() ->
    Def = {"myrev", ['List'], ['List'],
           [#operation{name = "reverse"}]},
    {ok, af_wc_test_rev} = af_word_compiler:compile_words_to_module(af_wc_test_rev, [Def]),
    Result = af_wc_test_rev:myrev([{'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}]),
    ?assertEqual([{'List', [{'Int', 3}, {'Int', 2}, {'Int', 1}]}], Result).

list_append_test() ->
    Def = {"myappend", ['List', 'List'], ['List'],
           [#operation{name = "append"}]},
    {ok, af_wc_test_append} = af_word_compiler:compile_words_to_module(af_wc_test_append, [Def]),
    %% TOS-first: first arg = TOS = list to append, second = base list
    Result = af_wc_test_append:myappend([{'List', [{'Int', 3}]}, {'List', [{'Int', 1}, {'Int', 2}]}]),
    ?assertEqual([{'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}], Result).

list_length_test() ->
    Def = {"listlen", ['List'], ['Int'],
           [#operation{name = "length"}]},
    {ok, af_wc_test_llen} = af_word_compiler:compile_words_to_module(af_wc_test_llen, [Def]),
    Result = af_wc_test_llen:listlen([{'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}]),
    ?assertEqual([{'Int', 3}], Result).

%% ===================================================================
%% Map operations in compilation
%% ===================================================================

map_new_test() ->
    Def = {"mkmap", [], ['Map'], [#operation{name = "map-new"}]},
    {ok, af_wc_test_mapnew} = af_word_compiler:compile_words_to_module(af_wc_test_mapnew, [Def]),
    Result = af_wc_test_mapnew:mkmap([]),
    ?assertEqual([{'Map', #{}}], Result).

map_size_test() ->
    Def = {"msize", ['Map'], ['Int'],
           [#operation{name = "map-size"}]},
    {ok, af_wc_test_msize} = af_word_compiler:compile_words_to_module(af_wc_test_msize, [Def]),
    Result = af_wc_test_msize:msize([{'Map', #{a => 1, b => 2}}]),
    ?assertEqual([{'Int', 2}], Result).

map_new_then_size_test() ->
    setup_types(),
    %% Combine: push empty map, then get its size
    Def = {"empty_map_size", [], ['Int'],
           [#operation{name = "map-new"}, #operation{name = "map-size"}]},
    {ok, af_wc_test_mns} = af_word_compiler:compile_words_to_module(af_wc_test_mns, [Def]),
    Result = af_wc_test_mns:'empty_map_size'([]),
    ?assertEqual([{'Int', 0}], Result).

%% ===================================================================
%% Product type getter/setter compilation
%% ===================================================================

product_type_getter_test() ->
    setup(),
    Tokens = af_parser:parse("type Point x Int y Int .\n: get-x Point -> Int ; x .", "test"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    Defs = af_word_compiler:find_compiled_word_defs("get-x"),
    ?assertMatch([{_, _, _, _}], Defs),
    {ok, af_wc_test_prod_get} = af_word_compiler:compile_words_to_module(af_wc_test_prod_get, Defs),
    Instance = {'Point', #{x => {'Int', 3}, y => {'Int', 4}}},
    Result = af_wc_test_prod_get:'get-x'([Instance]),
    %% Getter returns field value + keeps instance
    ?assertMatch([{'Int', 3}, {'Point', _}], Result).

product_type_getter_y_test() ->
    setup(),
    Tokens = af_parser:parse("type Point x Int y Int .\n: get-y Point -> Int ; y .", "test"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    Defs = af_word_compiler:find_compiled_word_defs("get-y"),
    ?assertMatch([{_, _, _, _}], Defs),
    {ok, af_wc_test_prod_gety} = af_word_compiler:compile_words_to_module(af_wc_test_prod_gety, Defs),
    Instance = {'Point', #{x => {'Int', 3}, y => {'Int', 7}}},
    Result = af_wc_test_prod_gety:'get-y'([Instance]),
    ?assertMatch([{'Int', 7}, {'Point', _}], Result).

product_type_setter_direct_test() ->
    setup(),
    %% Define Point type so try_product_op can look it up
    Tokens = af_parser:parse("type Point x Int y Int .", "test"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    %% Build word def directly with correct sig_in for setter:
    %% x! expects [NewVal, Instance] — so TOS is new value, below is Point instance
    %% The setter path in try_product_op checks stack[1] for InstanceType
    Def = {"set-x", ['Int', 'Point'], ['Point'],
           [#operation{name = "x!"}]},
    {ok, af_wc_test_prod_set2} = af_word_compiler:compile_words_to_module(af_wc_test_prod_set2, [Def]),
    Instance = {'Point', #{x => {'Int', 3}, y => {'Int', 4}}},
    %% Stack is TOS-first: [Int, Point]
    Result = af_wc_test_prod_set2:'set-x'([{'Int', 10}, Instance]),
    ?assertMatch([{'Point', _}], Result),
    [{'Point', Fields}] = Result,
    ?assertEqual({'Int', 10}, maps:get(x, Fields)),
    ?assertEqual({'Int', 4}, maps:get(y, Fields)).

%% ===================================================================
%% Interpreter-driven compilation (full round-trip)
%% ===================================================================

interpreter_compile_double_test() ->
    setup(),
    Tokens = af_parser:parse(": double Int -> Int ; dup + .", "test"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    Defs = af_word_compiler:find_compiled_word_defs("double"),
    ?assertMatch([{_, _, _, _}], Defs),
    {ok, af_wc_test_idouble} = af_word_compiler:compile_words_to_module(af_wc_test_idouble, Defs),
    ?assertEqual([{'Int', 42}], af_wc_test_idouble:double([{'Int', 21}])).

interpreter_compile_inc_test() ->
    setup(),
    Tokens = af_parser:parse(": inc Int -> Int ; 1 + .", "test"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    Defs = af_word_compiler:find_compiled_word_defs("inc"),
    {ok, af_wc_test_iinc} = af_word_compiler:compile_words_to_module(af_wc_test_iinc, Defs),
    ?assertEqual([{'Int', 6}], af_wc_test_iinc:inc([{'Int', 5}])).

%% ===================================================================
%% resolve_word_call: context without module key
%% ===================================================================

resolve_no_module_context_test() ->
    setup_types(),
    %% Compile a native helper first
    HelperDef = word_def("rhelper", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_rhelper} = af_word_compiler:compile_words_to_module(af_wc_test_rhelper, [HelperDef]),
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_rhelper, rhelper, ['Int'], ['Int']),
    af_type:add_op('Int', Wrapper),
    %% Compile a word that calls rhelper — should find it via cross-module native lookup
    CallerDef = word_def("use_rhelper", ['Int'], ['Int'], ["rhelper"]),
    {ok, af_wc_test_rcaller} = af_word_compiler:compile_words_to_module(af_wc_test_rcaller, [CallerDef]),
    ?assertEqual([{'Int', 14}], af_wc_test_rcaller:use_rhelper([{'Int', 7}])).

%% ===================================================================
%% Rest-of-stack preservation (compiled functions pass through extra items)
%% ===================================================================

rest_of_stack_preserved_test() ->
    Def = word_def("inc_r", ['Int'], ['Int'], ["1", "+"]),
    {ok, af_wc_test_rest} = af_word_compiler:compile_words_to_module(af_wc_test_rest, [Def]),
    %% Extra items on the stack should be preserved
    Result = af_wc_test_rest:inc_r([{'Int', 5}, {'Bool', true}, {'Int', 99}]),
    ?assertEqual([{'Int', 6}, {'Bool', true}, {'Int', 99}], Result).

rest_of_stack_two_arg_test() ->
    Def = word_def("add_r", ['Int', 'Int'], ['Int'], ["+"]),
    {ok, af_wc_test_rest2} = af_word_compiler:compile_words_to_module(af_wc_test_rest2, [Def]),
    Result = af_wc_test_rest2:add_r([{'Int', 3}, {'Int', 7}, {'Int', 42}]),
    ?assertEqual([{'Int', 10}, {'Int', 42}], Result).

%% ===================================================================
%% lookup_op_effect indirect test (via runtime_dispatch for side-effect ops)
%% ===================================================================

%% This tests the side_effect path in runtime_dispatch: when lookup_op_effect
%% returns {ok, _, 0, _}, the op is treated as side-effect-only
side_effect_op_test() ->
    setup(),
    %% "stack" is a side-effect op (prints stack, returns 0 outputs)
    Def = word_def("show_stack", ['Int'], ['Int'], ["stack"]),
    {ok, af_wc_test_se} = af_word_compiler:compile_words_to_module(af_wc_test_se, [Def]),
    Result = af_wc_test_se:show_stack([{'Int', 42}]),
    %% Side-effect ops keep the stack unchanged
    ?assertEqual([{'Int', 42}], Result).

%% ===================================================================
%% Edge cases
%% ===================================================================

%% Empty body word (identity function)
empty_body_identity_test() ->
    Def = word_def("noop", ['Int', 'Bool'], ['Int', 'Bool'], []),
    {ok, af_wc_test_noop} = af_word_compiler:compile_words_to_module(af_wc_test_noop, [Def]),
    ?assertEqual([{'Int', 5}, {'Bool', true}],
                 af_wc_test_noop:noop([{'Int', 5}, {'Bool', true}])).

%% Word with hyphenated name (common in ActorForth)
hyphenated_name_test() ->
    Def = word_def("add-one", ['Int'], ['Int'], ["1", "+"]),
    {ok, af_wc_test_hyph} = af_word_compiler:compile_words_to_module(af_wc_test_hyph, [Def]),
    ?assertEqual([{'Int', 6}], af_wc_test_hyph:'add-one'([{'Int', 5}])).

%% Multiple words with same name get grouped into multi-clause
multi_word_same_name_compile_test() ->
    Def1 = {"mw", [{'Int', 0}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "100"}]},
    Def2 = {"mw", [{'Int', 1}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "200"}]},
    Def3 = {"mw", ['Int'], ['Int'],
            [#operation{name = "drop"}, #operation{name = "300"}]},
    {ok, af_wc_test_mw} = af_word_compiler:compile_words_to_module(af_wc_test_mw, [Def1, Def2, Def3]),
    ?assertEqual([{'Int', 100}], af_wc_test_mw:mw([{'Int', 0}])),
    ?assertEqual([{'Int', 200}], af_wc_test_mw:mw([{'Int', 1}])),
    ?assertEqual([{'Int', 300}], af_wc_test_mw:mw([{'Int', 7}])).

%% Mixed multi-clause and distinct words in one module
mixed_words_module_test() ->
    Defs = [
        {"alpha", [{'Int', 0}], ['Int'], [#operation{name = "drop"}, #operation{name = "0"}]},
        {"beta", ['Int'], ['Int'], [#operation{name = "dup"}, #operation{name = "+"}]},
        {"alpha", ['Int'], ['Int'], [#operation{name = "dup"}, #operation{name = "*"}]}
    ],
    {ok, af_wc_test_mixed} = af_word_compiler:compile_words_to_module(af_wc_test_mixed, Defs),
    ?assertEqual([{'Int', 0}], af_wc_test_mixed:alpha([{'Int', 0}])),
    ?assertEqual([{'Int', 25}], af_wc_test_mixed:alpha([{'Int', 5}])),
    ?assertEqual([{'Int', 14}], af_wc_test_mixed:beta([{'Int', 7}])).
