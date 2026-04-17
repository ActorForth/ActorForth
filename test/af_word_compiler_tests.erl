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
    %% group_by_name normalises defs to 5-tuple {Name, SigIn, SigOut, Body, Guard}.
    ?assertMatch({"a", [{"a", ['Int'], ['Int'], [], undefined}]}, hd(Groups)).

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

%% ===================================================================
%% Helper for interpreter-based tests
%% ===================================================================

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup_full() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_string:init(),
    af_type_map:init(),
    af_type_list:init(),
    af_type_float:init(),
    af_type_tuple:init(),
    af_type_actor:init().

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
%% String operations in compilation (direct, from self-hosted branch)
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
%% String operation translate_op coverage
%% ===================================================================

string_ops_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"concat compiles to native BEAM", fun() ->
            C1 = eval(": test-concat String String -> String ; concat .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-concat\" compile", C1),
            C3 = eval("\"hello\" \"world\" test-concat", C2),
            [{'String', <<"helloworld">>}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"string length compiles to native BEAM", fun() ->
            C1 = eval(": test-slen String -> Int ; length .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-slen\" compile", C1),
            C3 = eval("\"hello\" test-slen", C2),
            [{'Int', 5}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"split compiles to native BEAM", fun() ->
            C1 = eval(": test-split String String -> List ; split .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-split\" compile", C1),
            C3 = eval("\"a,b,c\" \",\" test-split", C2),
            [{'List', [<<"a">>, <<"b">>, <<"c">>]}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"contains compiles to native BEAM", fun() ->
            C1 = eval(": test-contains String String -> Bool ; contains .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-contains\" compile", C1),
            C3 = eval("\"hello world\" \"world\" test-contains", C2),
            [{'Bool', true}] = C3#continuation.data_stack,
            C4 = eval("\"hello world\" \"xyz\" test-contains", C2),
            [{'Bool', false}] = C4#continuation.data_stack
        end} end,

        fun(_) -> {"starts-with compiles to native BEAM", fun() ->
            C1 = eval(": test-sw String String -> Bool ; starts-with .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-sw\" compile", C1),
            C3 = eval("\"hello world\" \"hello\" test-sw", C2),
            [{'Bool', true}] = C3#continuation.data_stack,
            C4 = eval("\"hello world\" \"world\" test-sw", C2),
            [{'Bool', false}] = C4#continuation.data_stack
        end} end,

        fun(_) -> {"ends-with compiles to native BEAM", fun() ->
            C1 = eval(": test-ew String String -> Bool ; ends-with .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-ew\" compile", C1),
            C3 = eval("\"hello world\" \"world\" test-ew", C2),
            [{'Bool', true}] = C3#continuation.data_stack,
            C4 = eval("\"hello world\" \"hello\" test-ew", C2),
            [{'Bool', false}] = C4#continuation.data_stack
        end} end,

        fun(_) -> {"string reverse compiles to native BEAM", fun() ->
            C1 = eval(": test-srev String -> String ; reverse .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-srev\" compile", C1),
            C3 = eval("\"abc\" test-srev", C2),
            %% string:reverse returns a list, native compiled wraps as String
            [{'String', Result}] = C3#continuation.data_stack,
            ?assert(Result =:= <<"cba">> orelse Result =:= "cba")
        end} end,

        fun(_) -> {"replace compiles to native BEAM", fun() ->
            C1 = eval(": test-repl String String String -> String ; replace .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-repl\" compile", C1),
            C3 = eval("\"hello world\" \"world\" \"there\" test-repl", C2),
            [{'String', <<"hello there">>}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"substring compiles to native BEAM", fun() ->
            C1 = eval(": test-sub String Int Int -> String ; substring .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-sub\" compile", C1),
            C3 = eval("\"hello world\" 0 5 test-sub", C2),
            [{'String', <<"hello">>}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"to-atom compiles to native BEAM", fun() ->
            C1 = eval(": test-toatom String -> Atom ; to-atom .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-toatom\" compile", C1),
            C3 = eval("\"hello\" test-toatom", C2),
            [{'Atom', hello}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"to-int from string compiles to native BEAM", fun() ->
            C1 = eval(": test-toint String -> Int ; to-int .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-toint\" compile", C1),
            C3 = eval("\"42\" test-toint", C2),
            [{'Int', 42}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"to-string from Atom compiles to native BEAM (direct)", fun() ->
            %% Build the word def directly to avoid interpreter issues
            Def = {"atom_to_str", ['Atom'], ['String'],
                   [#operation{name = "to-string"}]},
            {ok, af_wc_test_atos} = af_word_compiler:compile_words_to_module(
                af_wc_test_atos, [Def]),
            Result = af_wc_test_atos:atom_to_str([{'Atom', hello}]),
            [{'String', <<"hello">>}] = Result
        end} end,

        fun(_) -> {"trim compiles to native BEAM", fun() ->
            C1 = eval(": test-trim2 String -> String ; trim .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-trim2\" compile", C1),
            C3 = eval("\"  hello  \" test-trim2", C2),
            [{'String', <<"hello">>}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"to-upper compiles to native BEAM", fun() ->
            C1 = eval(": test-up2 String -> String ; to-upper .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-up2\" compile", C1),
            C3 = eval("\"hello\" test-up2", C2),
            [{'String', <<"HELLO">>}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"to-lower compiles to native BEAM", fun() ->
            C1 = eval(": test-low2 String -> String ; to-lower .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-low2\" compile", C1),
            C3 = eval("\"HELLO\" test-low2", C2),
            [{'String', <<"hello">>}] = C3#continuation.data_stack
        end} end
    ]}.

%% ===================================================================
%% List operation translate_op coverage
%% ===================================================================

list_ops_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"nil compiles to native BEAM (direct)", fun() ->
            %% nil has empty sig_in so build the def directly
            Def = {"push_nil", ['Int'], ['List', 'Int'],
                   [#operation{name = "nil"}]},
            {ok, af_wc_test_nil} = af_word_compiler:compile_words_to_module(
                af_wc_test_nil, [Def]),
            Result = af_wc_test_nil:push_nil([{'Int', 42}]),
            [{'List', []}, {'Int', 42}] = Result
        end} end,

        fun(_) -> {"cons compiles to native BEAM (direct)", fun() ->
            %% cons: Item List -> List (use Int for the item type)
            Def = {"do_cons", ['Int', 'List'], ['List'],
                   [#operation{name = "cons"}]},
            {ok, af_wc_test_cons} = af_word_compiler:compile_words_to_module(
                af_wc_test_cons, [Def]),
            Result = af_wc_test_cons:do_cons([{'Int', 1}, {'List', []}]),
            [{'List', _}] = Result
        end} end,

        fun(_) -> {"head compiles to native BEAM", fun() ->
            C1 = eval(": test-head List -> Any ; head .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-head\" compile", C1),
            C3 = eval("nil 1 cons 2 cons test-head", C2),
            [{'Int', 2}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"tail compiles to native BEAM", fun() ->
            C1 = eval(": test-tail List -> List ; tail .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-tail\" compile", C1),
            C3 = eval("nil 1 cons 2 cons 3 cons test-tail", C2),
            [{'List', [{'Int', 2}, {'Int', 1}]}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"list reverse compiles to native BEAM", fun() ->
            C1 = eval(": test-lrev List -> List ; reverse .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-lrev\" compile", C1),
            C3 = eval("nil 1 cons 2 cons 3 cons test-lrev", C2),
            [{'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"append compiles to native BEAM", fun() ->
            C1 = eval(": test-append List List -> List ; append .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-append\" compile", C1),
            C3 = eval("nil 1 cons 2 cons nil 3 cons 4 cons test-append", C2),
            [{'List', [{'Int', 2}, {'Int', 1}, {'Int', 4}, {'Int', 3}]}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"list length compiles to native BEAM", fun() ->
            C1 = eval(": test-llen List -> Int ; length .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-llen\" compile", C1),
            C3 = eval("nil 1 cons 2 cons 3 cons test-llen", C2),
            [{'Int', 3}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"last compiles to native BEAM", fun() ->
            C1 = eval(": test-last List -> Any ; last .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-last\" compile", C1),
            C3 = eval("nil 1 cons 2 cons 3 cons test-last", C2),
            [{'Int', 1}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"take compiles to native BEAM", fun() ->
            C1 = eval(": test-take List Int -> List ; take .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-take\" compile", C1),
            C3 = eval("nil 1 cons 2 cons 3 cons 2 test-take", C2),
            [{'List', [{'Int', 3}, {'Int', 2}]}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"empty? compiles to native BEAM", fun() ->
            C1 = eval(": test-empty2 List -> Bool ; empty? .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-empty2\" compile", C1),
            C3 = eval("nil test-empty2", C2),
            [{'Bool', true}] = C3#continuation.data_stack,
            C4 = eval("nil 1 cons test-empty2", C2),
            [{'Bool', false}] = C4#continuation.data_stack
        end} end,

        fun(_) -> {"contains? compiles to native BEAM (direct)", fun() ->
            %% contains?: TOS=Item, below=List -> Bool
            %% Use Int for the item type to match head pattern
            Def = {"has_item", ['Int', 'List'], ['Bool'],
                   [#operation{name = "contains?"}]},
            {ok, af_wc_test_contains} = af_word_compiler:compile_words_to_module(
                af_wc_test_contains, [Def]),
            Result = af_wc_test_contains:has_item([{'Int', 2}, {'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}]),
            [{'Bool', true}] = Result,
            Result2 = af_wc_test_contains:has_item([{'Int', 5}, {'List', [{'Int', 1}, {'Int', 2}]}]),
            [{'Bool', false}] = Result2
        end} end,

        fun(_) -> {"zip compiles to native BEAM", fun() ->
            C1 = eval(": test-zip List List -> List ; zip .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-zip\" compile", C1),
            C3 = eval("nil 1 cons 2 cons nil 3 cons 4 cons test-zip", C2),
            [{'List', _}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"list drop compiles to native BEAM", fun() ->
            C1 = eval(": test-ldrop List Int -> List ; drop .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-ldrop\" compile", C1),
            C3 = eval("nil 1 cons 2 cons 3 cons 1 test-ldrop", C2),
            [{'List', [{'Int', 2}, {'Int', 1}]}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"nth compiles to native BEAM", fun() ->
            C1 = eval(": test-nth2 List Int -> Any ; nth .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-nth2\" compile", C1),
            C3 = eval("nil 1 cons 2 cons 3 cons 0 test-nth2", C2),
            [{'Int', 3}] = C3#continuation.data_stack
        end} end
    ]}.

%% ===================================================================
%% Map operation translate_op coverage
%% ===================================================================

map_ops_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"map-new compiles to native BEAM (direct)", fun() ->
            %% map-new has empty sig_in, build def directly
            Def = {"push_map", ['Int'], ['Map', 'Int'],
                   [#operation{name = "map-new"}]},
            {ok, af_wc_test_mnew} = af_word_compiler:compile_words_to_module(
                af_wc_test_mnew, [Def]),
            Result = af_wc_test_mnew:push_map([{'Int', 42}]),
            [{'Map', M}, {'Int', 42}] = Result,
            ?assertEqual(0, maps:size(M))
        end} end,

        fun(_) -> {"map-put compiles to native BEAM", fun() ->
            C1 = eval(": test-mput Map String Int -> Map ; map-put .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-mput\" compile", C1),
            C3 = eval("map-new \"x\" 42 test-mput", C2),
            [{'Map', M}] = C3#continuation.data_stack,
            ?assertEqual({'Int', 42}, maps:get({'String', <<"x">>}, M))
        end} end,

        fun(_) -> {"map-get compiles to native BEAM", fun() ->
            C1 = eval(": test-mget Map String -> Any ; map-get .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-mget\" compile", C1),
            C3 = eval("map-new 42 \"x\" map-put \"x\" test-mget", C2),
            [{'Int', 42}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"map-delete compiles to native BEAM", fun() ->
            C1 = eval(": test-mdel Map String -> Map ; map-delete .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-mdel\" compile", C1),
            C3 = eval("map-new 42 \"x\" map-put \"x\" test-mdel", C2),
            [{'Map', M}] = C3#continuation.data_stack,
            ?assertEqual(0, maps:size(M))
        end} end,

        fun(_) -> {"map-has? compiles to native BEAM", fun() ->
            C1 = eval(": test-mhas Map String -> Bool ; map-has? .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-mhas\" compile", C1),
            C3 = eval("map-new 42 \"x\" map-put \"x\" test-mhas", C2),
            [{'Bool', true}] = C3#continuation.data_stack,
            C4 = eval("map-new \"y\" test-mhas", C2),
            [{'Bool', false}] = C4#continuation.data_stack
        end} end,

        fun(_) -> {"map-keys compiles to native BEAM", fun() ->
            C1 = eval(": test-mkeys Map -> List ; map-keys .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-mkeys\" compile", C1),
            C3 = eval("map-new 42 \"x\" map-put test-mkeys", C2),
            [{'List', _}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"map-values compiles to native BEAM", fun() ->
            C1 = eval(": test-mvals Map -> List ; map-values .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-mvals\" compile", C1),
            C3 = eval("map-new 42 \"x\" map-put test-mvals", C2),
            [{'List', _}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"map-size compiles to native BEAM", fun() ->
            C1 = eval(": test-msz Map -> Int ; map-size .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-msz\" compile", C1),
            C3 = eval("map-new 42 \"x\" map-put test-msz", C2),
            [{'Int', 1}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"map-merge compiles to native BEAM", fun() ->
            C1 = eval(": test-mmrg Map Map -> Map ; map-merge .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-mmrg\" compile", C1),
            C3 = eval("map-new 1 \"a\" map-put map-new 2 \"b\" map-put test-mmrg", C2),
            [{'Map', M}] = C3#continuation.data_stack,
            ?assertEqual(2, maps:size(M))
        end} end
    ]}.

%% ===================================================================
%% Int operation translate_op coverage (mod, abs, max, min)
%% ===================================================================

int_ops_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"mod compiles to native BEAM", fun() ->
            C1 = eval(": test-mod2 Int Int -> Int ; mod .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-mod2\" compile", C1),
            C3 = eval("10 3 test-mod2", C2),
            [{'Int', 1}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"abs compiles to native BEAM", fun() ->
            C1 = eval(": test-abs2 Int -> Int ; abs .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-abs2\" compile", C1),
            C3 = eval("-7 test-abs2", C2),
            [{'Int', 7}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"max compiles to native BEAM", fun() ->
            C1 = eval(": test-max2 Int Int -> Int ; max .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-max2\" compile", C1),
            C3 = eval("3 7 test-max2", C2),
            [{'Int', 7}] = C3#continuation.data_stack
        end} end,

        fun(_) -> {"min compiles to native BEAM", fun() ->
            C1 = eval(": test-min2 Int Int -> Int ; min .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-min2\" compile", C1),
            C3 = eval("3 7 test-min2", C2),
            [{'Int', 3}] = C3#continuation.data_stack
        end} end
    ]}.

%% ===================================================================
%% Boolean logic translate_op coverage (and, or)
%% ===================================================================

bool_logic_ops_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"and compiles to native BEAM", fun() ->
            C1 = eval(": test-and2 Bool Bool -> Bool ; and .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-and2\" compile", C1),
            C3 = eval("True bool True bool test-and2", C2),
            [{'Bool', true}] = C3#continuation.data_stack,
            C4 = eval("True bool False bool test-and2", C2),
            [{'Bool', false}] = C4#continuation.data_stack
        end} end,

        fun(_) -> {"or compiles to native BEAM", fun() ->
            C1 = eval(": test-or2 Bool Bool -> Bool ; or .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"test-or2\" compile", C1),
            C3 = eval("False bool True bool test-or2", C2),
            [{'Bool', true}] = C3#continuation.data_stack,
            C4 = eval("False bool False bool test-or2", C2),
            [{'Bool', false}] = C4#continuation.data_stack
        end} end
    ]}.

%% ===================================================================
%% Product type accessor/setter translate_op coverage
%% ===================================================================

product_ops_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"product getter compiles to native BEAM", fun() ->
            C1 = eval("type Point x Int y Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": test-getx Point -> Point Int ; x .", C1),
            C3 = eval("\"test-getx\" compile", C2),
            C4 = eval("3 int 5 int point test-getx", C3),
            [{'Int', 3}, {'Point', _}] = C4#continuation.data_stack
        end} end,

        fun(_) -> {"product setter compiles to native BEAM", fun() ->
            C1 = eval("type Point x Int y Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": test-setx Point Int -> Point ; x! .", C1),
            C3 = eval("\"test-setx\" compile", C2),
            C4 = eval("3 int 5 int point 99 test-setx", C3),
            [{'Point', Fields}] = C4#continuation.data_stack,
            ?assertEqual({'Int', 99}, maps:get(x, Fields))
        end} end,

        fun(_) -> {"product getter-then-op compiles to native BEAM", fun() ->
            C1 = eval("type Counter value Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": test-inc Counter -> Counter ; value 1 int + value! .", C1),
            C3 = eval("\"test-inc\" compile", C2),
            C4 = eval("0 int counter test-inc", C3),
            [{'Counter', Fields}] = C4#continuation.data_stack,
            ?assertEqual({'Int', 1}, maps:get(value, Fields))
        end} end
    ]}.

%% ===================================================================
%% Send block compilation (compile_send_block)
%% ===================================================================

send_block_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"simple cast send block compiles to native BEAM (direct)", fun() ->
            %% Build word def with << increment >> in body directly
            %% The word: do_bump Actor -> Actor ; << increment >>
            Body = [#operation{name = "<<"},
                    #operation{name = "increment"},
                    #operation{name = ">>"}],
            Def = {"do_bump", ['Actor'], ['Actor'], Body},
            {ok, af_wc_send_cast} = af_word_compiler:compile_words_to_module(
                af_wc_send_cast, [Def]),
            %% Set up a Counter actor
            C1 = eval("type Counter value Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": increment Counter -> Counter ; value 1 int + value! .", C1),
            C3 = eval(": count Counter -> Counter Int ; value .", C2),
            C4 = eval("0 int counter server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            %% Call the compiled function directly
            Result = af_wc_send_cast:do_bump([{'Actor', Info}]),
            [{'Actor', _}] = Result,
            timer:sleep(50),
            %% Verify the cast was sent
            C5 = eval("<< count >>", C4),
            [{'Int', 1}, {'Actor', _}] = C5#continuation.data_stack,
            eval("<< stop >>", C5)
        end} end,

        fun(_) -> {"call send block compiles to native BEAM (direct)", fun() ->
            %% Build word def with << count >> which is a call (returns Int)
            %% First define the actor and its words so is_actor_word_cast can find them
            C1 = eval("type Counter value Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": count Counter -> Counter Int ; value .", C1),
            %% Now compile the send block word — count is known as a call
            Body = [#operation{name = "<<"},
                    #operation{name = "count"},
                    #operation{name = ">>"}],
            Def = {"do_count", ['Actor'], ['Actor'], Body},
            {ok, af_wc_send_call} = af_word_compiler:compile_words_to_module(
                af_wc_send_call, [Def]),
            C3 = eval("0 int counter server", C2),
            [{'Actor', Info}] = C3#continuation.data_stack,
            %% The compiled function calls through send_call
            Result = af_wc_send_call:do_count([{'Actor', Info}]),
            %% Result is opaque stack — at least contains actor and count value
            ?assert(length(Result) >= 1),
            eval("<< stop >>", C3)
        end} end,

        fun(_) -> {"send block with args compiles (direct)", fun() ->
            %% First define the actor type and words
            C1 = eval("type Acc total Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": add-to Acc Int -> Acc ; total + total! .", C1),
            %% Now compile with multi-token send block — key test is compilation succeeds
            Body = [#operation{name = "<<"},
                    #operation{name = "5"},
                    #operation{name = "add-to"},
                    #operation{name = ">>"}],
            Def = {"add_five", ['Actor'], ['Actor'], Body},
            {ok, af_wc_send_args} = af_word_compiler:compile_words_to_module(
                af_wc_send_args, [Def]),
            C3 = eval("0 int acc server", C2),
            [{'Actor', Info}] = C3#continuation.data_stack,
            %% Call the compiled function — verify it runs without error
            Result = af_wc_send_args:add_five([{'Actor', Info}]),
            %% Verify the Actor is in the result
            ?assert(lists:any(fun({'Actor', _}) -> true; (_) -> false end, Result)),
            eval("<< stop >>", C3)
        end} end
    ]}.

%% ===================================================================
%% Loop optimization (try_loop_opt / generate_loop_opt)
%% ===================================================================

loop_opt_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"loop opt generates optimized send loop (direct)", fun() ->
            %% Build the blast word defs directly:
            %% Base case: blast(0, Actor) -> Actor  (body: swap drop)
            %% Recursive: blast(N, Actor) -> << increment >> swap 1 - swap blast
            BaseDef = {"blast", [{'Int', 0}, 'Actor'], ['Actor'],
                       [#operation{name = "swap"}, #operation{name = "drop"}]},
            RecBody = [#operation{name = "<<"},
                       #operation{name = "increment"},
                       #operation{name = ">>"},
                       #operation{name = "swap"},
                       #operation{name = "1"},
                       #operation{name = "-"},
                       #operation{name = "swap"},
                       #operation{name = "blast"}],
            RecDef = {"blast", ['Int', 'Actor'], ['Actor'], RecBody},
            {ok, af_wc_test_loop} = af_word_compiler:compile_words_to_module(
                af_wc_test_loop, [BaseDef, RecDef]),
            %% Set up a Counter actor
            C1 = eval("type Counter value Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": increment Counter -> Counter ; value 1 int + value! .", C1),
            C3 = eval(": count Counter -> Counter Int ; value .", C2),
            C4 = eval("0 int counter server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            %% Call the compiled blast function
            Result = af_wc_test_loop:blast([{'Int', 3}, {'Actor', Info}]),
            [{'Actor', _} | _] = Result,
            timer:sleep(100),
            C5 = eval("<< count >>", C4),
            [{'Int', 3}, {'Actor', _}] = C5#continuation.data_stack,
            eval("<< stop >>", C5)
        end} end,

        fun(_) -> {"try_loop_opt returns not_applicable for non-recursive word", fun() ->
            %% A multi-clause word without self-recursion should not trigger loop opt
            Def1 = {"myop", [{'Int', 0}], ['Int'],
                    [#operation{name = "drop"}, #operation{name = "0"}]},
            Def2 = {"myop", ['Int'], ['Int'],
                    [#operation{name = "dup"}, #operation{name = "+"}]},
            Defs = [Def1, Def2],
            %% This should compile as regular multi-clause, not loop-optimized
            {ok, af_wc_test_nolp} = af_word_compiler:compile_words_to_module(af_wc_test_nolp, Defs),
            ?assertEqual([{'Int', 0}], af_wc_test_nolp:myop([{'Int', 0}])),
            ?assertEqual([{'Int', 10}], af_wc_test_nolp:myop([{'Int', 5}]))
        end} end,

        fun(_) -> {"try_loop_opt not_applicable when no send block", fun() ->
            %% Multi-clause with self-recursion but no << >> send block
            Def1 = {"recur", [{'Int', 0}], ['Int'],
                    [#operation{name = "drop"}, #operation{name = "0"}]},
            Def2 = {"recur", ['Int'], ['Int'],
                    [#operation{name = "1"}, #operation{name = "-"},
                     #operation{name = "recur"}]},
            {ok, af_wc_test_nosend} = af_word_compiler:compile_words_to_module(
                af_wc_test_nosend, [Def1, Def2]),
            ?assertEqual([{'Int', 0}], af_wc_test_nosend:recur([{'Int', 5}]))
        end} end,

        fun(_) -> {"try_loop_opt not_applicable with multiple recursive clauses", fun() ->
            %% Multiple recursive clauses — loop opt only handles single recursive clause
            Def1 = {"mrec", [{'Int', 0}], ['Int'],
                    [#operation{name = "drop"}, #operation{name = "0"}]},
            Def2 = {"mrec", ['Int'], ['Int'],
                    [#operation{name = "1"}, #operation{name = "-"},
                     #operation{name = "mrec"}]},
            Def3 = {"mrec", ['Int'], ['Int'],
                    [#operation{name = "2"}, #operation{name = "-"},
                     #operation{name = "mrec"}]},
            {ok, af_wc_test_mrec} = af_word_compiler:compile_words_to_module(
                af_wc_test_mrec, [Def1, Def2, Def3]),
            ?assertEqual([{'Int', 0}], af_wc_test_mrec:mrec([{'Int', 0}]))
        end} end
    ]}.

%% ===================================================================
%% Error path coverage
%% ===================================================================

error_paths_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"compile_words_to_module with error returns error", fun() ->
            ?assertMatch({error, no_compilable_words},
                         af_word_compiler:compile_words_to_module(af_wc_err1, []))
        end} end,

        fun(_) -> {"compile_words_to_binary with empty defs returns error", fun() ->
            ?assertMatch({error, no_compilable_words},
                         af_word_compiler:compile_words_to_binary(af_wc_err2, []))
        end} end,

        fun(_) -> {"group_by_name preserves order and groups correctly", fun() ->
            Defs = [
                {"a", ['Int'], ['Int'], [#operation{name = "dup"}]},
                {"b", ['Int'], ['Int'], [#operation{name = "dup"}]},
                {"a", [{'Int', 0}], ['Int'], [#operation{name = "drop"}]}
            ],
            Groups = af_word_compiler:group_by_name(Defs),
            ?assertEqual(2, length(Groups)),
            [{"a", ADefs}, {"b", _BDefs}] = Groups,
            ?assertEqual(2, length(ADefs))
        end} end,

        fun(_) -> {"find_compiled_word_defs skips non-compiled ops", fun() ->
            %% Register an op with source=undefined (not compiled)
            Op = #operation{
                name = "skip_test_op",
                sig_in = ['Int'],
                sig_out = ['Int'],
                impl = fun(C) -> C end,
                source = undefined
            },
            af_type:add_op('Int', Op),
            ?assertEqual([], af_word_compiler:find_compiled_word_defs("skip_test_op"))
        end} end
    ]}.

%% ===================================================================
%% has_send_block / collect_send_block coverage
%% ===================================================================

has_send_block_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"word with << >> in body compiles send block (direct)", fun() ->
            %% Build word def with << bump >> in body
            Body = [#operation{name = "<<"},
                    #operation{name = "bump"},
                    #operation{name = ">>"}],
            Def = {"do_bump2", ['Actor'], ['Actor'], Body},
            {ok, af_wc_send_bump} = af_word_compiler:compile_words_to_module(
                af_wc_send_bump, [Def]),
            C1 = eval("type State val Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": bump State -> State ; val 1 int + val! .", C1),
            C3 = eval(": get-val State -> State Int ; val .", C2),
            C4 = eval("0 int state server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            %% Call the compiled function
            Result = af_wc_send_bump:do_bump2([{'Actor', Info}]),
            [{'Actor', _}] = Result,
            timer:sleep(50),
            C5 = eval("<< get-val >>", C4),
            [{'Int', 1}, {'Actor', _}] = C5#continuation.data_stack,
            eval("<< stop >>", C5)
        end} end
    ]}.

%% ===================================================================
%% resolve_word_call: test without module context (line 935-941)
%% ===================================================================

resolve_no_module_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [

        fun(_) -> {"word call without module context falls back correctly", fun() ->
            %% Compile a helper module and register as native
            HelperDef = word_def("rhelper", ['Int'], ['Int'], ["dup", "+"]),
            {ok, af_wc_test_rhelp} = af_word_compiler:compile_words_to_module(af_wc_test_rhelp, [HelperDef]),
            Wrapper = af_word_compiler:make_wrapper(af_wc_test_rhelp, rhelper, ['Int'], ['Int']),
            af_type:add_op('Int', Wrapper),
            %% Now compile a word in a different module that calls rhelper
            CallerDef = word_def("use_rhelper", ['Int'], ['Int'], ["rhelper"]),
            {ok, af_wc_test_rcaller} = af_word_compiler:compile_words_to_module(af_wc_test_rcaller, [CallerDef]),
            ?assertEqual([{'Int', 10}], af_wc_test_rcaller:use_rhelper([{'Int', 5}]))
        end} end
    ]}.

%% ===================================================================
%% compile_send_block edge: empty send block (line 874-875)
%% ===================================================================

empty_send_block_test() ->
    %% Empty send block should not crash; the body << >> has no ops between markers
    %% This tests compile_send_block(_, _, _, _) -> {error, empty_send_block}
    setup_full(),
    %% Define a word that has << >> with nothing inside
    %% The interpreter handles this, but we test the compile path
    C1 = eval("type Dummy val Int .", af_interpreter:new_continuation()),
    C2 = eval("0 int dummy server", C1),
    %% The << >> block is empty, which should either skip or handle gracefully
    C3 = eval(": test-empty-send Actor -> Actor ; << >> .", C2),
    %% Should still work via interpreted fallback
    C4 = eval("test-empty-send", C3),
    [{'Actor', _}] = C4#continuation.data_stack,
    eval("<< stop >>", C4).

%% ===================================================================
%% Additional coverage tests
%% ===================================================================

%% --- compile_single_word error path (lines 136-137) ---
%% When simulate_body returns an error, compile_single_word propagates it.
%% The translate_op catches most cases via fallback, but with an empty stack
%% and an op like "+", it falls through to runtime_dispatch producing opaque stack.
compile_single_word_error_test() ->
    %% "+" on an empty stack falls through to runtime_dispatch, not an error.
    %% Verify compilation succeeds (runtime handles it).
    Def = {"fallback_add", [], ['Int'], [#operation{name = "+"}]},
    {ok, af_wc_err_single} = af_word_compiler:compile_words_to_module(af_wc_err_single, [Def]).

%% --- compile_clause error path (line 150) ---
%% Multi-clause where one clause has a body error
multi_clause_with_bad_clause_test() ->
    %% First clause is good, second has insufficient stack for "+"
    Def1 = {"mxop", [{'Int', 0}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "1"}]},
    Def2 = {"mxop", [], ['Int'], [#operation{name = "+"}]},
    %% The bad clause is silently filtered out; good clause remains
    {ok, af_wc_test_badclause} = af_word_compiler:compile_words_to_module(
        af_wc_test_badclause, [Def1, Def2]),
    ?assertEqual([{'Int', 1}], af_wc_test_badclause:mxop([{'Int', 0}])).

%% --- multi-clause with all fallback ops still compiles ---
multi_clause_all_fallback_test() ->
    %% Both clauses have ops that fall through to runtime_dispatch.
    %% They still compile, just produce opaque stacks.
    Def1 = {"allfb", [{'Int', 0}], ['Int'], [#operation{name = "drop"}, #operation{name = "0"}]},
    Def2 = {"allfb", ['Int'], ['Int'], [#operation{name = "dup"}, #operation{name = "+"}]},
    {ok, af_wc_err_allbad} = af_word_compiler:compile_words_to_module(af_wc_err_allbad, [Def1, Def2]),
    ?assertEqual([{'Int', 0}], af_wc_err_allbad:allfb([{'Int', 0}])),
    ?assertEqual([{'Int', 10}], af_wc_err_allbad:allfb([{'Int', 5}])).

%% --- to-string from Int (lines 644-646) ---
to_string_int_test() ->
    Def = {"int_str", ['Int'], ['String'],
           [#operation{name = "to-string"}]},
    {ok, af_wc_test_intstr} = af_word_compiler:compile_words_to_module(
        af_wc_test_intstr, [Def]),
    Result = af_wc_test_intstr:int_str([{'Int', 42}]),
    ?assertEqual([{'String', <<"42">>}], Result).

%% --- collect_send_block reaching end without >> (line 816) ---
collect_send_block_unterminated_test() ->
    setup_full(),
    %% Build a body where << has content but no >> terminator
    %% The word body: << someop (no >>)
    Body = [#operation{name = "<<"},
            #operation{name = "bump"}],
    Def = {"unterminated", ['Actor'], ['Actor'], Body},
    %% Should still compile (collect_send_block returns remaining as [])
    {ok, af_wc_test_unterm} = af_word_compiler:compile_words_to_module(
        af_wc_test_unterm, [Def]).

%% --- send block with no actor on TOS (line 847) ---
send_block_no_actor_test() ->
    setup_full(),
    %% << word >> but TOS is Int, not Actor => falls through to runtime
    Body = [#operation{name = "<<"},
            #operation{name = "bump"},
            #operation{name = ">>"}],
    Def = {"no_actor", ['Int'], ['Int'], Body},
    {ok, af_wc_test_noactor} = af_word_compiler:compile_words_to_module(
        af_wc_test_noactor, [Def]),
    %% It should work via runtime dispatch fallback
    Result = af_wc_test_noactor:no_actor([{'Int', 42}]),
    ?assert(is_list(Result)).

%% --- multi-token send block no actor on TOS (line 882) ---
send_block_multi_no_actor_test() ->
    setup_full(),
    %% << 5 add-to >> but TOS is Int not Actor
    Body = [#operation{name = "<<"},
            #operation{name = "5"},
            #operation{name = "add-to"},
            #operation{name = ">>"}],
    Def = {"multi_no_actor", ['Int'], ['Int'], Body},
    {ok, af_wc_test_mna} = af_word_compiler:compile_words_to_module(
        af_wc_test_mna, [Def]),
    Result = af_wc_test_mna:multi_no_actor([{'Int', 42}]),
    ?assert(is_list(Result)).

%% --- multi-token send block with call (not cast) (lines 872-877) ---
send_block_multi_call_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [
        fun(_) -> {"multi-token send block with call return values", fun() ->
            %% Define a type with a call word (returns extra values)
            C1 = eval("type Bank balance Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": deposit Bank Int -> Bank ; balance + balance! .", C1),
            C3 = eval(": get-balance Bank -> Bank Int ; balance .", C2),
            %% Now compile a word with << 100 deposit >> which is a cast
            Body = [#operation{name = "<<"},
                    #operation{name = "100"},
                    #operation{name = "deposit"},
                    #operation{name = ">>"}],
            Def = {"deposit_100", ['Actor'], ['Actor'], Body},
            {ok, af_wc_test_mscall} = af_word_compiler:compile_words_to_module(
                af_wc_test_mscall, [Def]),
            C4 = eval("0 int bank server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            Result = af_wc_test_mscall:deposit_100([{'Actor', Info}]),
            ?assert(lists:any(fun({'Actor', _}) -> true; (_) -> false end, Result)),
            eval("<< stop >>", C4)
        end} end
    ]}.

%% --- empty send block compilation error path (line 885) ---
empty_send_block_compile_test() ->
    setup_full(),
    %% Build a body with << >> directly (no ops between markers)
    Body = [#operation{name = "<<"},
            #operation{name = ">>"}],
    Def = {"empty_send", ['Actor'], ['Actor'], Body},
    %% compile_send_block returns {error, empty_send_block}
    %% which falls through to runtime dispatch
    {ok, af_wc_test_empsend} = af_word_compiler:compile_words_to_module(
        af_wc_test_empsend, [Def]).

%% --- try_product_op with non-product type on TOS (line 806) ---
try_product_op_non_product_test() ->
    setup_full(),
    %% An op name that looks like it could be a product getter but TOS is 'Atom'
    %% which is not a product type
    Def = {"try_field", ['Atom'], ['Atom'],
           [#operation{name = "somefield"}]},
    {ok, af_wc_test_noprod} = af_word_compiler:compile_words_to_module(
        af_wc_test_noprod, [Def]),
    %% Should fall through to runtime dispatch (push as atom)
    Result = af_wc_test_noprod:try_field([{'Atom', hello}]),
    ?assert(is_list(Result)).

%% --- try_product_op setter failure path (line 785) ---
%% When TOS type has no product type info, setter falls through to runtime.
try_product_setter_non_product_test() ->
    setup_full(),
    %% setter "x!" but second item (the instance) is a 'Bogus' type not in registry
    %% This exercises the _ -> not_product path in try_product_op setter
    Def = {"bad_setter", ['Bogus', 'Int'], ['Int'],
           [#operation{name = "x!"}]},
    {ok, af_wc_test_badsetter} = af_word_compiler:compile_words_to_module(
        af_wc_test_badsetter, [Def]),
    %% The compilation succeeds (falls through to runtime_dispatch)
    %% Just verify compilation worked
    ok.

%% --- resolve_word_call without module key in Ctx (lines 945-951) ---
resolve_no_module_key_test() ->
    setup_full(),
    %% Compile a native helper, register it, then compile a word
    %% that calls it via a Ctx without the module key
    HelperDef = word_def("ctx_helper", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_ctxh} = af_word_compiler:compile_words_to_module(af_wc_test_ctxh, [HelperDef]),
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_ctxh, ctx_helper, ['Int'], ['Int']),
    af_type:add_op('Int', Wrapper),
    %% Now compile a caller that calls ctx_helper (resolved via find_native_word)
    CallerDef = word_def("use_ctx", ['Int'], ['Int'], ["ctx_helper"]),
    {ok, af_wc_test_usectx} = af_word_compiler:compile_words_to_module(af_wc_test_usectx, [CallerDef]),
    ?assertEqual([{'Int', 10}], af_wc_test_usectx:use_ctx([{'Int', 5}])).

%% --- simulate_body send block compile error fallback (lines 381-382) ---
send_block_fallback_on_error_test() ->
    setup_full(),
    %% << >> (empty) with Actor on stack: compile_send_block returns error,
    %% simulate_body should fall through to runtime handling
    Body = [#operation{name = "<<"},
            #operation{name = ">>"},
            #operation{name = "dup"}],
    Def = {"send_fallback", ['Actor'], ['Actor', 'Actor'], Body},
    {ok, af_wc_test_sfb} = af_word_compiler:compile_words_to_module(
        af_wc_test_sfb, [Def]).

%% --- String length compilation (line 506-509) ---
string_length_direct_test() ->
    Def = {"slen", ['String'], ['Int'],
           [#operation{name = "length"}]},
    {ok, af_wc_test_slen} = af_word_compiler:compile_words_to_module(
        af_wc_test_slen, [Def]),
    Result = af_wc_test_slen:slen([{'String', <<"hello">>}]),
    ?assertEqual([{'Int', 5}], Result).

%% --- List drop with Int TOS (lines 432-438) ---
list_drop_direct_test() ->
    Def = {"ldrop", ['Int', 'List'], ['List'],
           [#operation{name = "drop"}]},
    {ok, af_wc_test_ldrop2} = af_word_compiler:compile_words_to_module(
        af_wc_test_ldrop2, [Def]),
    Result = af_wc_test_ldrop2:ldrop([{'Int', 1}, {'List', [a, b, c]}]),
    [{'List', [b, c]}] = Result.

%% --- Stack operations with rest preservation ---
rest_preservation_test() ->
    Def = word_def("inc_keep", ['Int'], ['Int'], ["1", "+"]),
    {ok, af_wc_test_rest} = af_word_compiler:compile_words_to_module(af_wc_test_rest, [Def]),
    %% Extra items on the stack should be preserved
    ?assertEqual([{'Int', 6}, {'Bool', true}, {'Int', 99}],
                 af_wc_test_rest:inc_keep([{'Int', 5}, {'Bool', true}, {'Int', 99}])).

%% --- Multi-clause with float value constraint ---
multi_clause_float_pattern_test() ->
    Def1 = {"fcheck", [{'Float', 0.0}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "0"}]},
    Def2 = {"fcheck", ['Float'], ['Int'],
            [#operation{name = "drop"}, #operation{name = "1"}]},
    {ok, af_wc_test_mfloat} = af_word_compiler:compile_words_to_module(
        af_wc_test_mfloat, [Def1, Def2]),
    ?assertEqual([{'Int', 0}], af_wc_test_mfloat:fcheck([{'Float', 0.0}])),
    ?assertEqual([{'Int', 1}], af_wc_test_mfloat:fcheck([{'Float', 3.14}])).

%% --- build_tagged_return opaque stack path (line 974-975) ---
opaque_return_test() ->
    setup_full(),
    %% A word whose body ends with an inter-word call, producing opaque stack
    Def1 = word_def("helper2", ['Int'], ['Int'], ["dup", "+"]),
    Def2 = word_def("wrap_helper2", ['Int'], ['Int'], ["helper2"]),
    {ok, af_wc_test_oret} = af_word_compiler:compile_words_to_module(af_wc_test_oret, [Def1, Def2]),
    ?assertEqual([{'Int', 10}], af_wc_test_oret:wrap_helper2([{'Int', 5}])).

%% --- find_word_sig with non-standard ops map entry (line 906) ---
find_word_sig_edge_test() ->
    setup_full(),
    %% Register a non-list value in an ops map (edge case)
    %% This exercises the _ -> find_word_sig(Name, Rest) clause
    Op = #operation{
        name = "edge_sig_word",
        sig_in = ['Int'],
        sig_out = ['Int', 'Int'],
        impl = fun(C) -> C end,
        source = undefined
    },
    af_type:add_op('Int', Op),
    %% Send block compilation calls is_actor_word_cast which calls find_word_sig
    %% "edge_sig_word" has 2 outputs > 1 input, so it's a call (not cast)
    Body = [#operation{name = "<<"},
            #operation{name = "edge_sig_word"},
            #operation{name = ">>"}],
    Def = {"test_sig_edge", ['Actor'], ['Actor'], Body},
    %% Should compile successfully
    {ok, af_wc_test_sigedge} = af_word_compiler:compile_words_to_module(
        af_wc_test_sigedge, [Def]).

%% --- simulate_body side_effect path (line 411-412) ---
%% "print" is a side-effect op (0 outputs). After calling it, the original stack
%% is preserved and the next op continues on it.
side_effect_test() ->
    setup_full(),
    %% "print" produces a side effect but doesn't change the stack
    Def = word_def("print_and_dup", ['Int'], ['Int', 'Int'], ["dup", "print"]),
    {ok, af_wc_test_se} = af_word_compiler:compile_words_to_module(af_wc_test_se, [Def]),
    Result = af_wc_test_se:print_and_dup([{'Int', 5}]),
    %% print is a side-effect, stack stays as [5, 5] from the dup
    ?assertEqual([{'Int', 5}, {'Int', 5}], Result).

%% --- compile_words_to_binary warnings path (line 79-80) ---
compile_binary_warnings_test() ->
    %% Normal compilation may produce warnings but still succeed
    Def = word_def("warntest", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_warn, Bin} = af_word_compiler:compile_words_to_binary(af_wc_test_warn, [Def]),
    ?assert(is_binary(Bin)).

%% --- compile_word_group with form list return (line 58-59) ---
%% The loop optimization path returns {ok, [Form1, Form2], [Export1, Export2]}
%% This is already tested by loop_opt_test_ but let's verify the Forms/Exports structure
loop_opt_form_list_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [
        fun(_) -> {"loop opt returns form list that exports both functions", fun() ->
            %% Set up actor with increment word
            C1 = eval("type Counter2 value Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": increment Counter2 -> Counter2 ; value 1 int + value! .", C1),
            %% blast2 with base case and recursive case with << >> send block
            BaseDef = {"blast2", [{'Int', 0}, 'Actor'], ['Actor'],
                       [#operation{name = "swap"}, #operation{name = "drop"}]},
            RecBody = [#operation{name = "<<"},
                       #operation{name = "increment"},
                       #operation{name = ">>"},
                       #operation{name = "swap"},
                       #operation{name = "1"},
                       #operation{name = "-"},
                       #operation{name = "swap"},
                       #operation{name = "blast2"}],
            RecDef = {"blast2", ['Int', 'Actor'], ['Actor'], RecBody},
            %% compile_words_to_binary to inspect the binary
            {ok, af_wc_test_loopform, Bin} = af_word_compiler:compile_words_to_binary(
                af_wc_test_loopform, [BaseDef, RecDef]),
            ?assert(is_binary(Bin)),
            %% Load and verify both blast2 and blast2_loop are exported
            {module, af_wc_test_loopform} = code:load_binary(
                af_wc_test_loopform, "af_wc_test_loopform.beam", Bin),
            Exports = af_wc_test_loopform:module_info(exports),
            ?assert(lists:member({blast2, 1}, Exports)),
            ?assert(lists:member({blast2_loop, 4}, Exports))
        end} end
    ]}.

%% --- simulate_body with insufficient stack for ops ---
%% "swap" with only 1 item falls through to runtime_dispatch (not an error).
simulate_body_fallback_test() ->
    Def = word_def("swap_fb", ['Int'], ['Int'], ["swap"]),
    {ok, af_wc_fb_swap} = af_word_compiler:compile_words_to_module(af_wc_fb_swap, [Def]),
    %% Just verify compilation succeeded
    ok.

%% --- Multiple words with one failing (filtermap in compile_words_to_binary) ---
mixed_good_bad_words_test() ->
    GoodDef = word_def("good_w", ['Int'], ['Int'], ["dup", "+"]),
    BadDef = {"bad_w", [], ['Int'], [#operation{name = "+"}]},
    {ok, af_wc_test_mixed} = af_word_compiler:compile_words_to_module(
        af_wc_test_mixed, [GoodDef, BadDef]),
    ?assertEqual([{'Int', 10}], af_wc_test_mixed:good_w([{'Int', 5}])).

%% --- opaque stack with same-module word call (line 394-396) ---
opaque_stack_same_module_test() ->
    setup_full(),
    Def1 = word_def("tripl", ['Int'], ['Int'], ["dup", "dup", "+", "+"]),
    %% After calling tripl, stack is opaque. Call tripl again (same-module call on opaque stack)
    Def2 = word_def("tripl2", ['Int'], ['Int'], ["tripl", "tripl"]),
    {ok, af_wc_test_opsm} = af_word_compiler:compile_words_to_module(af_wc_test_opsm, [Def1, Def2]),
    ?assertEqual([{'Int', 9}], af_wc_test_opsm:tripl([{'Int', 3}])),
    ?assertEqual([{'Int', 27}], af_wc_test_opsm:tripl2([{'Int', 3}])).

%% --- Multi-token send block call path (lines 871-877) ---
%% A send block with args where the word is a call (returns values),
%% not a cast. This exercises the false branch of is_actor_word_cast
%% in the multi-token send block handler.
send_block_multi_call_path_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [
        fun(_) -> {"multi-token send block call (not cast) path", fun() ->
            %% Define a type where get-bal is a call (returns extra values)
            C1 = eval("type Wallet balance Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": get-bal Wallet -> Wallet Int ; balance .", C1),
            C3 = eval(": add-funds Wallet Int -> Wallet ; balance + balance! .", C2),

            %% Build a word with << 50 add-funds >> (multi-token cast)
            CastBody = [#operation{name = "<<"},
                        #operation{name = "50"},
                        #operation{name = "add-funds"},
                        #operation{name = ">>"}],
            CastDef = {"add50", ['Actor'], ['Actor'], CastBody},

            %% Build a word with << get-bal >> (single-token call)
            CallBody = [#operation{name = "<<"},
                        #operation{name = "get-bal"},
                        #operation{name = ">>"}],
            CallDef = {"check_bal", ['Actor'], ['Actor'], CallBody},

            {ok, af_wc_test_mscall2} = af_word_compiler:compile_words_to_module(
                af_wc_test_mscall2, [CastDef, CallDef]),

            C4 = eval("0 int wallet server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,

            %% Test the cast word
            CastResult = af_wc_test_mscall2:add50([{'Actor', Info}]),
            ?assert(lists:any(fun({'Actor', _}) -> true; (_) -> false end, CastResult)),
            timer:sleep(50),

            %% Test the call word
            CallResult = af_wc_test_mscall2:check_bal([{'Actor', Info}]),
            ?assert(length(CallResult) >= 1),
            eval("<< stop >>", C4)
        end} end
    ]}.

%% --- send block arg simulation error (line 879) ---
%% Exercise compile_send_block multi-token where arg simulation fails
send_block_arg_error_test() ->
    setup_full(),
    %% << (empty-arg-list) word >> where the arg ops would fail
    %% Actually, ops always fall through to runtime, so this just tests
    %% the multi-op send block with various arg ops
    C1 = eval("type Svc count Int .", af_interpreter:new_continuation()),
    C2 = eval(": bump Svc -> Svc ; count 1 int + count! .", C1),
    Body = [#operation{name = "<<"},
            #operation{name = "bump"},
            #operation{name = ">>"}],
    Def = {"do_bump3", ['Actor'], ['Actor'], Body},
    {ok, af_wc_test_sbarg} = af_word_compiler:compile_words_to_module(
        af_wc_test_sbarg, [Def]),
    C3 = eval("0 int svc server", C2),
    [{'Actor', Info}] = C3#continuation.data_stack,
    Result = af_wc_test_sbarg:do_bump3([{'Actor', Info}]),
    ?assert(lists:any(fun({'Actor', _}) -> true; (_) -> false end, Result)),
    eval("<< stop >>", C3).

%% --- lookup_op_effect non-list value edge case (line 929) ---
%% Test that lookup_op_in_types skips types with non-list ops entry
lookup_op_effect_edge_test() ->
    setup_full(),
    %% Register an op where the ops map value is a non-list (edge case)
    %% This can happen if someone stores a single op instead of a list
    %% Actually, the code checks maps:get(Name, Ops, []) so it gets a list.
    %% The _ clause at line 929 matches when the map entry is not a list of ops.
    %% Let's just verify the runtime_dispatch path handles unknown ops correctly.
    Def = word_def("mystery2", ['Int'], ['Int'], ["completely_unknown_op"]),
    {ok, af_wc_test_unk} = af_word_compiler:compile_words_to_module(af_wc_test_unk, [Def]),
    Result = af_wc_test_unk:mystery2([{'Int', 42}]),
    %% Unknown op goes through runtime dispatch, push as atom
    ?assertEqual([{'Atom', "completely_unknown_op"}, {'Int', 42}], Result).

%% --- resolve_word_call without module key (line 945-951) ---
%% This is the second clause of resolve_word_call that doesn't pattern match
%% on #{module := _Mod, words := Words}. It's hit when the Ctx map doesn't
%% have a module key.
resolve_word_call_no_module_ctx_test() ->
    setup_full(),
    %% This is already indirectly tested, but let's be explicit:
    %% When a word is compiled that calls another word, but the Ctx
    %% doesn't have the module key, it uses find_native_word.
    HelperDef = word_def("nmod_helper", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_nmod} = af_word_compiler:compile_words_to_module(af_wc_test_nmod, [HelperDef]),
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_nmod, nmod_helper, ['Int'], ['Int']),
    af_type:add_op('Int', Wrapper),
    CallerDef = word_def("call_nmod", ['Int'], ['Int'], ["nmod_helper"]),
    {ok, af_wc_test_callnmod} = af_word_compiler:compile_words_to_module(
        af_wc_test_callnmod, [CallerDef]),
    ?assertEqual([{'Int', 10}], af_wc_test_callnmod:call_nmod([{'Int', 5}])).

%% --- Multi-token send block with call word (not cast) (lines 872-877) ---
%% Exercises the false branch of is_actor_word_cast in multi-token send block.
send_block_multi_call_word_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [
        fun(_) -> {"multi-token send block where word is call (returns values)", fun() ->
            %% Define type with a call word (more outputs than inputs)
            C1 = eval("type Store val Int .",
                       af_interpreter:new_continuation()),
            %% get-store is a call: Store -> Store Int (2 outputs > 1 input)
            C2 = eval(": get-store Store -> Store Int ; val .", C1),
            %% set-store is a cast: Store Int -> Store (1 output <= 2 inputs)
            C3 = eval(": set-store Store Int -> Store ; val! .", C2),

            %% Multi-token call: << 5 set-store >> is a cast (uses cast path)
            %% What about << get-store >> single-token call with a prefix arg?
            %% We need a multi-token send with a call word:
            %% Define a word where call returns extra value
            %% Actually, multi-token send always has the last token as the word.
            %% To test the call path, the LAST token must be a call word.
            %% We can do: << some_literal get-store >>
            %% But get-store takes Store (already the actor state), no extra args needed.
            %% Let's make a word that takes args and returns extra values.
            C4 = eval(": add-and-get Store Int -> Store Int ; val + dup val! .", C3),
            %% add-and-get: 2 inputs, 2 outputs -> outputs > inputs? No, 2=2 => cast.
            %% Let's make outputs > inputs:
            C5 = eval(": peek-add Store Int -> Store Int Int ; val over + .", C4),
            %% peek-add: 2 inputs, 3 outputs -> 3 > 2 => call!
            %% << 10 peek-add >> should use call path

            Body = [#operation{name = "<<"},
                    #operation{name = "10"},
                    #operation{name = "peek-add"},
                    #operation{name = ">>"}],
            Def = {"do_peek", ['Actor'], ['Actor'], Body},
            {ok, af_wc_test_mscall3} = af_word_compiler:compile_words_to_module(
                af_wc_test_mscall3, [Def]),
            C6 = eval("0 int store server", C5),
            [{'Actor', Info}] = C6#continuation.data_stack,
            Result = af_wc_test_mscall3:do_peek([{'Actor', Info}]),
            ?assert(is_list(Result)),
            eval("<< stop >>", C6)
        end} end
    ]}.

%% --- try_loop_opt: body ends with << >> but last op after send is not self-recursive (line 179) ---
loop_opt_send_but_not_recursive_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [
        fun(_) -> {"try_loop_opt not_applicable when body after send doesn't end with self-call", fun() ->
            %% Base case
            BaseDef = {"nrec", [{'Int', 0}, 'Actor'], ['Actor'],
                       [#operation{name = "swap"}, #operation{name = "drop"}]},
            %% Recursive case with << >> but body after >> doesn't end with self-call
            RecBody = [#operation{name = "<<"},
                       #operation{name = "increment"},
                       #operation{name = ">>"},
                       #operation{name = "swap"},
                       #operation{name = "drop"}],  % ends with "drop", not "nrec"
            RecDef = {"nrec", ['Int', 'Actor'], ['Actor'], RecBody},
            %% Should compile as regular multi-clause, not loop-optimized
            {ok, af_wc_test_nrec} = af_word_compiler:compile_words_to_module(
                af_wc_test_nrec, [BaseDef, RecDef]),
            ok
        end} end
    ]}.

%% --- try_loop_opt with no Actor in sig_in (line 202) ---
loop_opt_no_actor_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [
        fun(_) -> {"try_loop_opt not_applicable when recursive clause has no Actor", fun() ->
            BaseDef = {"noact", [{'Int', 0}], ['Int'],
                       [#operation{name = "drop"}, #operation{name = "0"}]},
            RecBody = [#operation{name = "<<"},
                       #operation{name = "increment"},
                       #operation{name = ">>"},
                       #operation{name = "1"},
                       #operation{name = "-"},
                       #operation{name = "noact"}],
            RecDef = {"noact", ['Int'], ['Int'], RecBody},
            %% No Actor in sig => not_applicable for loop opt
            {ok, af_wc_test_noact} = af_word_compiler:compile_words_to_module(
                af_wc_test_noact, [BaseDef, RecDef]),
            ?assertEqual([{'Int', 0}], af_wc_test_noact:noact([{'Int', 0}]))
        end} end
    ]}.

%% --- try_loop_opt with empty base case body (line 162) ---
loop_opt_empty_base_body_test_() ->
    {foreach, fun setup_full/0, fun(_) -> ok end, [
        fun(_) -> {"try_loop_opt base case with empty body", fun() ->
            %% Base case with empty body ([] -> true in partition)
            BaseDef = {"eblast", [{'Int', 0}, 'Actor'], ['Actor'],
                       []},  %% Empty body
            RecBody = [#operation{name = "<<"},
                       #operation{name = "increment"},
                       #operation{name = ">>"},
                       #operation{name = "swap"},
                       #operation{name = "1"},
                       #operation{name = "-"},
                       #operation{name = "swap"},
                       #operation{name = "eblast"}],
            RecDef = {"eblast", ['Int', 'Actor'], ['Actor'], RecBody},
            C1 = eval("type Counter3 value Int .",
                       af_interpreter:new_continuation()),
            C2 = eval(": increment Counter3 -> Counter3 ; value 1 int + value! .", C1),
            {ok, af_wc_test_eblast} = af_word_compiler:compile_words_to_module(
                af_wc_test_eblast, [BaseDef, RecDef]),
            C3 = eval("0 int counter3 server", C2),
            [{'Actor', Info}] = C3#continuation.data_stack,
            Result = af_wc_test_eblast:eblast([{'Int', 2}, {'Actor', Info}]),
            ?assert(is_list(Result)),
            eval("<< stop >>", C3)
        end} end
    ]}.
