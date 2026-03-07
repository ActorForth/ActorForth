-module(af_word_compiler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("operation.hrl").
-include("af_type.hrl").

%% Helper to build a word definition tuple from components.
word_def(Name, SigIn, SigOut, BodyNames) ->
    Body = [#operation{name = N} || N <- BodyNames],
    {Name, SigIn, SigOut, Body}.

%% --- compile_words_to_module tests ---
%% Functions take/return tagged stack lists: [{Type, Val}, ...]

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
    %% add on stack [X, Y | Rest] -> [Y+X | Rest]
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
    %% over on [X, Y] -> [Y, X, Y], then + on [Y, X, Y] -> [X+Y, Y]
    Def = word_def("over_add", ['Int', 'Int'], ['Int', 'Int'], ["over", "+"]),
    {ok, af_wc_test_over} = af_word_compiler:compile_words_to_module(af_wc_test_over, [Def]),
    ?assertEqual([{'Int', 12}, {'Int', 5}], af_wc_test_over:over_add([{'Int', 7}, {'Int', 5}])).

drop_test() ->
    %% second(X, Y) -> Y  (body: drop)
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
    %% inc(X) -> X + 1  (body: 1 +)
    Def = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    {ok, af_wc_test_lit} = af_word_compiler:compile_words_to_module(af_wc_test_lit, [Def]),
    ?assertEqual([{'Int', 6}], af_wc_test_lit:inc([{'Int', 5}])),
    ?assertEqual([{'Int', 0}], af_wc_test_lit:inc([{'Int', -1}])).

complex_body_test() ->
    %% sum_squares(X, Y) -> X*X + Y*Y  (body: swap dup * swap dup * +)
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
    %% 2dup on [X, Y] -> [X, Y, X, Y]
    Def = word_def("both", ['Int', 'Int'], ['Int', 'Int', 'Int', 'Int'], ["2dup"]),
    {ok, af_wc_test_2dup} = af_word_compiler:compile_words_to_module(af_wc_test_2dup, [Def]),
    ?assertEqual([{'Int', 3}, {'Int', 7}, {'Int', 3}, {'Int', 7}],
                 af_wc_test_2dup:both([{'Int', 3}, {'Int', 7}])).

rot_test() ->
    %% rot on [A, B, C] -> [C, A, B]
    Def = word_def("myrot", ['Int', 'Int', 'Int'], ['Int', 'Int', 'Int'], ["rot"]),
    {ok, af_wc_test_rot} = af_word_compiler:compile_words_to_module(af_wc_test_rot, [Def]),
    ?assertEqual([{'Int', 3}, {'Int', 1}, {'Int', 2}],
                 af_wc_test_rot:myrot([{'Int', 1}, {'Int', 2}, {'Int', 3}])).

%% --- Inter-word call tests ---
%% These use apply_impl for ops after a word call, so the type system must be initialized.

setup_types() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init().

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

%% --- get_module_binary / store_module_binary tests ---
%% Covers lines 46, and the store/retrieve paths

get_module_binary_not_found_test() ->
    %% Ensure table exists but module is not stored
    af_word_compiler:store_module_binary(af_wc_test_bintable_sentinel, <<>>),
    ?assertEqual(not_found, af_word_compiler:get_module_binary(af_wc_nonexistent_mod)).

get_module_binary_found_test() ->
    %% Store and retrieve a binary
    Def = word_def("id", ['Int'], ['Int'], []),
    {ok, af_wc_test_binget} = af_word_compiler:compile_words_to_module(af_wc_test_binget, [Def]),
    {ok, Bin} = af_word_compiler:get_module_binary(af_wc_test_binget),
    ?assert(is_binary(Bin)).

store_module_binary_test() ->
    %% Public store_module_binary should work
    af_word_compiler:store_module_binary(af_wc_test_manual_store, <<"fake_binary">>),
    ?assertEqual({ok, <<"fake_binary">>}, af_word_compiler:get_module_binary(af_wc_test_manual_store)).

%% --- Value-constrained patterns (float, boolean) ---
%% Covers lines 154-160

float_value_constraint_test() ->
    %% Float value in sig_in: {Float, 0.0}
    Def = {"fzero", [{'Float', 0.0}], ['Int'], [#operation{name = "drop"}, #operation{name = "1"}]},
    {ok, af_wc_test_fval} = af_word_compiler:compile_words_to_module(af_wc_test_fval, [Def]),
    ?assertEqual([{'Int', 1}], af_wc_test_fval:fzero([{'Float', 0.0}])).

bool_value_constraint_test() ->
    %% Boolean value in sig_in: {Bool, true}
    Def = {"btrue", [{'Bool', true}], ['Int'], [#operation{name = "drop"}, #operation{name = "1"}]},
    {ok, af_wc_test_bval} = af_word_compiler:compile_words_to_module(af_wc_test_bval, [Def]),
    ?assertEqual([{'Int', 1}], af_wc_test_bval:btrue([{'Bool', true}])).

%% --- Comparison ops (==, !=, <, <=, >=) ---
%% Covers lines 274-290

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
    %% Stack is TOS-first: lt([TOS, Below]) computes Below < TOS
    ?assertEqual([{'Bool', true}], af_wc_test_lt:lt([{'Int', 5}, {'Int', 3}])),
    ?assertEqual([{'Bool', false}], af_wc_test_lt:lt([{'Int', 3}, {'Int', 5}])).

le_comparison_test() ->
    Def = word_def("le", ['Int', 'Int'], ['Bool'], ["<="]),
    {ok, af_wc_test_le} = af_word_compiler:compile_words_to_module(af_wc_test_le, [Def]),
    %% Stack is TOS-first: le([TOS, Below]) computes Below <= TOS
    ?assertEqual([{'Bool', true}], af_wc_test_le:le([{'Int', 5}, {'Int', 3}])),
    ?assertEqual([{'Bool', true}], af_wc_test_le:le([{'Int', 5}, {'Int', 5}])),
    ?assertEqual([{'Bool', false}], af_wc_test_le:le([{'Int', 3}, {'Int', 5}])).

ge_comparison_test() ->
    Def = word_def("ge", ['Int', 'Int'], ['Bool'], [">="]),
    {ok, af_wc_test_ge} = af_word_compiler:compile_words_to_module(af_wc_test_ge, [Def]),
    %% Stack is TOS-first: ge([TOS, Below]) computes Below >= TOS
    ?assertEqual([{'Bool', true}], af_wc_test_ge:ge([{'Int', 3}, {'Int', 5}])),
    ?assertEqual([{'Bool', true}], af_wc_test_ge:ge([{'Int', 5}, {'Int', 5}])),
    ?assertEqual([{'Bool', false}], af_wc_test_ge:ge([{'Int', 5}, {'Int', 3}])).

%% --- Float and boolean literals in body ---
%% Covers lines 307, 311, 313

float_literal_in_body_test() ->
    Def = word_def("addfloat", ['Int'], ['Int'], ["1.5"]),
    {ok, af_wc_test_flit} = af_word_compiler:compile_words_to_module(af_wc_test_flit, [Def]),
    Result = af_wc_test_flit:addfloat([{'Int', 5}]),
    %% Float literal 1.5 gets pushed as {Float, 1.5} on top of stack
    ?assertEqual([{'Float', 1.5}, {'Int', 5}], Result).

true_literal_in_body_test() ->
    Def = word_def("pushtrue", ['Int'], ['Bool', 'Int'], ["true"]),
    {ok, af_wc_test_tlit} = af_word_compiler:compile_words_to_module(af_wc_test_tlit, [Def]),
    ?assertEqual([{'Bool', true}, {'Int', 5}], af_wc_test_tlit:pushtrue([{'Int', 5}])).

false_literal_in_body_test() ->
    Def = word_def("pushfalse", ['Int'], ['Bool', 'Int'], ["false"]),
    {ok, af_wc_test_flit2} = af_word_compiler:compile_words_to_module(af_wc_test_flit2, [Def]),
    ?assertEqual([{'Bool', false}, {'Int', 5}], af_wc_test_flit2:pushfalse([{'Int', 5}])).

%% --- Multi-clause word compilation ---
%% Covers lines 103, 107 (multi-clause with errors / no compilable clauses)

multi_clause_integer_pattern_test() ->
    %% Two clauses for same word name with integer value constraints
    Def1 = {"absval", [{'Int', 0}], ['Int'],
            [#operation{name = "drop"}, #operation{name = "0"}]},
    Def2 = {"absval", ['Int'], ['Int'], [#operation{name = "dup"}, #operation{name = "*"}]},
    {ok, af_wc_test_mclause} = af_word_compiler:compile_words_to_module(af_wc_test_mclause, [Def1, Def2]),
    ?assertEqual([{'Int', 0}], af_wc_test_mclause:absval([{'Int', 0}])),
    ?assertEqual([{'Int', 25}], af_wc_test_mclause:absval([{'Int', 5}])).

%% --- Runtime dispatch (unknown ops go through apply_impl) ---
%% Covers lines 319, 329-340

runtime_dispatch_test() ->
    setup_types(),
    %% Use a body op that isn't a known primitive or literal — forces runtime dispatch
    %% "print" is a side-effect op (0 outputs) registered in Any
    Def = word_def("show", ['Int'], ['Int'], ["print"]),
    {ok, af_wc_test_rt} = af_word_compiler:compile_words_to_module(af_wc_test_rt, [Def]),
    %% print returns the stack unchanged (side-effect only)
    Result = af_wc_test_rt:show([{'Int', 42}]),
    ?assertEqual([{'Int', 42}], Result).

runtime_dispatch_unknown_op_test() ->
    setup_types(),
    %% An op that isn't found anywhere — runtime dispatch with unknown effect
    Def = word_def("mystery", ['Int'], ['Int'], ["totally_unknown_xyz"]),
    {ok, af_wc_test_rt2} = af_word_compiler:compile_words_to_module(af_wc_test_rt2, [Def]),
    %% apply_impl pushes as Atom for unknown words
    Result = af_wc_test_rt2:mystery([{'Int', 42}]),
    ?assertEqual([{'Atom', "totally_unknown_xyz"}, {'Int', 42}], Result).

%% --- find_native_word ---
%% Covers lines 465, 472, 476, 480

find_native_word_not_found_test() ->
    setup_types(),
    ?assertEqual(not_found, af_word_compiler:find_native_word("no_such_native_word_xyz")).

find_native_word_found_test() ->
    setup_types(),
    %% Compile a word to native, then find it
    Def = word_def("nfind", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_nfind} = af_word_compiler:compile_words_to_module(af_wc_test_nfind, [Def]),
    %% Register the native word in the type system
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_nfind, nfind, ['Int'], ['Int']),
    af_type:add_op('Int', Wrapper),
    ?assertMatch({ok, af_wc_test_nfind, _, _}, af_word_compiler:find_native_word("nfind")).

%% --- find_compiled_word_defs ---
%% Covers lines 453-454

find_compiled_word_defs_empty_test() ->
    setup_types(),
    ?assertEqual([], af_word_compiler:find_compiled_word_defs("nonexistent_compiled_word")).

find_compiled_word_defs_found_test() ->
    setup_types(),
    %% Register a compiled operation
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

%% --- Cross-module (remote) word call ---
%% Covers lines 372-384, 398-403

cross_module_native_call_test() ->
    setup_types(),
    %% Compile "helper" word to a module
    HelperDef = word_def("helper", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_helper} = af_word_compiler:compile_words_to_module(af_wc_test_helper, [HelperDef]),
    %% Register it as native in the type system
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_helper, helper, ['Int'], ['Int']),
    af_type:add_op('Int', Wrapper),
    %% Now compile another module that calls "helper" — should resolve via find_native_word
    CallerDef = word_def("use_helper", ['Int'], ['Int'], ["helper"]),
    {ok, af_wc_test_caller} = af_word_compiler:compile_words_to_module(af_wc_test_caller, [CallerDef]),
    ?assertEqual([{'Int', 10}], af_wc_test_caller:use_helper([{'Int', 5}])).

%% --- arg_name for N > 3 ---
%% Covers line 419

four_arg_word_test() ->
    %% Uses 4 args, triggering arg_name(4) = 'Arg4'
    Def = word_def("sum4", ['Int', 'Int', 'Int', 'Int'], ['Int'],
                   ["+", "+", "+"]),
    {ok, af_wc_test_4arg} = af_word_compiler:compile_words_to_module(af_wc_test_4arg, [Def]),
    ?assertEqual([{'Int', 10}], af_wc_test_4arg:sum4([{'Int', 1}, {'Int', 2}, {'Int', 3}, {'Int', 4}])).

%% --- compile_words_to_binary directly ---
%% Covers line 75 (warnings path)

compile_words_to_binary_test() ->
    Def = word_def("bin_id", ['Int'], ['Int'], []),
    {ok, af_wc_test_bin, Bin} = af_word_compiler:compile_words_to_binary(af_wc_test_bin, [Def]),
    ?assert(is_binary(Bin)).

compile_words_to_binary_empty_test() ->
    ?assertEqual({error, no_compilable_words},
                 af_word_compiler:compile_words_to_binary(af_wc_test_empty2, [])).

%% --- Opaque stack dispatch (simulate_body with stack type) ---
%% Covers lines 202-213 (opaque stack paths)

opaque_stack_local_call_test() ->
    setup_types(),
    %% After a local call, the stack becomes opaque. Subsequent ops dispatch via apply_impl.
    Def1 = word_def("dbl", ['Int'], ['Int'], ["dup", "+"]),
    Def2 = word_def("dbl_dbl", ['Int'], ['Int'], ["dbl", "dbl"]),
    {ok, af_wc_test_opaque} = af_word_compiler:compile_words_to_module(af_wc_test_opaque, [Def1, Def2]),
    ?assertEqual([{'Int', 20}], af_wc_test_opaque:dbl_dbl([{'Int', 5}])).

%% --- make_wrapper tests ---

make_wrapper_test() ->
    Def = word_def("wrap_test", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_wrap} = af_word_compiler:compile_words_to_module(af_wc_test_wrap, [Def]),
    Wrapper = af_word_compiler:make_wrapper(af_wc_test_wrap, wrap_test, ['Int'], ['Int']),
    ?assertEqual("wrap_test", Wrapper#operation.name),
    ?assertEqual(['Int'], Wrapper#operation.sig_in),
    ?assertEqual(['Int'], Wrapper#operation.sig_out),
    ?assertMatch({native, af_wc_test_wrap}, Wrapper#operation.source).
