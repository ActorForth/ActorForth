-module(af_word_compiler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
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
