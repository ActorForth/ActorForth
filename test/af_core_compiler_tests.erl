%% af_core_compiler_tests.erl -- Tests for af_core_compiler.erl
%%
%% Covers: compile_word/2, compile_words/2, save_module/2,
%%         eval_and_compile/3, call_compiled/3, init/0,
%%         op_call_compiled/1, to_str/to_atom helpers.

-module(af_core_compiler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

setup() ->
    af_type:reset().

eval(Code) ->
    Tokens = af_parser:parse(Code, "test"),
    Cont = af_interpreter:new_continuation(),
    af_interpreter:interpret_tokens(Tokens, Cont).

eval(Code, Cont) ->
    Tokens = af_parser:parse(Code, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

define_word(Source) ->
    Tokens = af_parser:parse(Source, "test"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()).

%%====================================================================
%% eval_and_compile/3 (existing tests preserved)
%%====================================================================

eval_compile_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"eval_and_compile compiles double", fun() ->
            Source = ": double Int -> Int ; dup + .",
            {ok, Mod} = af_core_compiler:eval_and_compile(Source, "test", "ec_test"),
            ?assertEqual(af_native_ec_test, Mod),
            Result = af_native_ec_test:double([{'Int', 21}]),
            ?assertEqual([{'Int', 42}], Result)
        end} end,

        fun(_) -> {"eval_and_compile compiles from file", fun() ->
            {ok, Src} = file:read_file("samples/lib_math.a4"),
            {ok, Mod} = af_core_compiler:eval_and_compile(
                binary_to_list(Src), "samples/lib_math.a4", "math_ec"),
            ?assertEqual(af_native_math_ec, Mod),
            Result = af_native_math_ec:square([{'Int', 5}]),
            ?assertEqual([{'Int', 25}], Result)
        end} end
    ]}.

%% call-compiled word via interpreter (existing tests preserved)
call_compiled_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"call-compiled calls compiled function", fun() ->
            Source = ": triple Int -> Int ; dup dup + + .",
            {ok, _} = af_core_compiler:eval_and_compile(Source, "test", "cc_test"),
            C = eval("7 \"af_native_cc_test\" \"triple\" 1 call-compiled"),
            Stack = C#continuation.data_stack,
            ?assertEqual([{'Int', 21}], Stack)
        end} end,

        fun(_) -> {"call-compiled handles string ops", fun() ->
            {ok, Src} = file:read_file("test/selfhost/greet.a4"),
            {ok, _} = af_core_compiler:eval_and_compile(
                binary_to_list(Src), "test", "greet_cc"),
            C = eval("\"World\" \"af_native_greet_cc\" \"greet\" 1 call-compiled"),
            Stack = C#continuation.data_stack,
            ?assertMatch([{'String', <<"Hello, World!">>}], Stack)
        end} end,

        fun(_) -> {"call-compiled handles product types", fun() ->
            {ok, Src} = file:read_file("test/selfhost/product_test.a4"),
            {ok, _} = af_core_compiler:eval_and_compile(
                binary_to_list(Src), "test", "prod_cc"),
            C1 = eval("0 counter"),
            C2 = eval("\"af_native_prod_cc\" \"bump\" 1 call-compiled count", C1),
            [{'Int', Count} | _] = C2#continuation.data_stack,
            ?assertEqual(1, Count)
        end} end
    ]}.

%%====================================================================
%% compile_word/2
%%====================================================================

compile_word_basic_test() ->
    setup(),
    define_word(": double Int -> Int ; dup + ."),
    {ok, Mod} = af_core_compiler:compile_word("cw_basic", "double"),
    ?assertEqual([{'Int', 42}], Mod:double([{'Int', 21}])).

compile_word_not_found_test() ->
    setup(),
    Result = af_core_compiler:compile_word("cw_nf", "nonexistent_word_xyz"),
    ?assertMatch({error, {no_word, "nonexistent_word_xyz"}}, Result).

compile_word_binary_args_test() ->
    setup(),
    define_word(": bwfn Int -> Int ; 3 * ."),
    {ok, Mod} = af_core_compiler:compile_word(<<"bw_mod">>, <<"bwfn">>),
    ?assertEqual([{'Int', 15}], Mod:bwfn([{'Int', 5}])).

compile_word_atom_args_test() ->
    setup(),
    define_word(": awfn Int -> Int ; 1 + ."),
    {ok, Mod} = af_core_compiler:compile_word(aw_mod, awfn),
    ?assertEqual([{'Int', 6}], Mod:awfn([{'Int', 5}])).

compile_word_module_prefix_test() ->
    setup(),
    define_word(": prefixed Int -> Int ; 2 + ."),
    {ok, Mod} = af_core_compiler:compile_word("pfx", "prefixed"),
    ?assertEqual(af_native_pfx, Mod).

%%====================================================================
%% compile_words/2
%%====================================================================

compile_words_multiple_test() ->
    setup(),
    define_word(": inc Int -> Int ; 1 + .\n: dec Int -> Int ; 1 - ."),
    {ok, Mod} = af_core_compiler:compile_words("cw_multi", ["inc", "dec"]),
    ?assertEqual([{'Int', 6}], Mod:inc([{'Int', 5}])),
    ?assertEqual([{'Int', 4}], Mod:dec([{'Int', 5}])).

compile_words_none_found_test() ->
    setup(),
    Result = af_core_compiler:compile_words("cw_empty", ["no_such_word_abc"]),
    ?assertEqual({error, no_words_found}, Result).

compile_words_mixed_found_test() ->
    setup(),
    define_word(": exists_w Int -> Int ; 1 + ."),
    %% One valid word plus one invalid -- flatmap still collects the valid one
    {ok, Mod} = af_core_compiler:compile_words("cw_mixed", ["exists_w", "nope_xyz"]),
    ?assertEqual([{'Int', 6}], Mod:exists_w([{'Int', 5}])).

compile_words_binary_names_test() ->
    setup(),
    define_word(": bnames_fn Int -> Int ; 2 + ."),
    {ok, _Mod} = af_core_compiler:compile_words(<<"bnames">>, [<<"bnames_fn">>]).

compile_words_atom_names_test() ->
    setup(),
    define_word(": anames_fn Int -> Int ; 3 + ."),
    {ok, _Mod} = af_core_compiler:compile_words(anames, [anames_fn]).

%%====================================================================
%% save_module/2
%%====================================================================

%% save_module works with modules on the code path (code:get_object_code).
%% Dynamically compiled modules (via code:load_binary) are NOT found by
%% code:get_object_code, so we test with a real on-disk module.

save_module_real_module_test() ->
    Path = "/tmp/af_core_compiler_save_test.beam",
    ok = af_core_compiler:save_module(af_core_compiler, Path),
    ?assert(filelib:is_file(Path)),
    file:delete(Path).

save_module_not_loaded_test() ->
    Result = af_core_compiler:save_module(
        nonexistent_module_xyz_99999, "/tmp/nope.beam"),
    ?assertMatch({error, {no_object_code, nonexistent_module_xyz_99999}}, Result).

save_module_binary_path_test() ->
    Path = <<"/tmp/af_core_compiler_save_bp.beam">>,
    ok = af_core_compiler:save_module(af_core_compiler, Path),
    ?assert(filelib:is_file(binary_to_list(Path))),
    file:delete(binary_to_list(Path)).

save_module_binary_mod_arg_test() ->
    %% Pass module name as binary
    Path = "/tmp/af_core_compiler_save_bm.beam",
    ok = af_core_compiler:save_module(<<"af_core_compiler">>, Path),
    ?assert(filelib:is_file(Path)),
    file:delete(Path).

save_module_string_mod_arg_test() ->
    %% Pass module name as string (list)
    Path = "/tmp/af_core_compiler_save_sm.beam",
    ok = af_core_compiler:save_module("af_core_compiler", Path),
    ?assert(filelib:is_file(Path)),
    file:delete(Path).

save_module_dynamic_not_on_path_test() ->
    %% Dynamically compiled modules are not found by code:get_object_code
    setup(),
    define_word(": sq_dyn Int -> Int ; dup * ."),
    {ok, Mod} = af_core_compiler:compile_word("save_dyn", "sq_dyn"),
    Result = af_core_compiler:save_module(Mod, "/tmp/nope.beam"),
    ?assertMatch({error, {no_object_code, _}}, Result).

%%====================================================================
%% eval_and_compile/3 -- additional coverage
%%====================================================================

eval_and_compile_no_words_test() ->
    setup(),
    Result = af_core_compiler:eval_and_compile("42", "test", "ec_nw"),
    ?assertEqual({error, no_words_defined}, Result).

eval_and_compile_multiple_words_test() ->
    setup(),
    Source = ": sq_ec Int -> Int ; dup * .\n: cube_ec Int -> Int ; dup dup * * .",
    {ok, Mod} = af_core_compiler:eval_and_compile(Source, "test", "ec_multi"),
    ?assertEqual([{'Int', 25}], Mod:sq_ec([{'Int', 5}])),
    ?assertEqual([{'Int', 125}], Mod:cube_ec([{'Int', 5}])).

eval_and_compile_binary_args_test() ->
    setup(),
    Source = <<": bsrc Int -> Int ; 2 * .">>,
    {ok, _Mod} = af_core_compiler:eval_and_compile(Source, <<"bfile">>, <<"bmod">>).

eval_and_compile_bad_source_test() ->
    setup(),
    %% Source with no word definitions produces no_words_defined
    Result = af_core_compiler:eval_and_compile("42 99", "test", "ec_bad"),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% call_compiled/3 (direct Erlang call)
%%====================================================================

call_compiled_direct_test() ->
    setup(),
    define_word(": add1_cc Int -> Int ; 1 + ."),
    {ok, Mod} = af_core_compiler:compile_word("cc_dir", "add1_cc"),
    Result = af_core_compiler:call_compiled(Mod, add1_cc, [{'Int', 10}]),
    ?assertEqual([{'Int', 11}], Result).

call_compiled_string_args_test() ->
    setup(),
    define_word(": sub1_cc Int -> Int ; 1 - ."),
    {ok, _Mod} = af_core_compiler:compile_word("cc_str", "sub1_cc"),
    Result = af_core_compiler:call_compiled("af_native_cc_str", "sub1_cc", [{'Int', 10}]),
    ?assertEqual([{'Int', 9}], Result).

call_compiled_binary_args_test() ->
    setup(),
    define_word(": mul2_cc Int -> Int ; 2 * ."),
    {ok, _Mod} = af_core_compiler:compile_word("cc_bin", "mul2_cc"),
    Result = af_core_compiler:call_compiled(<<"af_native_cc_bin">>, <<"mul2_cc">>, [{'Int', 7}]),
    ?assertEqual([{'Int', 14}], Result).

%%====================================================================
%% init/0
%%====================================================================

init_registers_word_test() ->
    setup(),
    af_core_compiler:init(),
    Result = af_type:find_op_by_name("call-compiled", 'Any'),
    ?assertMatch({ok, #operation{name = "call-compiled"}}, Result).

init_idempotent_test() ->
    setup(),
    af_core_compiler:init(),
    af_core_compiler:init(),
    Result = af_type:find_op_by_name("call-compiled", 'Any'),
    ?assertMatch({ok, #operation{name = "call-compiled"}}, Result).

%%====================================================================
%% op_call_compiled/1 -- direct invocation
%%====================================================================

op_call_compiled_direct_test() ->
    setup(),
    af_core_compiler:init(),
    define_word(": dbl_op Int -> Int ; dup + ."),
    {ok, Mod} = af_core_compiler:compile_word("op_cc", "dbl_op"),
    ModStr = atom_to_binary(Mod, utf8),
    %% Stack (TOS-first): Arity, FunName, ModName, Arg
    Stack = [
        {'Int', 1},
        {'String', <<"dbl_op">>},
        {'String', ModStr},
        {'Int', 21}
    ],
    Cont = #continuation{data_stack = Stack},
    {ok, Op} = af_type:find_op_by_name("call-compiled", 'Any'),
    ResultCont = (Op#operation.impl)(Cont),
    ?assertEqual([{'Int', 42}], ResultCont#continuation.data_stack).

op_call_compiled_zero_arity_test() ->
    setup(),
    af_core_compiler:init(),
    %% Define a word that takes nothing and pushes a value
    define_word(": const42 -> Int ; 42 ."),
    {ok, Mod} = af_core_compiler:compile_word("op_cc0", "const42"),
    ModStr = atom_to_binary(Mod, utf8),
    Stack = [
        {'Int', 0},
        {'String', <<"const42">>},
        {'String', ModStr}
    ],
    Cont = #continuation{data_stack = Stack},
    {ok, Op} = af_type:find_op_by_name("call-compiled", 'Any'),
    ResultCont = (Op#operation.impl)(Cont),
    ?assertMatch([{'Int', 42}], ResultCont#continuation.data_stack).

op_call_compiled_preserves_remaining_stack_test() ->
    setup(),
    af_core_compiler:init(),
    define_word(": inc_op Int -> Int ; 1 + ."),
    {ok, Mod} = af_core_compiler:compile_word("op_ccr", "inc_op"),
    ModStr = atom_to_binary(Mod, utf8),
    %% Extra items below the call-compiled args should be preserved
    Stack = [
        {'Int', 1},
        {'String', <<"inc_op">>},
        {'String', ModStr},
        {'Int', 10},
        {'Int', 999}  %% this should remain after the call
    ],
    Cont = #continuation{data_stack = Stack},
    {ok, Op} = af_type:find_op_by_name("call-compiled", 'Any'),
    ResultCont = (Op#operation.impl)(Cont),
    ?assertEqual([{'Int', 11}, {'Int', 999}],
                 ResultCont#continuation.data_stack).

op_call_compiled_error_test() ->
    setup(),
    af_core_compiler:init(),
    %% Call a non-existent module/function to trigger the error path
    Stack = [
        {'Int', 0},
        {'String', <<"no_such_fn">>},
        {'String', <<"no_such_mod_xyz">>}
    ],
    Cont = #continuation{data_stack = Stack},
    {ok, Op} = af_type:find_op_by_name("call-compiled", 'Any'),
    ?assertError(_, (Op#operation.impl)(Cont)).

%%====================================================================
%% to_str / to_atom helpers exercised through various input types
%%====================================================================

to_helpers_all_types_compile_word_test() ->
    setup(),
    define_word(": hlp Int -> Int ; 1 + ."),
    %% String (list) input
    {ok, M1} = af_core_compiler:compile_word("hlp_str", "hlp"),
    ?assertEqual([{'Int', 2}], M1:hlp([{'Int', 1}])),
    %% Binary input
    {ok, M2} = af_core_compiler:compile_word(<<"hlp_bin">>, <<"hlp">>),
    ?assertEqual([{'Int', 2}], M2:hlp([{'Int', 1}])),
    %% Atom input
    {ok, M3} = af_core_compiler:compile_word(hlp_atom, hlp),
    ?assertEqual([{'Int', 2}], M3:hlp([{'Int', 1}])).

to_helpers_call_compiled_test() ->
    setup(),
    define_word(": hlp2 Int -> Int ; 2 * ."),
    {ok, Mod} = af_core_compiler:compile_word("hlp2_mod", "hlp2"),
    %% call_compiled with atom args
    R1 = af_core_compiler:call_compiled(Mod, hlp2, [{'Int', 3}]),
    ?assertEqual([{'Int', 6}], R1),
    %% call_compiled with string args
    R2 = af_core_compiler:call_compiled(atom_to_list(Mod), "hlp2", [{'Int', 4}]),
    ?assertEqual([{'Int', 8}], R2),
    %% call_compiled with binary args
    R3 = af_core_compiler:call_compiled(atom_to_binary(Mod, utf8), <<"hlp2">>, [{'Int', 5}]),
    ?assertEqual([{'Int', 10}], R3).
