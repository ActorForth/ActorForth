-module(af_core_compiler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").

setup() ->
    af_type:reset(),
    af_repl:init_types().

eval(Code) ->
    Tokens = af_parser:parse(Code, "test"),
    Cont = af_interpreter:new_continuation(),
    af_interpreter:interpret_tokens(Tokens, Cont).

eval(Code, Cont) ->
    Tokens = af_parser:parse(Code, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

%% Test eval_and_compile compiles simple words
eval_compile_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"eval_and_compile compiles double", fun() ->
            Source = ": double Int -> Int ; dup + .",
            {ok, Mod} = af_core_compiler:eval_and_compile(Source, "test", "ec_test"),
            ?assertEqual(af_native_ec_test, Mod),
            %% Call the compiled function
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

%% Test call-compiled word
call_compiled_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"call-compiled calls compiled function", fun() ->
            %% First compile a word
            Source = ": triple Int -> Int ; dup dup + + .",
            {ok, _} = af_core_compiler:eval_and_compile(Source, "test", "cc_test"),
            %% Now call it from A4 using call-compiled
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
            %% Create counter, bump it, check count
            C1 = eval("0 counter"),
            C2 = eval("\"af_native_prod_cc\" \"bump\" 1 call-compiled count", C1),
            [{'Int', Count} | _] = C2#continuation.data_stack,
            ?assertEqual(1, Count)
        end} end
    ]}.
