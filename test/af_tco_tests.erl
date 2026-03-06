-module(af_tco_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init().

%% --- Tail Call Optimization ---

tco_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"tail-recursive countdown works", fun() ->
            %% Use inline sub-clause syntax
            C1 = eval(": countdown Int -> ; : 0 -> ; drop : Int -> ; 1 - countdown .", af_interpreter:new_continuation()),
            C2 = eval("5 countdown", C1),
            ?assertEqual([], C2#continuation.data_stack)
        end} end,

        fun(_) -> {"tail-recursive sum accumulator", fun() ->
            %% sum-helper: acc n -> result
            %% Base case: n=0, return acc
            %% Recursive: acc' = acc+n, n' = n-1
            C1 = eval(": sum-helper Int Int -> Int ; : Int 0 -> Int ; drop : Int Int -> Int ; dup rot + swap 1 - sum-helper .", af_interpreter:new_continuation()),
            C2 = eval("0 5 sum-helper", C1),
            [{'Int', 15}] = C2#continuation.data_stack
        end} end,

        fun(_) -> {"non-tail-recursive fib still works", fun() ->
            C1 = eval(": fib Int -> Int ; : 0 -> 0 ; : 1 -> 1 ; : Int -> Int ; dup 1 - fib swap 2 - fib + .", af_interpreter:new_continuation()),
            C2 = eval("6 fib", C1),
            [{'Int', 8}] = C2#continuation.data_stack
        end} end,

        fun(_) -> {"tail-recursive word preserves word trace correctly", fun() ->
            C1 = eval(": tc-test Int -> ; : 0 -> ; drop : Int -> ; 1 - tc-test .", af_interpreter:new_continuation()),
            C2 = eval("10 tc-test", C1),
            ?assertEqual([], C2#continuation.word_trace)
        end} end,

        fun(_) -> {timeout, 30, {"moderate recursion depth with TCO", fun() ->
            C1 = eval(": deep Int -> ; : 0 -> ; drop : Int -> ; 1 - deep .", af_interpreter:new_continuation()),
            C2 = eval("1000 deep", C1),
            ?assertEqual([], C2#continuation.data_stack)
        end}} end
    ]}.
