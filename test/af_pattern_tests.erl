-module(af_pattern_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

%% --- Int pattern matching (existing, verify still works) ---

int_pattern_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"match int literal 0", fun() ->
            C1 = eval(": f Int -> Int ; : 0 -> 0 ; : Int -> Int ; 100 + .", af_interpreter:new_continuation()),
            C2 = eval("0 f", C1),
            [{'Int', 0}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"match int literal 1", fun() ->
            C1 = eval(": f Int -> Int ; : 0 -> 0 ; : 1 -> 1 ; : Int -> Int ; 100 + .", af_interpreter:new_continuation()),
            C2 = eval("1 f", C1),
            [{'Int', 1}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"fallthrough to general clause", fun() ->
            C1 = eval(": f Int -> Int ; : 0 -> 0 ; : Int -> Int ; 100 + .", af_interpreter:new_continuation()),
            C2 = eval("5 f", C1),
            [{'Int', 105}] = C2#continuation.data_stack
        end} end
    ]}.

%% --- Bool pattern matching ---

bool_pattern_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"match True", fun() ->
            C1 = eval(": to-int Bool -> Int ; : True -> Int ; drop 1 : False -> Int ; drop 0 .", af_interpreter:new_continuation()),
            C2 = eval("True to-int", C1),
            [{'Int', 1}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"match False", fun() ->
            C1 = eval(": to-int Bool -> Int ; : True -> Int ; drop 1 : False -> Int ; drop 0 .", af_interpreter:new_continuation()),
            C2 = eval("False to-int", C1),
            [{'Int', 0}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"if-like construct using bool patterns", fun() ->
            %% Stack: ... else-val then-val Bool
            %% True: keep then-val (swap drop swap drop)
            %% False: keep else-val (drop swap drop)
            C1 = eval(": choose Int Int Bool -> Int ; : True -> Int ; drop drop : False -> Int ; drop swap drop .", af_interpreter:new_continuation()),
            C2 = eval("10 20 True choose", C1),
            [{'Int', 10}] = C2#continuation.data_stack,
            C3 = eval("10 20 False choose", C1),
            [{'Int', 20}] = C3#continuation.data_stack
        end} end
    ]}.

%% --- String pattern matching ---

string_pattern_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"match string literal", fun() ->
            C1 = eval(": greet String -> String ; : \"hello\" -> String ; drop \"Hello World!\" : String -> String ; .", af_interpreter:new_continuation()),
            C2 = eval("\"hello\" greet", C1),
            [{'String', <<"Hello World!">>}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"string pattern fallthrough", fun() ->
            C1 = eval(": greet String -> String ; : \"hello\" -> String ; drop \"Hello World!\" : String -> String ; .", af_interpreter:new_continuation()),
            C2 = eval("\"goodbye\" greet", C1),
            [{'String', <<"goodbye">>}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"multiple string patterns", fun() ->
            C1 = eval(": lang String -> String ; : \"en\" -> String ; drop \"English\" : \"fr\" -> String ; drop \"French\" : String -> String ; drop \"Unknown\" .", af_interpreter:new_continuation()),
            C2 = eval("\"en\" lang", C1),
            [{'String', <<"English">>}] = C2#continuation.data_stack,
            C3 = eval("\"fr\" lang", C1),
            [{'String', <<"French">>}] = C3#continuation.data_stack,
            C4 = eval("\"de\" lang", C1),
            [{'String', <<"Unknown">>}] = C4#continuation.data_stack
        end} end
    ]}.

%% --- Mixed type pattern matching ---

mixed_pattern_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"fib still works with all matching", fun() ->
            C1 = eval(": fib Int -> Int ; : 0 -> 0 ; : 1 -> 1 ; : Int -> Int ; dup 1 - fib swap 2 - fib + .", af_interpreter:new_continuation()),
            C2 = eval("10 fib", C1),
            [{'Int', 55}] = C2#continuation.data_stack
        end} end
    ]}.
