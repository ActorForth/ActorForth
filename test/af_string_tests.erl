-module(af_string_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

%% --- Quoted string literals ---

quoted_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"quoted string pushes String", fun() ->
            C = eval("\"hello\"", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"empty quoted string", fun() ->
            C = eval("\"\"", af_interpreter:new_continuation()),
            [{'String', <<>>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"quoted string with spaces", fun() ->
            C = eval("\"hello world\"", af_interpreter:new_continuation()),
            [{'String', <<"hello world">>}] = C#continuation.data_stack
        end} end
    ]}.

%% --- Constructor ---

constructor_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"string constructor from Atom", fun() ->
            C = eval("hello string", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"string pass-through", fun() ->
            C = eval("\"hello\" string", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end
    ]}.

%% --- concat ---

concat_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"concat two strings", fun() ->
            C = eval("\"hello\" \" world\" concat", af_interpreter:new_continuation()),
            [{'String', <<"hello world">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"concat empty strings", fun() ->
            C = eval("\"\" \"\" concat", af_interpreter:new_continuation()),
            [{'String', <<>>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"concat chain", fun() ->
            C = eval("\"a\" \"b\" concat \"c\" concat", af_interpreter:new_continuation()),
            [{'String', <<"abc">>}] = C#continuation.data_stack
        end} end
    ]}.

%% --- length ---

length_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"length of string", fun() ->
            C = eval("\"hello\" length", af_interpreter:new_continuation()),
            [{'Int', 5}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"length of empty string", fun() ->
            C = eval("\"\" length", af_interpreter:new_continuation()),
            [{'Int', 0}] = C#continuation.data_stack
        end} end
    ]}.

%% --- conversion ---

conversion_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"to-atom", fun() ->
            C = eval("\"hello\" to-atom", af_interpreter:new_continuation()),
            [{'Atom', "hello"}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"to-int", fun() ->
            C = eval("\"42\" to-int", af_interpreter:new_continuation()),
            [{'Int', 42}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"to-string from Int", fun() ->
            C = eval("42 to-string", af_interpreter:new_continuation()),
            [{'String', <<"42">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"roundtrip Int -> String -> Int", fun() ->
            C = eval("123 to-string to-int", af_interpreter:new_continuation()),
            [{'Int', 123}] = C#continuation.data_stack
        end} end
    ]}.

%% --- split ---

split_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"split by comma", fun() ->
            C = eval("\"a,b,c\" \",\" split", af_interpreter:new_continuation()),
            [{'List', [{'String', <<"a">>}, {'String', <<"b">>}, {'String', <<"c">>}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"split no match returns single-element list", fun() ->
            C = eval("\"hello\" \",\" split", af_interpreter:new_continuation()),
            [{'List', [{'String', <<"hello">>}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"split by space", fun() ->
            C = eval("\"one two three\" \" \" split", af_interpreter:new_continuation()),
            [{'List', [{'String', <<"one">>}, {'String', <<"two">>}, {'String', <<"three">>}]}] = C#continuation.data_stack
        end} end
    ]}.

%% --- contains ---

contains_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"contains found", fun() ->
            C = eval("\"hello world\" \"world\" contains", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"contains not found", fun() ->
            C = eval("\"hello\" \"xyz\" contains", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"contains empty needle", fun() ->
            C = eval("\"hello\" \"\" contains", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end
    ]}.

%% --- starts-with ---

starts_with_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"starts-with match", fun() ->
            C = eval("\"hello world\" \"hello\" starts-with", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"starts-with no match", fun() ->
            C = eval("\"hello world\" \"world\" starts-with", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"starts-with empty prefix", fun() ->
            C = eval("\"hello\" \"\" starts-with", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end
    ]}.

%% --- ends-with ---

ends_with_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"ends-with match", fun() ->
            C = eval("\"hello world\" \"world\" ends-with", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"ends-with no match", fun() ->
            C = eval("\"hello world\" \"hello\" ends-with", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"ends-with longer suffix than target", fun() ->
            C = eval("\"hi\" \"hello\" ends-with", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end
    ]}.

%% --- trim ---

trim_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"trim leading and trailing spaces", fun() ->
            C = eval("\"  hello  \" trim", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"trim no whitespace", fun() ->
            C = eval("\"hello\" trim", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"trim all whitespace", fun() ->
            C = eval("\"   \" trim", af_interpreter:new_continuation()),
            [{'String', <<>>}] = C#continuation.data_stack
        end} end
    ]}.

%% --- to-upper ---

to_upper_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"to-upper basic", fun() ->
            C = eval("\"hello\" to-upper", af_interpreter:new_continuation()),
            [{'String', <<"HELLO">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"to-upper already upper", fun() ->
            C = eval("\"HELLO\" to-upper", af_interpreter:new_continuation()),
            [{'String', <<"HELLO">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"to-upper mixed", fun() ->
            C = eval("\"Hello World\" to-upper", af_interpreter:new_continuation()),
            [{'String', <<"HELLO WORLD">>}] = C#continuation.data_stack
        end} end
    ]}.

%% --- to-lower ---

to_lower_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"to-lower basic", fun() ->
            C = eval("\"HELLO\" to-lower", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"to-lower already lower", fun() ->
            C = eval("\"hello\" to-lower", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"to-lower mixed", fun() ->
            C = eval("\"Hello World\" to-lower", af_interpreter:new_continuation()),
            [{'String', <<"hello world">>}] = C#continuation.data_stack
        end} end
    ]}.

%% --- reverse ---

reverse_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"reverse basic", fun() ->
            C = eval("\"hello\" reverse", af_interpreter:new_continuation()),
            [{'String', <<"olleh">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"reverse empty string", fun() ->
            C = eval("\"\" reverse", af_interpreter:new_continuation()),
            [{'String', <<>>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"reverse palindrome", fun() ->
            C = eval("\"aba\" reverse", af_interpreter:new_continuation()),
            [{'String', <<"aba">>}] = C#continuation.data_stack
        end} end
    ]}.

%% --- replace ---

replace_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"replace single occurrence", fun() ->
            C = eval("\"hello\" \"world\" \"hello world\" replace", af_interpreter:new_continuation()),
            [{'String', <<"world world">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"replace all occurrences", fun() ->
            C = eval("\"a\" \"x\" \"banana\" replace", af_interpreter:new_continuation()),
            [{'String', <<"bxnxnx">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"replace no match", fun() ->
            C = eval("\"z\" \"x\" \"hello\" replace", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end
    ]}.

%% --- substring ---

substring_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"substring from beginning", fun() ->
            C = eval("0 5 \"hello world\" substring", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"substring from middle", fun() ->
            C = eval("6 5 \"hello world\" substring", af_interpreter:new_continuation()),
            [{'String', <<"world">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"substring zero length", fun() ->
            C = eval("0 0 \"hello\" substring", af_interpreter:new_continuation()),
            [{'String', <<>>}] = C#continuation.data_stack
        end} end
    ]}.
