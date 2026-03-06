-module(af_string_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_string:init(),
    af_type_map:init(),
    af_type_list:init(),
    af_type_actor:init().

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
