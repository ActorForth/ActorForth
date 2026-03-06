-module(af_error_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("af_error.hrl").

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
    af_type_actor:init(),
    af_type_ffi:init().

%% --- Structured error format ---

format_test_() ->
    [
        {"format_value for Int", fun() ->
            ?assertEqual("42", af_error:format_value({'Int', 42}))
        end},
        {"format_value for String", fun() ->
            ?assertEqual("\"hello\"", af_error:format_value({'String', <<"hello">>}))
        end},
        {"format_value for Bool", fun() ->
            ?assertEqual("True", af_error:format_value({'Bool', true})),
            ?assertEqual("False", af_error:format_value({'Bool', false}))
        end},
        {"format_value for Atom", fun() ->
            ?assertEqual("foo", af_error:format_value({'Atom', "foo"}))
        end},
        {"format produces readable string", fun() ->
            Err = #af_error{
                type = division_by_zero,
                message = "Division by zero",
                token = #token{value = "/", file = "test.a4", line = 3, column = 5},
                stack = [{'Int', 0}, {'Int', 10}],
                word_trace = []
            },
            Formatted = af_error:format(Err),
            ?assert(is_list(Formatted)),
            ?assertNotEqual(nomatch, string:find(Formatted, "division_by_zero")),
            ?assertNotEqual(nomatch, string:find(Formatted, "test.a4:3:5")),
            ?assertNotEqual(nomatch, string:find(Formatted, "Division by zero"))
        end},
        {"format with word trace", fun() ->
            Err = #af_error{
                type = assertion_failed,
                message = "Assertion failed",
                token = #token{value = "assert", file = "test.a4", line = 5, column = 1},
                stack = [{'Bool', false}],
                word_trace = [{"my-word", #token{value = "my-word", file = "test.a4", line = 2, column = 1}}]
            },
            Formatted = af_error:format(Err),
            ?assertNotEqual(nomatch, string:find(Formatted, "my-word"))
        end}
    ].

%% --- Division by zero ---

division_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"division by zero raises structured error", fun() ->
            ?assertError({af_error, division_by_zero, _, _, _, _},
                eval("10 0 /", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- Assertion errors ---

assertion_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"assert false raises structured error", fun() ->
            ?assertError({af_error, assertion_failed, _, _, _, _},
                eval("False assert", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"assert-eq mismatch raises structured error", fun() ->
            ?assertError({af_error, assert_eq_failed, _, _, _, _},
                eval("42 43 assert-eq", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"assert-eq message shows expected and actual", fun() ->
            try
                eval("42 43 assert-eq", af_interpreter:new_continuation()),
                ?assert(false)
            catch error:#af_error{message = Msg} ->
                ?assertNotEqual(nomatch, string:find(Msg, "42")),
                ?assertNotEqual(nomatch, string:find(Msg, "43"))
            end
        end} end
    ]}.

%% --- Empty list errors ---

list_error_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"head on empty list", fun() ->
            ?assertError({af_error, empty_list, _, _, _, _},
                eval("nil head", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"tail on empty list", fun() ->
            ?assertError({af_error, empty_list, _, _, _, _},
                eval("nil tail", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- Word trace ---

word_trace_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"word trace captures calling word", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": bad-div Int -> Int ; 0 / .", C0),
            try
                eval("10 bad-div", C1),
                ?assert(false)
            catch error:#af_error{word_trace = Trace} ->
                ?assertMatch([{"bad-div", _} | _], Trace)
            end
        end} end,
        fun(_) -> {"nested word trace shows chain", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": inner Int -> Int ; 0 / .", C0),
            C2 = eval(": outer Int -> Int ; inner .", C1),
            try
                eval("10 outer", C2),
                ?assert(false)
            catch error:#af_error{word_trace = Trace} ->
                ?assertMatch([{"inner", _}, {"outer", _} | _], Trace)
            end
        end} end
    ]}.
