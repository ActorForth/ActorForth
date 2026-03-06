-module(af_list_tests).

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
    af_type_list:init(),
    af_type_actor:init().

%% --- nil ---

nil_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"nil pushes empty list", fun() ->
            C = eval("nil", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end
    ]}.

%% --- cons ---

cons_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"cons prepends item to empty list", fun() ->
            C = eval("nil 42 cons", af_interpreter:new_continuation()),
            [{'List', [{'Int', 42}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"cons chains build list", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons", af_interpreter:new_continuation()),
            [{'List', [{'Int', 3}, {'Int', 2}, {'Int', 1}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"cons works with mixed types", fun() ->
            C = eval("nil 42 cons True cons", af_interpreter:new_continuation()),
            [{'List', [{'Bool', true}, {'Int', 42}]}] = C#continuation.data_stack
        end} end
    ]}.

%% --- length ---

length_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"length of empty list is 0", fun() ->
            C = eval("nil length", af_interpreter:new_continuation()),
            [{'Int', 0}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"length of 3-element list", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons length", af_interpreter:new_continuation()),
            [{'Int', 3}] = C#continuation.data_stack
        end} end
    ]}.

%% --- head / tail ---

head_tail_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"head returns first item", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons head", af_interpreter:new_continuation()),
            [{'Int', 3}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"tail returns rest", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons tail", af_interpreter:new_continuation()),
            [{'List', [{'Int', 2}, {'Int', 1}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"head and tail decompose list", fun() ->
            C = eval("nil 1 cons 2 cons dup head swap tail head", af_interpreter:new_continuation()),
            [{'Int', 1}, {'Int', 2}] = C#continuation.data_stack
        end} end
    ]}.
