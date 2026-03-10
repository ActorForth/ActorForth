-module(af_list_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

%% --- nil ---

nil_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"nil pushes empty list", fun() ->
            C = eval("nil", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"two nils push two empty lists", fun() ->
            C = eval("nil nil", af_interpreter:new_continuation()),
            [{'List', []}, {'List', []}] = C#continuation.data_stack
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

%% --- append ---

append_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"append two non-empty lists", fun() ->
            C = eval("nil 1 cons 2 cons nil 3 cons 4 cons append", af_interpreter:new_continuation()),
            [{'List', [{'Int', 2}, {'Int', 1}, {'Int', 4}, {'Int', 3}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"append empty list to non-empty", fun() ->
            C = eval("nil 1 cons 2 cons nil append", af_interpreter:new_continuation()),
            [{'List', [{'Int', 2}, {'Int', 1}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"append non-empty to empty", fun() ->
            C = eval("nil nil 1 cons append", af_interpreter:new_continuation()),
            [{'List', [{'Int', 1}]}] = C#continuation.data_stack
        end} end
    ]}.

%% --- reverse ---

reverse_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"reverse a list", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons reverse", af_interpreter:new_continuation()),
            [{'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"reverse empty list", fun() ->
            C = eval("nil reverse", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"reverse single element", fun() ->
            C = eval("nil 42 cons reverse", af_interpreter:new_continuation()),
            [{'List', [{'Int', 42}]}] = C#continuation.data_stack
        end} end
    ]}.

%% --- nth ---

nth_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"nth gets first element", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons 0 nth", af_interpreter:new_continuation()),
            [{'Int', 3}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"nth gets last element", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons 2 nth", af_interpreter:new_continuation()),
            [{'Int', 1}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"nth gets middle element", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons 1 nth", af_interpreter:new_continuation()),
            [{'Int', 2}] = C#continuation.data_stack
        end} end
    ]}.

%% --- last ---

last_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"last of multi-element list", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons last", af_interpreter:new_continuation()),
            [{'Int', 1}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"last of single-element list", fun() ->
            C = eval("nil 42 cons last", af_interpreter:new_continuation()),
            [{'Int', 42}] = C#continuation.data_stack
        end} end
    ]}.

%% --- take ---

take_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"take 2 from 3-element list", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons 2 take", af_interpreter:new_continuation()),
            [{'List', [{'Int', 3}, {'Int', 2}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"take 0 gives empty list", fun() ->
            C = eval("nil 1 cons 2 cons 0 take", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"take more than length gives whole list", fun() ->
            C = eval("nil 1 cons 2 cons 10 take", af_interpreter:new_continuation()),
            [{'List', [{'Int', 2}, {'Int', 1}]}] = C#continuation.data_stack
        end} end
    ]}.

%% --- drop ---

drop_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"drop 1 from 3-element list", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons 1 drop", af_interpreter:new_continuation()),
            [{'List', [{'Int', 2}, {'Int', 1}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"drop 0 keeps all", fun() ->
            C = eval("nil 1 cons 2 cons 0 drop", af_interpreter:new_continuation()),
            [{'List', [{'Int', 2}, {'Int', 1}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"drop all gives empty list", fun() ->
            C = eval("nil 1 cons 2 cons 2 drop", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end
    ]}.

%% --- empty? ---

empty_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"empty? on empty list is true", fun() ->
            C = eval("nil empty?", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"empty? on non-empty list is false", fun() ->
            C = eval("nil 1 cons empty?", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end
    ]}.

%% --- contains? ---

contains_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"contains? finds present item", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons 2 contains?", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"contains? returns false for absent item", fun() ->
            C = eval("nil 1 cons 2 cons 99 contains?", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"contains? on empty list", fun() ->
            C = eval("nil 1 contains?", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end
    ]}.

%% --- flatten ---

flatten_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"flatten nested lists", fun() ->
            %% Build: nil (nil 1 cons 2 cons) cons (nil 3 cons) cons
            %% Stack before flatten: [{List, [{List,[3]}, {List,[2,1]}]}]
            %% flatten -> [3, 2, 1]
            C = eval("nil nil 1 cons 2 cons cons nil 3 cons cons flatten", af_interpreter:new_continuation()),
            [{'List', [{'Int', 3}, {'Int', 2}, {'Int', 1}]}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"flatten empty list stays empty", fun() ->
            C = eval("nil flatten", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"flatten mixed list and non-list items", fun() ->
            %% Build: nil 42 cons (nil 1 cons) cons -> [list[1], 42] -> flatten -> [1, 42]
            C = eval("nil 42 cons nil 1 cons cons flatten", af_interpreter:new_continuation()),
            [{'List', [{'Int', 1}, {'Int', 42}]}] = C#continuation.data_stack
        end} end
    ]}.

%% --- zip ---

zip_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"zip two equal-length lists", fun() ->
            C = eval("nil 1 cons 2 cons nil 3 cons 4 cons zip", af_interpreter:new_continuation()),
            Expected = [{'Tuple', {{'Int', 2}, {'Int', 4}}},
                        {'Tuple', {{'Int', 1}, {'Int', 3}}}],
            [{'List', Expected}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"zip empty lists", fun() ->
            C = eval("nil nil zip", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end
    ]}.
