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

%% --- map ---

map_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"map with compiled word doubles each element", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": double Int -> Int ; 2 * .", C0),
            C2 = eval("nil 1 cons 2 cons 3 cons double map", C1),
            [{'List', Items}] = C2#continuation.data_stack,
            ?assertEqual([{'Int', 6}, {'Int', 4}, {'Int', 2}], Items)
        end} end,
        fun(_) -> {"map over empty list returns empty list", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": double Int -> Int ; 2 * .", C0),
            C2 = eval("nil double map", C1),
            [{'List', []}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"map with not word", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("nil True cons False cons True cons not map", C0),
            [{'List', Items}] = C1#continuation.data_stack,
            ?assertEqual([{'Bool', false}, {'Bool', true}, {'Bool', false}], Items)
        end} end
    ]}.

%% --- filter ---

filter_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"filter keeps elements where word returns true", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": positive? Int -> Bool ; 0 > .", C0),
            C2 = eval("nil -2 cons 3 cons -1 cons 5 cons positive? filter", C1),
            [{'List', Items}] = C2#continuation.data_stack,
            ?assertEqual([{'Int', 5}, {'Int', 3}], Items)
        end} end,
        fun(_) -> {"filter on empty list returns empty", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": positive? Int -> Bool ; 0 > .", C0),
            C2 = eval("nil positive? filter", C1),
            [{'List', []}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"filter removes all when none match", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": positive? Int -> Bool ; 0 > .", C0),
            C2 = eval("nil -1 cons -2 cons positive? filter", C1),
            [{'List', []}] = C2#continuation.data_stack
        end} end
    ]}.

%% --- reduce ---

reduce_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"reduce sums a list", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("nil 1 cons 2 cons 3 cons 0 + reduce", C0),
            [{'Int', 6}] = C1#continuation.data_stack
        end} end,
        fun(_) -> {"reduce with compiled word", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": mul Int Int -> Int ; * .", C0),
            C2 = eval("nil 2 cons 3 cons 4 cons 1 mul reduce", C1),
            [{'Int', 24}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"reduce empty list returns initial value", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("nil 99 + reduce", C0),
            [{'Int', 99}] = C1#continuation.data_stack
        end} end
    ]}.

%% --- each ---

each_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"each consumes list and word name, leaves stack clean", fun() ->
            C0 = af_interpreter:new_continuation(),
            %% Define a word with Int input so it won't dispatch on List TOS
            C1 = eval(": inc Int -> Int ; 1 + .", C0),
            C2 = eval("42 nil 1 cons 2 cons inc each", C1),
            %% each should consume the list and word, leaving 42 below
            [{'Int', 42}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"each on empty list is a no-op", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": inc Int -> Int ; 1 + .", C0),
            C2 = eval("42 nil inc each", C1),
            [{'Int', 42}] = C2#continuation.data_stack
        end} end
    ]}.

%% --- error cases ---

error_cases_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"head on empty list raises error", fun() ->
            ?assertError(_, eval("nil head", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"tail on empty list raises error", fun() ->
            ?assertError(_, eval("nil tail", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"last on empty list raises error", fun() ->
            ?assertError(_, eval("nil last", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"nth with negative index raises error", fun() ->
            ?assertError(_, eval("nil 1 cons -1 nth", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"nth with index past end raises error", fun() ->
            ?assertError(_, eval("nil 1 cons 5 nth", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- drop edge cases ---

drop_edge_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"drop more than length gives empty list", fun() ->
            C = eval("nil 1 cons 2 cons 10 drop", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end
    ]}.

%% --- contains? with different types ---

contains_mixed_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"contains? with bool in int list", fun() ->
            C = eval("nil 1 cons 2 cons True contains?", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"contains? finds string in mixed list", fun() ->
            C = eval("nil 1 cons \"hello\" cons \"hello\" contains?", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end
    ]}.

%% --- flatten edge cases ---

flatten_edge_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"flatten list with only non-list items unchanged", fun() ->
            C = eval("nil 1 cons 2 cons 3 cons flatten", af_interpreter:new_continuation()),
            [{'List', [{'Int', 3}, {'Int', 2}, {'Int', 1}]}] = C#continuation.data_stack
        end} end
    ]}.

%% --- map/filter/reduce edge cases ---

higher_order_edge_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"map with single element", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": double Int -> Int ; 2 * .", C0),
            C2 = eval("nil 5 cons double map", C1),
            [{'List', [{'Int', 10}]}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"filter keeps all when all match", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": positive? Int -> Bool ; 0 > .", C0),
            C2 = eval("nil 1 cons 2 cons 3 cons positive? filter", C1),
            [{'List', Items}] = C2#continuation.data_stack,
            ?assertEqual(3, length(Items))
        end} end,
        fun(_) -> {"reduce with single element", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("nil 42 cons 0 + reduce", C0),
            [{'Int', 42}] = C1#continuation.data_stack
        end} end
    ]}.
