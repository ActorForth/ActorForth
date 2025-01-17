-module(stack_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test suite for new/0
new_test() ->
    ?assertEqual([], stack:new()).

%% Test suite for push/2
push_test() ->
    ?assertEqual([{a, 1}], stack:push({a, 1}, stack:new())),
    ?assertEqual([{b, 2}, {a, 1}], stack:push({b, 2}, [{a, 1}])).

%% Test suite for pop/1
pop_test() ->
    ?assertMatch({'EXIT', {empty_stack, _}}, (catch stack:pop([]))),
    Stack1 = stack:push({a, 1}, stack:new()),
    ?assertEqual({{a, 1}, []}, stack:pop(Stack1)),
    Stack2 = stack:push({b, 2}, stack:push({a, 1}, stack:new())),
    ?assertEqual({{b, 2}, [{a, 1}]}, stack:pop(Stack2)).

%% Test suite for swap/1
swap_test() ->
    ?assertMatch({'EXIT', {insufficient_elements, _}}, (catch stack:swap([]))),
    Stack1 = stack:push({a, 1}, stack:new()),
    ?assertMatch({'EXIT', {insufficient_elements, _}}, (catch stack:swap(Stack1))),
    ?assertEqual([{b, 2}, {a, 1}], stack:swap([{a, 1}, {b, 2}])),
    ?assertEqual([{c, 3}, {b, 2}, {a, 1}], stack:swap([{b, 2}, {c, 3}, {a, 1}])).