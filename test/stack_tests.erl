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

%% Test suite for dup/1
dup_test() ->
    ?assertEqual([{a, 1}, {a, 1}], stack:dup([{a, 1}])),
    ?assertEqual([{b, 2}, {b, 2}, {a, 1}], stack:dup([{b, 2}, {a, 1}])),
    ?assertMatch({'EXIT', {empty_stack, _}}, (catch stack:dup([]))).

%% Test suite for drop/1
drop_test() ->
    ?assertEqual([], stack:drop([{a, 1}])),
    ?assertEqual([{a, 1}], stack:drop([{b, 2}, {a, 1}])),
    ?assertMatch({'EXIT', {empty_stack, _}}, (catch stack:drop([]))).

%% Test suite for peek/1
peek_test() ->
    ?assertEqual({a, 1}, stack:peek([{a, 1}])),
    ?assertEqual({b, 2}, stack:peek([{b, 2}, {a, 1}])),
    ?assertMatch({'EXIT', {empty_stack, _}}, (catch stack:peek([]))).