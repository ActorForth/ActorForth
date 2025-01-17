-module(stack_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> 
    [new_test, push_test, pop_test, swap_test].

new_test(_Config) ->
    ?assertEqual([], stack:new()).

push_test(_Config) ->
    S1 = stack:new(),
    Item = {test, 1},
    S2 = stack:push(Item, S1),
    ?assertEqual([Item], S2).

pop_test(_Config) ->
    S1 = stack:new(),
    Item = {test, 1},
    S2 = stack:push(Item, S1),
    
    % Test successful pop
    {PoppedItem, S3} = stack:pop(S2),
    ?assertEqual(Item, PoppedItem),
    ?assertEqual([], S3),

    % Test popping from an empty stack
    try stack:pop(S1) of
        _ -> ?assert(false) % This should not happen
    catch 
        error:empty_stack -> ok
    end.

swap_test(_Config) ->
    S1 = stack:new(),
    Item1 = {item1, 1},
    Item2 = {item2, 2},
    S2 = stack:push(Item1, stack:push(Item2, S1)),
    
    % Test successful swap
    Swapped = stack:swap(S2),
    ?assertEqual([Item2, Item1], Swapped), % Corrected expected result

    % Test swapping with insufficient elements
    try stack:swap(S1) of
        _ -> ?assert(false) % This should not happen
    catch 
        error:insufficient_elements -> ok
    end.