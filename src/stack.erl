-module(stack).

-export_type([stack/0, stack_item/0]).


-type type() :: atom().
-type stack_item() :: {type(), term() | function()}.
-type stack() :: [stack_item()].

-export([new/0, push/2, pop/1, swap/1, dup/1, drop/1, peek/1]).

%% @doc Creates a new, empty stack.
-spec new() -> stack().
new() ->
    [].

%% @doc Pushes an item onto the stack.
-spec push(stack_item(), stack()) -> stack().
push(Item, Stack) ->
    [Item | Stack].

%% @doc Pops the top item from the stack.
-spec pop(stack()) -> {stack_item(), stack()} | no_return().
pop([]) ->
    error(empty_stack);
pop([Top | Rest]) ->
    {Top, Rest}.

%% @doc Swaps the top two items on the stack.
-spec swap(stack()) -> stack() | no_return().
swap([A, B | Rest]) ->
    [B, A | Rest];
swap(_) ->
    error(insufficient_elements).

%% @doc Duplicates the top item on the stack.
-spec dup(stack()) -> stack() | no_return().
dup([Top | _] = Stack) ->
    [Top | Stack];
dup([]) ->
    error(empty_stack).

%% @doc Removes the top item from the stack.
-spec drop(stack()) -> stack() | no_return().
drop([_ | Rest]) ->
    Rest;
drop([]) ->
    error(empty_stack).

%% @doc Returns the top item without removing it.
-spec peek(stack()) -> stack_item() | no_return().
peek([Top | _]) ->
    Top;
peek([]) ->
    error(empty_stack).
