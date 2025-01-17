-module(stack).

-export_type([stack/0]).


-type type() :: atom().
-type stack_item() :: {type(), term() | function()}.
-type stack() :: [stack_item()].

-export([new/0, push/2, pop/1, swap/1]).

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
    
    