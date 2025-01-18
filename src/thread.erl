-module(thread).

-include("token.hrl").
-include("continuation.hrl").

-export([make_continuation/0, make_continuation/1, make_continuation/2, make_continuation/3, make_continuation/4]).

-export_type([type/0, thread/0, continuation/0]).

-type type() :: atom().
-type thread() :: [function()].



-spec make_continuation() -> #continuation{}.
make_continuation() ->
    make_continuation([], [], 'end', #token{value = "", column = 0, line = 0, file = "(unknown)"}).

-spec make_continuation(#token{}) -> #continuation{}.
make_continuation(#token{} = CurrentToken) ->
    make_continuation([], [], 'end', CurrentToken).

-spec make_continuation(DataStack :: list(), ReturnStack :: list()) -> #continuation{}.
make_continuation(DataStack, ReturnStack) ->
    make_continuation(DataStack, ReturnStack, 'end', #token{value = "", column = 0, line = 0, file = "(unknown)"}).

-spec make_continuation(DataStack :: list(), ReturnStack :: list(), NextOp :: op_ref()) -> #continuation{}.
make_continuation(DataStack, ReturnStack, NextOp) ->
    make_continuation(DataStack, ReturnStack, NextOp, #token{value = "", column = 0, line = 0, file = "(unknown)"}).

-spec make_continuation(DataStack :: list(), ReturnStack :: list(), NextOp :: op_ref(), CurrentToken :: #token{}) -> #continuation{}.
make_continuation(DataStack, ReturnStack, NextOp, CurrentToken) ->
    #continuation{
        data_stack = DataStack,
        return_stack = ReturnStack,
        next_op = NextOp,
        current_token = CurrentToken
    }.
