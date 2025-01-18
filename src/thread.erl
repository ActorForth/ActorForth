-module(thread).

-export([empty_continuation/0]).

-export_type([type/0, thread/0, continuation/0]).


-type type() :: atom().
-type stack_item() :: {type(), term() | function()}.

-type thread() :: [function()].


-record(continuation, {
    data_stack :: list(),
    return_stack :: list(),
    next_op :: reference() | 'end',
    current_token :: repl:token()
}).

-type op_ref() :: reference() | 'end'.
-type continuation() :: #continuation{
    data_stack :: stack:stack(),
    return_stack :: stack:stack(),
    next_op :: op_ref(),
    current_token :: repl:token()
}.

empty_continuation() ->
    #continuation{
        data_stack = [],
        return_stack = [],
        next_op = 'end',
        current_token = #token{
            value = "",
            column = 0,
            line = 0,
            file = "(unknown)"
        }
    }.