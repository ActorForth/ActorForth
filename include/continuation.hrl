
-record(continuation, {
    data_stack :: list(),
    return_stack :: list(),
    next_op :: reference() | 'end',
    current_token :: repl:token()
}).

-type op_ref() :: reference() | 'end'.
-type continuation() :: #continuation{
    data_stack :: list(),
    return_stack :: list(),
    next_op :: op_ref(),
    current_token :: repl:token()
}.