-module(op).

-export([op_int/1]).

-type cont() :: thread:continuation().

-type thread() :: thread:thread(). % Replace with the actual module and type

-export_type([op/0]). %, op/1]).

-type op() :: fun((cont()) -> cont()).
%-type op(Arg) :: fun((cont(), Arg) -> cont()) when Arg :: thread().

% -export([some_function/1]). % Export whatever functions you need

-spec op_int(op()) -> cont().
op_int(C) ->
    C.

    