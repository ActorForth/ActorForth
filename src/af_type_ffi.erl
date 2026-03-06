-module(af_type_ffi).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0]).

init() ->
    %% erlang-apply : List Atom Atom -> Any
    %% Takes args list (List), function name (Atom), module name (Atom) from stack
    %% Calls erlang:apply(Module, Function, Args), converts result to stack item
    af_type:add_op('Any', #operation{
        name = "erlang-apply",
        sig_in = ['Atom', 'Atom', 'List'],
        sig_out = ['Any'],
        impl = fun op_erlang_apply/1
    }),

    %% erlang-apply/0 : Atom Atom -> Any
    %% Zero-argument version for convenience
    af_type:add_op('Any', #operation{
        name = "erlang-apply0",
        sig_in = ['Atom', 'Atom'],
        sig_out = ['Any'],
        impl = fun op_erlang_apply0/1
    }),

    ok.

%%% Operations

op_erlang_apply(Cont) ->
    [{'Atom', ModStr}, {'Atom', FunStr}, {'List', ArgItems} | Rest] = Cont#continuation.data_stack,
    Module = list_to_atom(ModStr),
    Function = list_to_atom(FunStr),
    ErlArgs = [af_term:from_stack_item(A) || A <- ArgItems],
    try
        Result = erlang:apply(Module, Function, ErlArgs),
        StackItem = af_term:to_stack_item(Result),
        Cont#continuation{data_stack = [StackItem | Rest]}
    catch
        Class:Reason ->
            Msg = lists:flatten(io_lib:format("~p:~p/~p failed: ~p:~p",
                [Module, Function, length(ErlArgs), Class, Reason])),
            af_error:raise(ffi_error, Msg, Cont)
    end.

op_erlang_apply0(Cont) ->
    [{'Atom', ModStr}, {'Atom', FunStr} | Rest] = Cont#continuation.data_stack,
    Module = list_to_atom(ModStr),
    Function = list_to_atom(FunStr),
    try
        Result = erlang:apply(Module, Function, []),
        StackItem = af_term:to_stack_item(Result),
        Cont#continuation{data_stack = [StackItem | Rest]}
    catch
        Class:Reason ->
            Msg = lists:flatten(io_lib:format("~p:~p/0 failed: ~p:~p",
                [Module, Function, Class, Reason])),
            af_error:raise(ffi_error, Msg, Cont)
    end.
