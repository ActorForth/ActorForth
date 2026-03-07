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

    %% erlang-call : Int Atom Atom Any... -> Any
    %% Natural calling convention: push args, then Module Function Arity erlang-call
    %% Pops arity, function, module, then pops N args from stack
    af_type:add_op('Any', #operation{
        name = "erlang-call",
        sig_in = ['Int', 'Atom', 'Atom'],
        sig_out = ['Any'],
        impl = fun op_erlang_call/1
    }),

    %% erlang-call0 : Atom Atom -> Any
    %% Shorthand for zero-arg calls: Module Function erlang-call0
    af_type:add_op('Any', #operation{
        name = "erlang-call0",
        sig_in = ['Atom', 'Atom'],
        sig_out = ['Any'],
        impl = fun op_erlang_call0/1
    }),

    %% erlang-new : Atom -> Tuple
    %% Call Module:new() — common Erlang/Elixir pattern
    af_type:add_op('Any', #operation{
        name = "erlang-new",
        sig_in = ['Atom'],
        sig_out = ['Tuple'],
        impl = fun op_erlang_new/1
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

%% erlang-call: takes N args directly from stack (no List building needed)
%% Stack before: [Int(arity), Atom(fun), Atom(mod), arg_N, ..., arg_1, ...]
op_erlang_call(Cont) ->
    [{'Int', Arity}, {'Atom', FunStr}, {'Atom', ModStr} | Rest] = Cont#continuation.data_stack,
    Module = list_to_atom(ModStr),
    Function = list_to_atom(FunStr),
    case length(Rest) >= Arity of
        true ->
            {ArgItems, Remaining} = lists:split(Arity, Rest),
            %% Args are TOS-first on stack; reverse for left-to-right arg order
            ErlArgs = [af_term:from_stack_item(A) || A <- lists:reverse(ArgItems)],
            try
                Result = erlang:apply(Module, Function, ErlArgs),
                StackItem = af_term:to_stack_item(Result),
                Cont#continuation{data_stack = [StackItem | Remaining]}
            catch
                Class:Reason ->
                    Msg = lists:flatten(io_lib:format("~p:~p/~p failed: ~p:~p",
                        [Module, Function, Arity, Class, Reason])),
                    af_error:raise(ffi_error, Msg, Cont)
            end;
        false ->
            af_error:raise(stack_underflow,
                lists:flatten(io_lib:format("erlang-call needs ~p args on stack", [Arity])),
                Cont)
    end.

%% erlang-call0: shorthand for Module:Function()
op_erlang_call0(Cont) ->
    [{'Atom', FunStr}, {'Atom', ModStr} | Rest] = Cont#continuation.data_stack,
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

%% erlang-new: call Module:new()
op_erlang_new(Cont) ->
    [{'Atom', ModStr} | Rest] = Cont#continuation.data_stack,
    Module = list_to_atom(ModStr),
    try
        Result = erlang:apply(Module, new, []),
        StackItem = af_term:to_stack_item(Result),
        Cont#continuation{data_stack = [StackItem | Rest]}
    catch
        Class:Reason ->
            Msg = lists:flatten(io_lib:format("~p:new/0 failed: ~p:~p",
                [Module, Class, Reason])),
            af_error:raise(ffi_error, Msg, Cont)
    end.
