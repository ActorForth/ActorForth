-module(af_type_tuple).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0]).

%% Tuple type: wraps native Erlang tuples.
%% Representation: {Tuple, ErlangTuple}
%%
%% Many Erlang APIs use tuples (e.g., {ok, Value}, {error, Reason}).
%% This type bridges ActorForth with tuple-heavy Erlang interfaces.

init() ->
    af_type:register_type(#af_type{name = 'Tuple'}),

    %% to-tuple: List -> Tuple  (convert list to tuple)
    af_type:add_op('List', #operation{
        name = "to-tuple", sig_in = ['List'], sig_out = ['Tuple'],
        impl = fun op_to_tuple/1
    }),

    %% from-tuple: Tuple -> List  (convert tuple to list)
    af_type:add_op('Tuple', #operation{
        name = "from-tuple", sig_in = ['Tuple'], sig_out = ['List'],
        impl = fun op_from_tuple/1
    }),

    %% tuple-size: Tuple -> Int
    af_type:add_op('Tuple', #operation{
        name = "tuple-size", sig_in = ['Tuple'], sig_out = ['Int'],
        impl = fun op_tuple_size/1
    }),

    %% tuple-get: Int Tuple -> Any  (get element at 1-based index)
    %% Registered in Any because TOS is Int when called
    af_type:add_op('Any', #operation{
        name = "tuple-get", sig_in = ['Int', 'Tuple'], sig_out = ['Any'],
        impl = fun op_tuple_get/1
    }),

    %% make-tuple: builds tuple from N stack items
    %% N make-tuple: ( Any... Int -> Tuple )
    af_type:add_op('Any', #operation{
        name = "make-tuple", sig_in = ['Int'], sig_out = ['Tuple'],
        impl = fun op_make_tuple/1
    }),

    %% ok-tuple: ( Any -> Tuple )  convenience for {ok, Value}
    af_type:add_op('Any', #operation{
        name = "ok-tuple", sig_in = ['Any'], sig_out = ['Tuple'],
        impl = fun op_ok_tuple/1
    }),

    %% error-tuple: ( Any -> Tuple )  convenience for {error, Reason}
    af_type:add_op('Any', #operation{
        name = "error-tuple", sig_in = ['Any'], sig_out = ['Tuple'],
        impl = fun op_error_tuple/1
    }),

    %% is-ok: ( Tuple -> Bool )  check if tuple starts with ok
    af_type:add_op('Tuple', #operation{
        name = "is-ok", sig_in = ['Tuple'], sig_out = ['Bool'],
        impl = fun op_is_ok/1
    }),

    %% unwrap-ok: ( Tuple -> Any )  extract value from {ok, Value} tuple
    af_type:add_op('Tuple', #operation{
        name = "unwrap-ok", sig_in = ['Tuple'], sig_out = ['Any'],
        impl = fun op_unwrap_ok/1
    }),

    ok.

%%% Operations

op_to_tuple(Cont) ->
    [{'List', Items} | Rest] = Cont#continuation.data_stack,
    %% Convert tagged stack items to raw Erlang terms for the tuple
    RawItems = [af_term:from_stack_item(I) || I <- Items],
    Cont#continuation{data_stack = [{'Tuple', list_to_tuple(RawItems)} | Rest]}.

op_from_tuple(Cont) ->
    [{'Tuple', T} | Rest] = Cont#continuation.data_stack,
    Items = [af_term:to_stack_item(E) || E <- tuple_to_list(T)],
    Cont#continuation{data_stack = [{'List', Items} | Rest]}.

op_tuple_size(Cont) ->
    [{'Tuple', T} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', tuple_size(T)} | Rest]}.

op_tuple_get(Cont) ->
    [{'Int', N}, {'Tuple', T} | Rest] = Cont#continuation.data_stack,
    case N >= 1 andalso N =< tuple_size(T) of
        true ->
            Item = af_term:to_stack_item(element(N, T)),
            Cont#continuation{data_stack = [Item | Rest]};
        false ->
            af_error:raise(index_error,
                lists:flatten(io_lib:format("tuple index ~p out of range (size ~p)", [N, tuple_size(T)])),
                Cont)
    end.

op_make_tuple(Cont) ->
    [{'Int', N} | Rest] = Cont#continuation.data_stack,
    case length(Rest) >= N of
        true ->
            {Items, Remaining} = lists:split(N, Rest),
            RawItems = [af_term:from_stack_item(I) || I <- lists:reverse(Items)],
            Cont#continuation{data_stack = [{'Tuple', list_to_tuple(RawItems)} | Remaining]};
        false ->
            af_error:raise(stack_underflow,
                lists:flatten(io_lib:format("make-tuple needs ~p items", [N])),
                Cont)
    end.

op_ok_tuple(Cont) ->
    [Item | Rest] = Cont#continuation.data_stack,
    Raw = af_term:from_stack_item(Item),
    Cont#continuation{data_stack = [{'Tuple', {ok, Raw}} | Rest]}.

op_error_tuple(Cont) ->
    [Item | Rest] = Cont#continuation.data_stack,
    Raw = af_term:from_stack_item(Item),
    Cont#continuation{data_stack = [{'Tuple', {error, Raw}} | Rest]}.

op_is_ok(Cont) ->
    [{'Tuple', T} | Rest] = Cont#continuation.data_stack,
    IsOk = tuple_size(T) >= 1 andalso element(1, T) =:= ok,
    Cont#continuation{data_stack = [{'Bool', IsOk} | Rest]}.

op_unwrap_ok(Cont) ->
    [{'Tuple', T} | Rest] = Cont#continuation.data_stack,
    case T of
        {ok, Value} ->
            Item = af_term:to_stack_item(Value),
            Cont#continuation{data_stack = [Item | Rest]};
        _ ->
            af_error:raise(type_error,
                "unwrap-ok: tuple does not start with ok",
                Cont)
    end.
