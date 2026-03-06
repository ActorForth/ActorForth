-module(af_type_bool).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

init() ->
    af_type:register_type(#af_type{name = 'Bool'}),

    %% Literal handler: try to parse token as boolean
    af_type:add_op('Bool', #operation{
        name = "literal",
        sig_in = ['Atom'],
        sig_out = ['Bool'],
        impl = fun op_literal/1
    }),

    %% Constructor: bool (Atom -> Bool) — in Any dict
    af_type:add_op('Any', #operation{
        name = "bool",
        sig_in = ['Atom'],
        sig_out = ['Bool'],
        impl = fun op_bool/1
    }),

    %% Constructor pass-through: bool (Bool -> Bool) — no-op when already Bool
    af_type:add_op('Any', #operation{
        name = "bool",
        sig_in = ['Bool'],
        sig_out = ['Bool'],
        impl = fun op_bool_passthrough/1
    }),

    %% not: Bool -> Bool
    af_type:add_op('Bool', #operation{
        name = "not", sig_in = ['Bool'], sig_out = ['Bool'],
        impl = fun op_not/1
    }),

    %% Comparison operators — registered in Any so they work on any type pair
    lists:foreach(fun({Name, Fun}) ->
        af_type:add_op('Any', #operation{
            name = Name, sig_in = ['Any', 'Any'], sig_out = ['Bool'],
            impl = Fun
        })
    end, [
        {"==", fun op_eq/1},
        {"!=", fun op_neq/1},
        {"<",  fun op_lt/1},
        {">",  fun op_gt/1},
        {"<=", fun op_lte/1},
        {">=", fun op_gte/1}
    ]),
    ok.

%%% Operations

op_literal(Cont) ->
    [{'Atom', Value} | Rest] = Cont#continuation.data_stack,
    BoolVal = case Value of
        "True"  -> true;
        "true"  -> true;
        "False" -> false;
        "false" -> false;
        _ -> error({not_a_bool, Value})
    end,
    Cont#continuation{data_stack = [{'Bool', BoolVal} | Rest]}.

op_bool(Cont) ->
    [{'Atom', Value} | Rest] = Cont#continuation.data_stack,
    BoolVal = case Value of
        "True"  -> true;
        "true"  -> true;
        "False" -> false;
        "false" -> false;
        _ -> error({type_error, {cannot_convert_to_bool, Value}})
    end,
    Cont#continuation{data_stack = [{'Bool', BoolVal} | Rest]}.

op_bool_passthrough(Cont) -> Cont.

op_not(Cont) ->
    [{'Bool', V} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', not V} | Rest]}.

op_eq(Cont) ->
    [{_, A}, {_, B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', B =:= A} | Rest]}.

op_neq(Cont) ->
    [{_, A}, {_, B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', B =/= A} | Rest]}.

op_lt(Cont) ->
    [{_, A}, {_, B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', B < A} | Rest]}.

op_gt(Cont) ->
    [{_, A}, {_, B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', B > A} | Rest]}.

op_lte(Cont) ->
    [{_, A}, {_, B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', B =< A} | Rest]}.

op_gte(Cont) ->
    [{_, A}, {_, B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', B >= A} | Rest]}.
