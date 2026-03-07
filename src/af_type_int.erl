-module(af_type_int).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0]).

init() ->
    af_type:register_type(#af_type{name = 'Int'}),

    %% Literal handler: try to parse token as integer
    af_type:add_op('Int', #operation{
        name = "literal",
        sig_in = ['Atom'],
        sig_out = ['Int'],
        impl = fun op_literal/1
    }),

    %% Constructor: int (Atom -> Int) — registered in Any so it's found via fallback
    af_type:add_op('Any', #operation{
        name = "int",
        sig_in = ['Atom'],
        sig_out = ['Int'],
        impl = fun op_int/1
    }),

    %% Constructor pass-through: int (Int -> Int) — no-op when already Int
    af_type:add_op('Any', #operation{
        name = "int",
        sig_in = ['Int'],
        sig_out = ['Int'],
        impl = fun op_int_passthrough/1
    }),

    %% Arithmetic operations — registered in Int's dictionary
    af_type:add_op('Int', #operation{
        name = "+", sig_in = ['Int', 'Int'], sig_out = ['Int'],
        impl = fun op_plus/1
    }),
    af_type:add_op('Int', #operation{
        name = "-", sig_in = ['Int', 'Int'], sig_out = ['Int'],
        impl = fun op_minus/1
    }),
    af_type:add_op('Int', #operation{
        name = "*", sig_in = ['Int', 'Int'], sig_out = ['Int'],
        impl = fun op_multiply/1
    }),
    af_type:add_op('Int', #operation{
        name = "/", sig_in = ['Int', 'Int'], sig_out = ['Int'],
        impl = fun op_divide/1
    }),
    ok.

%%% Operations

op_literal(Cont) ->
    [{'Atom', Value} | Rest] = Cont#continuation.data_stack,
    IntVal = list_to_integer(Value),
    Cont#continuation{data_stack = [{'Int', IntVal} | Rest]}.

op_int(Cont) ->
    [{'Atom', Value} | Rest] = Cont#continuation.data_stack,
    IntVal = list_to_integer(Value),
    Cont#continuation{data_stack = [{'Int', IntVal} | Rest]}.

op_int_passthrough(Cont) -> Cont.

op_plus(Cont) ->
    [{'Int', A}, {'Int', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', B + A} | Rest]}.

op_minus(Cont) ->
    [{'Int', A}, {'Int', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', B - A} | Rest]}.

op_multiply(Cont) ->
    [{'Int', A}, {'Int', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', B * A} | Rest]}.

op_divide(Cont) ->
    [{'Int', A}, {'Int', B} | Rest] = Cont#continuation.data_stack,
    case A of
        0 -> af_error:raise(division_by_zero, "Division by zero", Cont);
        _ -> Cont#continuation{data_stack = [{'Int', B div A} | Rest]}
    end.
