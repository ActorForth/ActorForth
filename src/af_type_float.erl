-module(af_type_float).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

init() ->
    af_type:register_type(#af_type{name = 'Float'}),

    %% Constructor: Atom -> Float
    af_type:add_op('Any', #operation{
        name = "float",
        sig_in = ['Atom'],
        sig_out = ['Float'],
        impl = fun op_float/1
    }),

    %% Int -> Float conversion
    af_type:add_op('Int', #operation{
        name = "to-float",
        sig_in = ['Int'],
        sig_out = ['Float'],
        impl = fun op_int_to_float/1
    }),

    %% Float -> Int conversion (truncates)
    af_type:add_op('Float', #operation{
        name = "to-int",
        sig_in = ['Float'],
        sig_out = ['Int'],
        impl = fun op_float_to_int/1
    }),

    %% Arithmetic: Float Float -> Float
    af_type:add_op('Float', #operation{
        name = "+", sig_in = ['Float', 'Float'], sig_out = ['Float'],
        impl = fun op_add/1
    }),
    af_type:add_op('Float', #operation{
        name = "-", sig_in = ['Float', 'Float'], sig_out = ['Float'],
        impl = fun op_sub/1
    }),
    af_type:add_op('Float', #operation{
        name = "*", sig_in = ['Float', 'Float'], sig_out = ['Float'],
        impl = fun op_mul/1
    }),
    af_type:add_op('Float', #operation{
        name = "/", sig_in = ['Float', 'Float'], sig_out = ['Float'],
        impl = fun op_div/1
    }),

    %% Mixed arithmetic: Int on TOS, Float below -> Float
    af_type:add_op('Any', #operation{
        name = "+", sig_in = ['Int', 'Float'], sig_out = ['Float'],
        impl = fun op_add_int_float/1
    }),
    af_type:add_op('Any', #operation{
        name = "*", sig_in = ['Int', 'Float'], sig_out = ['Float'],
        impl = fun op_mul_int_float/1
    }),
    af_type:add_op('Any', #operation{
        name = "-", sig_in = ['Int', 'Float'], sig_out = ['Float'],
        impl = fun op_sub_int_float/1
    }),
    af_type:add_op('Any', #operation{
        name = "/", sig_in = ['Int', 'Float'], sig_out = ['Float'],
        impl = fun op_div_int_float/1
    }),

    %% Mixed arithmetic: Float on TOS, Int below -> Float
    af_type:add_op('Float', #operation{
        name = "+", sig_in = ['Float', 'Int'], sig_out = ['Float'],
        impl = fun op_add_float_int/1
    }),
    af_type:add_op('Float', #operation{
        name = "*", sig_in = ['Float', 'Int'], sig_out = ['Float'],
        impl = fun op_mul_float_int/1
    }),
    af_type:add_op('Float', #operation{
        name = "-", sig_in = ['Float', 'Int'], sig_out = ['Float'],
        impl = fun op_sub_float_int/1
    }),
    af_type:add_op('Float', #operation{
        name = "/", sig_in = ['Float', 'Int'], sig_out = ['Float'],
        impl = fun op_div_float_int/1
    }),

    %% Literal handler for float detection
    af_type:add_op('Float', #operation{
        name = "literal", sig_in = ['Atom'], sig_out = ['Float'],
        impl = fun op_literal/1
    }),

    %% to-string
    af_type:add_op('Float', #operation{
        name = "to-string", sig_in = ['Float'], sig_out = ['String'],
        impl = fun op_to_string/1
    }),

    ok.

%%% Operations

op_float(Cont) ->
    [{'Atom', Value} | Rest] = Cont#continuation.data_stack,
    case parse_float(Value) of
        {ok, F} ->
            Cont#continuation{data_stack = [{'Float', F} | Rest]};
        error ->
            error({type_error, {cannot_convert_to_float, Value}})
    end.

op_int_to_float(Cont) ->
    [{'Int', N} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', float(N)} | Rest]}.

op_float_to_int(Cont) ->
    [{'Float', F} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', trunc(F)} | Rest]}.

op_add(Cont) ->
    [{'Float', A}, {'Float', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B + A} | Rest]}.

op_sub(Cont) ->
    [{'Float', A}, {'Float', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B - A} | Rest]}.

op_mul(Cont) ->
    [{'Float', A}, {'Float', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B * A} | Rest]}.

op_div(Cont) ->
    [{'Float', A}, {'Float', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B / A} | Rest]}.

%% Int on TOS, Float below
op_add_int_float(Cont) ->
    [{'Int', A}, {'Float', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B + A} | Rest]}.

op_mul_int_float(Cont) ->
    [{'Int', A}, {'Float', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B * A} | Rest]}.

op_sub_int_float(Cont) ->
    [{'Int', A}, {'Float', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B - A} | Rest]}.

op_div_int_float(Cont) ->
    [{'Int', A}, {'Float', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B / A} | Rest]}.

%% Float on TOS, Int below
op_add_float_int(Cont) ->
    [{'Float', A}, {'Int', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B + A} | Rest]}.

op_mul_float_int(Cont) ->
    [{'Float', A}, {'Int', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B * A} | Rest]}.

op_sub_float_int(Cont) ->
    [{'Float', A}, {'Int', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B - A} | Rest]}.

op_div_float_int(Cont) ->
    [{'Float', A}, {'Int', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Float', B / A} | Rest]}.

op_literal(Cont) ->
    [{'Atom', Value} | Rest] = Cont#continuation.data_stack,
    case parse_float(Value) of
        {ok, F} ->
            Cont#continuation{data_stack = [{'Float', F} | Rest]};
        error ->
            error(not_a_float)
    end.

op_to_string(Cont) ->
    [{'Float', F} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'String', list_to_binary(io_lib:format("~p", [F]))} | Rest]}.

%%% Internal

parse_float(Str) ->
    case catch list_to_float(Str) of
        F when is_float(F) -> {ok, F};
        _ -> error
    end.
