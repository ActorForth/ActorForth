-module(af_type_string).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

init() ->
    af_type:register_type(#af_type{name = 'String'}),

    %% Constructor: Atom -> String
    af_type:add_op('Any', #operation{
        name = "string",
        sig_in = ['Atom'],
        sig_out = ['String'],
        impl = fun op_string/1
    }),

    %% Pass-through: String -> String
    af_type:add_op('Any', #operation{
        name = "string",
        sig_in = ['String'],
        sig_out = ['String'],
        impl = fun op_string_passthrough/1
    }),

    %% concat: String String -> String
    af_type:add_op('String', #operation{
        name = "concat",
        sig_in = ['String', 'String'],
        sig_out = ['String'],
        impl = fun op_concat/1
    }),

    %% length: String -> Int
    af_type:add_op('String', #operation{
        name = "length",
        sig_in = ['String'],
        sig_out = ['Int'],
        impl = fun op_length/1
    }),

    %% to-atom: String -> Atom
    af_type:add_op('String', #operation{
        name = "to-atom",
        sig_in = ['String'],
        sig_out = ['Atom'],
        impl = fun op_to_atom/1
    }),

    %% to-int: String -> Int
    af_type:add_op('String', #operation{
        name = "to-int",
        sig_in = ['String'],
        sig_out = ['Int'],
        impl = fun op_to_int/1
    }),

    %% to-string: Int -> String
    af_type:add_op('Int', #operation{
        name = "to-string",
        sig_in = ['Int'],
        sig_out = ['String'],
        impl = fun op_int_to_string/1
    }),

    ok.

%%% Operations

op_string(Cont) ->
    [{'Atom', Value} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'String', list_to_binary(Value)} | Rest]}.

op_string_passthrough(Cont) -> Cont.

op_concat(Cont) ->
    [{'String', A}, {'String', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'String', <<B/binary, A/binary>>} | Rest]}.

op_length(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', byte_size(S)} | Rest]}.

op_to_atom(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Atom', binary_to_list(S)} | Rest]}.

op_to_int(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', binary_to_integer(S)} | Rest]}.

op_int_to_string(Cont) ->
    [{'Int', N} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'String', integer_to_binary(N)} | Rest]}.
