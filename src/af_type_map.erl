-module(af_type_map).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0]).

init() ->
    af_type:register_type(#af_type{name = 'Map'}),

    %% map-new: -> Map (push empty map)
    af_type:add_op('Any', #operation{
        name = "map-new",
        sig_in = [],
        sig_out = ['Map'],
        impl = fun op_map_new/1
    }),

    %% map-put: Map value key -> Map (key on TOS, value below, map below that)
    af_type:add_op('Any', #operation{
        name = "map-put",
        sig_in = ['Any', 'Any', 'Map'],
        sig_out = ['Map'],
        impl = fun op_map_put/1
    }),

    %% map-get: Map key -> value (key on TOS, map below)
    af_type:add_op('Any', #operation{
        name = "map-get",
        sig_in = ['Any', 'Map'],
        sig_out = ['Any'],
        impl = fun op_map_get/1
    }),

    %% map-delete: Map key -> Map
    af_type:add_op('Any', #operation{
        name = "map-delete",
        sig_in = ['Any', 'Map'],
        sig_out = ['Map'],
        impl = fun op_map_delete/1
    }),

    %% map-has?: Map key -> Bool
    af_type:add_op('Any', #operation{
        name = "map-has?",
        sig_in = ['Any', 'Map'],
        sig_out = ['Bool'],
        impl = fun op_map_has/1
    }),

    %% map-keys: Map -> List
    af_type:add_op('Map', #operation{
        name = "map-keys",
        sig_in = ['Map'],
        sig_out = ['List'],
        impl = fun op_map_keys/1
    }),

    %% map-values: Map -> List
    af_type:add_op('Map', #operation{
        name = "map-values",
        sig_in = ['Map'],
        sig_out = ['List'],
        impl = fun op_map_values/1
    }),

    %% map-size: Map -> Int
    af_type:add_op('Map', #operation{
        name = "map-size",
        sig_in = ['Map'],
        sig_out = ['Int'],
        impl = fun op_map_size/1
    }),

    ok.

%%% Operations

op_map_new(Cont) ->
    Cont#continuation{data_stack = [{'Map', #{}} | Cont#continuation.data_stack]}.

op_map_put(Cont) ->
    [Key, Value, {'Map', M} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Map', maps:put(Key, Value, M)} | Rest]}.

op_map_get(Cont) ->
    [Key, {'Map', M} | Rest] = Cont#continuation.data_stack,
    case maps:find(Key, M) of
        {ok, Value} ->
            Cont#continuation{data_stack = [Value | Rest]};
        error ->
            Msg = lists:flatten(io_lib:format(
                "Key ~s not found in map", [af_error:format_value(Key)]
            )),
            af_error:raise(map_key_not_found, Msg, Cont)
    end.

op_map_delete(Cont) ->
    [Key, {'Map', M} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Map', maps:remove(Key, M)} | Rest]}.

op_map_has(Cont) ->
    [Key, {'Map', M} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', maps:is_key(Key, M)} | Rest]}.

op_map_keys(Cont) ->
    [{'Map', M} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'List', maps:keys(M)} | Rest]}.

op_map_values(Cont) ->
    [{'Map', M} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'List', maps:values(M)} | Rest]}.

op_map_size(Cont) ->
    [{'Map', M} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', maps:size(M)} | Rest]}.
