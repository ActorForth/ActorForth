-module(af_map_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_string:init(),
    af_type_map:init(),
    af_type_list:init(),
    af_type_actor:init(),
    af_type_ffi:init().

%% --- map-new ---

new_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"map-new pushes empty map", fun() ->
            C = eval("map-new", af_interpreter:new_continuation()),
            [{'Map', M}] = C#continuation.data_stack,
            ?assertEqual(0, maps:size(M))
        end} end
    ]}.

%% --- map-put / map-get ---

put_get_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"put and get string key", fun() ->
            C = eval("map-new 42 \"name\" map-put \"name\" map-get", af_interpreter:new_continuation()),
            [{'Int', 42}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"put and get int key", fun() ->
            C = eval("map-new \"hello\" 1 map-put 1 map-get", af_interpreter:new_continuation()),
            [{'String', <<"hello">>}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"multiple puts", fun() ->
            C = eval("map-new 10 \"a\" map-put 20 \"b\" map-put", af_interpreter:new_continuation()),
            [{'Map', M}] = C#continuation.data_stack,
            ?assertEqual({'Int', 10}, maps:get({'String', <<"a">>}, M)),
            ?assertEqual({'Int', 20}, maps:get({'String', <<"b">>}, M))
        end} end,
        fun(_) -> {"put overwrites existing key", fun() ->
            C = eval("map-new 10 \"x\" map-put 99 \"x\" map-put \"x\" map-get", af_interpreter:new_continuation()),
            [{'Int', 99}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"get missing key errors", fun() ->
            ?assertError({af_error, map_key_not_found, _, _, _, _},
                eval("map-new \"missing\" map-get", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- map-delete ---

delete_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"delete removes key", fun() ->
            C = eval("map-new 42 \"x\" map-put \"x\" map-delete map-size", af_interpreter:new_continuation()),
            [{'Int', 0}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"delete nonexistent key is no-op", fun() ->
            C = eval("map-new \"nope\" map-delete map-size", af_interpreter:new_continuation()),
            [{'Int', 0}] = C#continuation.data_stack
        end} end
    ]}.

%% --- map-has? ---

has_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"has? returns true for existing key", fun() ->
            C = eval("map-new 42 \"x\" map-put \"x\" map-has?", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"has? returns false for missing key", fun() ->
            C = eval("map-new \"x\" map-has?", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end
    ]}.

%% --- map-keys / map-values ---

keys_values_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"keys of empty map", fun() ->
            C = eval("map-new map-keys", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"values of empty map", fun() ->
            C = eval("map-new map-values", af_interpreter:new_continuation()),
            [{'List', []}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"keys returns list of keys", fun() ->
            C = eval("map-new 1 \"a\" map-put 2 \"b\" map-put map-keys length", af_interpreter:new_continuation()),
            [{'Int', 2}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"values returns list of values", fun() ->
            C = eval("map-new 1 \"a\" map-put 2 \"b\" map-put map-values length", af_interpreter:new_continuation()),
            [{'Int', 2}] = C#continuation.data_stack
        end} end
    ]}.

%% --- map-size ---

size_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"size of empty map", fun() ->
            C = eval("map-new map-size", af_interpreter:new_continuation()),
            [{'Int', 0}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"size after puts", fun() ->
            C = eval("map-new 1 \"a\" map-put 2 \"b\" map-put 3 \"c\" map-put map-size", af_interpreter:new_continuation()),
            [{'Int', 3}] = C#continuation.data_stack
        end} end
    ]}.
