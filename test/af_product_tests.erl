-module(af_product_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init().

product_type_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"define a product type", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            %% Stack should be empty after definition
            ?assertEqual([], C1#continuation.data_stack),
            %% Type should be registered
            ?assertMatch({ok, _}, af_type:get_type('Point'))
        end} end,
        fun(_) -> {"construct a product type instance", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point", C1),
            [{TypeName, FieldMap}] = C2#continuation.data_stack,
            ?assertEqual('Point', TypeName),
            ?assertEqual({'Int', 10}, maps:get(x, FieldMap)),
            ?assertEqual({'Int', 20}, maps:get(y, FieldMap))
        end} end,
        fun(_) -> {"getter retrieves field value", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point x", C1),
            ?assertEqual([{'Int', 10}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"getter retrieves second field", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point y", C1),
            ?assertEqual([{'Int', 20}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"setter updates field value", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point 99 int x!", C1),
            [{TypeName, FieldMap}] = C2#continuation.data_stack,
            ?assertEqual('Point', TypeName),
            ?assertEqual({'Int', 99}, maps:get(x, FieldMap)),
            ?assertEqual({'Int', 20}, maps:get(y, FieldMap))
        end} end,
        fun(_) -> {"product type with single field", fun() ->
            C1 = eval("type Wrapper val Int .", af_interpreter:new_continuation()),
            C2 = eval("42 int wrapper val", C1),
            ?assertEqual([{'Int', 42}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"product type with three fields", fun() ->
            C1 = eval("type Color r Int g Int b Int .", af_interpreter:new_continuation()),
            C2 = eval("255 int 128 int 0 int color", C1),
            [{TypeName, FieldMap}] = C2#continuation.data_stack,
            ?assertEqual('Color', TypeName),
            ?assertEqual({'Int', 255}, maps:get(r, FieldMap)),
            ?assertEqual({'Int', 128}, maps:get(g, FieldMap)),
            ?assertEqual({'Int', 0}, maps:get(b, FieldMap))
        end} end,
        fun(_) -> {"product type works with compiled words", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval(": origin -> Point ; 0 int 0 int point .", C1),
            C3 = eval("origin x", C2),
            ?assertEqual([{'Int', 0}], C3#continuation.data_stack)
        end} end,
        fun(_) -> {"getter after setter returns updated value", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point 99 int x! x", C1),
            ?assertEqual([{'Int', 99}], C2#continuation.data_stack)
        end} end
    ]}.
