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
    af_type:reset().

product_type_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"define a product type", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack),
            ?assertMatch({ok, _}, af_type:get_type('Point'))
        end} end,
        fun(_) -> {"construct a product type instance", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point", C1),
            %% New storage: {TypeName, V1, V2, ...} with raw field values.
            [Instance] = C2#continuation.data_stack,
            ?assertEqual('Point', element(1, Instance)),
            ?assertEqual(10, element(2, Instance)),
            ?assertEqual(20, element(3, Instance))
        end} end,
        fun(_) -> {"getter is non-destructive (leaves instance on stack)", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point x", C1),
            %% Non-destructive: tagged value on TOS, instance below
            [{'Int', 10}, Instance] = C2#continuation.data_stack,
            ?assertEqual('Point', element(1, Instance))
        end} end,
        fun(_) -> {"getter retrieves second field", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point y", C1),
            [{'Int', 20}, Instance] = C2#continuation.data_stack,
            ?assertEqual('Point', element(1, Instance))
        end} end,
        fun(_) -> {"multiple getters chain without dup", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point x swap y", C1),
            [{'Int', 20}, Instance, {'Int', 10}] = C2#continuation.data_stack,
            ?assertEqual('Point', element(1, Instance))
        end} end,
        fun(_) -> {"setter updates field value", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point 99 int x!", C1),
            [Instance] = C2#continuation.data_stack,
            ?assertEqual('Point', element(1, Instance)),
            ?assertEqual(99, element(2, Instance)),
            ?assertEqual(20, element(3, Instance))
        end} end,
        fun(_) -> {"product type with single field", fun() ->
            C1 = eval("type Wrapper val Int .", af_interpreter:new_continuation()),
            C2 = eval("42 int wrapper val", C1),
            [{'Int', 42}, Instance] = C2#continuation.data_stack,
            ?assertEqual('Wrapper', element(1, Instance))
        end} end,
        fun(_) -> {"product type with three fields", fun() ->
            C1 = eval("type Color r Int g Int b Int .", af_interpreter:new_continuation()),
            C2 = eval("255 int 128 int 0 int color", C1),
            [Instance] = C2#continuation.data_stack,
            ?assertEqual('Color', element(1, Instance)),
            ?assertEqual(255, element(2, Instance)),
            ?assertEqual(128, element(3, Instance)),
            ?assertEqual(0, element(4, Instance))
        end} end,
        fun(_) -> {"product type works with compiled words", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval(": origin -> Point ; 0 int 0 int point .", C1),
            C3 = eval("origin x swap drop", C2),
            ?assertEqual([{'Int', 0}], C3#continuation.data_stack)
        end} end,
        fun(_) -> {"getter after setter returns updated value", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point 99 int x! x swap drop", C1),
            ?assertEqual([{'Int', 99}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"drop instance after getter for clean stack", fun() ->
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            C2 = eval("10 int 20 int point x swap drop", C1),
            ?assertEqual([{'Int', 10}], C2#continuation.data_stack)
        end} end
    ]}.

%% Regression: the pattern "swap dup <getter> rot <op> <setter>" was broken
%% because non-destructive getters push value AND keep instance on stack,
%% producing wrong stack layout after rot. The correct pattern is:
%% "over <getter> rot <op> <setter> swap drop"
field_update_pattern_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"over-getter-rot-op-setter pattern updates field correctly", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            %% over: [Int(5), Counter(val=10)] -> [Counter, Int(5), Counter]
            %% value: -> [Int(10), Counter, Int(5), Counter]
            %% rot:  -> [Int(5), Int(10), Counter, Counter]
            %% +:    -> [Int(15), Counter, Counter]
            %% value!: -> [Counter(val=15), Counter]
            %% swap drop: -> [Counter(val=15)]
            C2 = eval(": add-value Counter Int -> Counter ; over value rot + value! swap drop .", C1),
            C3 = eval("10 counter 5 add-value", C2),
            [Instance] = C3#continuation.data_stack,
            ?assertEqual('Counter', element(1, Instance)),
            ?assertEqual(15, element(2, Instance))
        end} end,

        fun(_) -> {"field subtract pattern works correctly", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": sub-value Counter Int -> Counter ; over value rot - value! swap drop .", C1),
            C3 = eval("10 counter 3 sub-value", C2),
            [Instance] = C3#continuation.data_stack,
            ?assertEqual('Counter', element(1, Instance)),
            ?assertEqual(7, element(2, Instance))
        end} end,

        fun(_) -> {"chained field updates preserve state", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": add-value Counter Int -> Counter ; over value rot + value! swap drop .", C1),
            C3 = eval("0 counter 10 add-value 20 add-value 30 add-value", C2),
            [Instance] = C3#continuation.data_stack,
            ?assertEqual('Counter', element(1, Instance)),
            ?assertEqual(60, element(2, Instance))
        end} end
    ]}.
