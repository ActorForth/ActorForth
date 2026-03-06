-module(af_repl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

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

repl_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"interpret_line parses and interprets", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = af_repl:interpret_line("42 int", C0),
            ?assertEqual([{'Int', 42}], C1#continuation.data_stack)
        end} end,
        fun(_) -> {"interpret_line threads continuation", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = af_repl:interpret_line("10 int", C0),
            C2 = af_repl:interpret_line("20 int +", C1),
            ?assertEqual([{'Int', 30}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"interpret_line with word definition", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = af_repl:interpret_line(": double Int -> Int ; dup + .", C0),
            C2 = af_repl:interpret_line("5 int double", C1),
            ?assertEqual([{'Int', 10}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"interpret_line with empty input", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = af_repl:interpret_line("", C0),
            ?assertEqual([], C1#continuation.data_stack)
        end} end,
        fun(_) -> {"error in interpret_line is catchable", fun() ->
            C0 = af_interpreter:new_continuation(),
            %% Calling + with no ints on stack will crash
            Result = catch af_repl:interpret_line("garbage bool", C0),
            ?assertMatch({'EXIT', _}, Result)
        end} end
    ]}.
