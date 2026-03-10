-module(af_repl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

setup() ->
    af_type:reset().

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

env_test_() ->
    {foreach, fun() -> ok end, fun(_) -> ok end, [
        fun(_) -> {"load_env reads .env file", fun() ->
            %% Write a temporary .env file
            File = "/tmp/af_test_env_" ++ integer_to_list(erlang:unique_integer([positive])),
            ok = file:write_file(File, <<"TEST_AF_KEY=hello123\nTEST_AF_NUM=42\n">>),
            af_repl:load_env(File),
            ?assertEqual("hello123", os:getenv("TEST_AF_KEY")),
            ?assertEqual("42", os:getenv("TEST_AF_NUM")),
            file:delete(File),
            os:unsetenv("TEST_AF_KEY"),
            os:unsetenv("TEST_AF_NUM")
        end} end,
        fun(_) -> {"load_env handles quotes and comments", fun() ->
            File = "/tmp/af_test_env2_" ++ integer_to_list(erlang:unique_integer([positive])),
            ok = file:write_file(File, <<"# comment line\nQUOTED_VAL=\"with spaces\"\nSINGLE='quoted'\n">>),
            af_repl:load_env(File),
            ?assertEqual("with spaces", os:getenv("QUOTED_VAL")),
            ?assertEqual("quoted", os:getenv("SINGLE")),
            file:delete(File),
            os:unsetenv("QUOTED_VAL"),
            os:unsetenv("SINGLE")
        end} end,
        fun(_) -> {"load_env with missing file is ok", fun() ->
            ?assertEqual(ok, af_repl:load_env("/tmp/nonexistent_env_file_12345"))
        end} end
    ]}.
