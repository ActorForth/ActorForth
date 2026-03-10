-module(af_load_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

load_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"load a library file and use its words", fun() ->
            C1 = eval("\"samples/lib_math.a4\" load", af_interpreter:new_continuation()),
            %% square should now be defined
            C2 = eval("5 square", C1),
            [{'Int', 25}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"loaded words compose", fun() ->
            C1 = eval("\"samples/lib_math.a4\" load", af_interpreter:new_continuation()),
            C2 = eval("3 double square", C1),
            %% 3 double = 6, 6 square = 36
            [{'Int', 36}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"load multiple libraries", fun() ->
            C1 = eval("\"samples/lib_math.a4\" load", af_interpreter:new_continuation()),
            C2 = eval("7 cube", C1),
            [{'Int', 343}] = C2#continuation.data_stack
        end} end,
        fun(_) -> {"load nonexistent file raises error", fun() ->
            ?assertError({af_error, load_error, _, _, _, _},
                eval("\"nonexistent.a4\" load", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"load preserves stack state", fun() ->
            C1 = eval("42", af_interpreter:new_continuation()),
            C2 = eval("\"samples/lib_math.a4\" load", C1),
            %% Stack should still have the 42
            [{'Int', 42}] = C2#continuation.data_stack,
            %% And square should work
            C3 = eval("5 square", C2),
            [{'Int', 25}, {'Int', 42}] = C3#continuation.data_stack
        end} end,

        %% Regression: load resolves paths relative to current file's directory.
        %% When file A is in samples/ and loads "lib_math.a4", the path should
        %% resolve to "samples/lib_math.a4", NOT "samples/samples/lib_math.a4".
        fun(_) -> {"load resolves relative path from file directory", fun() ->
            %% Simulate loading from within samples/ directory
            %% by setting token.file to a path inside samples/
            Tokens = af_parser:parse("\"lib_math.a4\" load", "samples/test_file.a4"),
            C1 = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
            C2 = eval("5 square", C1),
            [{'Int', 25}] = C2#continuation.data_stack
        end} end
    ]}.
