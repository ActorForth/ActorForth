-module(af_type_any_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_string:init(),
    af_type_compiler:init().

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

%% --- over: copies 2nd item to top ---

over_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"over copies second item to top", fun() ->
            C1 = eval("1 int 2 int over", af_interpreter:new_continuation()),
            [{'Int', 1}, {'Int', 2}, {'Int', 1}] = C1#continuation.data_stack
        end} end,
        fun(_) -> {"over with different types", fun() ->
            C1 = eval("hello 2 int over", af_interpreter:new_continuation()),
            [{'Atom', "hello"}, {'Int', 2}, {'Atom', "hello"}] = C1#continuation.data_stack
        end} end
    ]}.

%% --- 2dup: duplicates top two items ---

two_dup_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"2dup copies top two items", fun() ->
            C1 = eval("1 int 2 int 2dup", af_interpreter:new_continuation()),
            [{'Int', 2}, {'Int', 1}, {'Int', 2}, {'Int', 1}] = C1#continuation.data_stack
        end} end,
        fun(_) -> {"2dup with different types", fun() ->
            C1 = eval("hello 42 int 2dup", af_interpreter:new_continuation()),
            [{'Int', 42}, {'Atom', "hello"}, {'Int', 42}, {'Atom', "hello"}] = C1#continuation.data_stack
        end} end
    ]}.

%% --- see: display word definition ---

see_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"see built-in word", fun() ->
            C1 = eval("dup see", af_interpreter:new_continuation()),
            %% see consumes the Atom and prints, stack should be empty
            ?assertEqual([], C1#continuation.data_stack)
        end} end,
        fun(_) -> {"see unknown word", fun() ->
            C1 = eval("nonexistent_word_xyz see", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end,
        fun(_) -> {"see compiled word shows body", fun() ->
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval("double see", C1),
            ?assertEqual([], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"see auto-generated word", fun() ->
            af_type_product:init(),
            C1 = eval("type Point x Int y Int .", af_interpreter:new_continuation()),
            %% x is auto-generated getter
            C2 = eval("x see", C1),
            ?assertEqual([], C2#continuation.data_stack)
        end} end
    ]}.

%% --- assert ---

assert_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"assert true passes silently", fun() ->
            C1 = eval("True assert", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end,
        fun(_) -> {"assert false raises error", fun() ->
            ?assertError(#af_error{type = assertion_failed},
                eval("False assert", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- assert-eq ---

assert_eq_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"assert-eq equal values passes", fun() ->
            C1 = eval("42 int 42 int assert-eq", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end,
        fun(_) -> {"assert-eq different values raises error", fun() ->
            ?assertError(#af_error{type = assert_eq_failed},
                eval("42 int 43 int assert-eq", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- debug ---

debug_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"debug pushes Debug marker", fun() ->
            C1 = eval("debug", af_interpreter:new_continuation()),
            [{'Debug', _}] = C1#continuation.data_stack
        end} end,
        fun(_) -> {"debug on enables debug flag", fun() ->
            C1 = eval("debug on", af_interpreter:new_continuation()),
            ?assertEqual(true, C1#continuation.debug),
            ?assertEqual([], C1#continuation.data_stack)
        end} end,
        fun(_) -> {"debug off disables debug flag", fun() ->
            C1 = eval("debug on", af_interpreter:new_continuation()),
            C2 = eval("debug off", C1),
            ?assertEqual(false, C2#continuation.debug),
            ?assertEqual([], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"debug with invalid token raises error", fun() ->
            ?assertError({debug_expected_on_off, _},
                eval("debug blah", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- load ---

load_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"load nonexistent file raises error", fun() ->
            af_type_string:init(),
            ?assertError(#af_error{type = load_error},
                eval("\"/nonexistent/file.a4\" load", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"load resolves relative path from current file dir", fun() ->
            %% Create a temp file to load
            TmpDir = "/tmp/af_load_test_" ++ integer_to_list(erlang:unique_integer([positive])),
            ok = filelib:ensure_dir(TmpDir ++ "/"),
            TmpFile = filename:join(TmpDir, "test_load.a4"),
            ok = file:write_file(TmpFile, "42 int"),
            C0 = af_interpreter:new_continuation(),
            C1 = eval("\"" ++ TmpFile ++ "\" load", C0),
            [{'Int', 42}] = C1#continuation.data_stack,
            file:delete(TmpFile),
            file:del_dir(TmpDir)
        end} end,
        fun(_) -> {"load relative path resolves against current token file", fun() ->
            %% Create a temp file
            TmpDir = "/tmp/af_load_rel_test_" ++ integer_to_list(erlang:unique_integer([positive])),
            ok = filelib:ensure_dir(TmpDir ++ "/"),
            TmpFile = filename:join(TmpDir, "included.a4"),
            ok = file:write_file(TmpFile, "99 int"),
            %% Create a continuation with current_token pointing to a file in TmpDir
            FakeFile = filename:join(TmpDir, "main.a4"),
            Tokens = af_parser:parse("\"included.a4\" load", FakeFile),
            C0 = af_interpreter:new_continuation(),
            C1 = af_interpreter:interpret_tokens(Tokens, C0),
            [{'Int', 99}] = C1#continuation.data_stack,
            file:delete(TmpFile),
            file:del_dir(TmpDir)
        end} end
    ]}.

%% --- import ---

import_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"import nonexistent file raises error", fun() ->
            af_type_string:init(),
            ?assertError(#af_error{type = import_error},
                eval("\"/nonexistent/file.a4\" import", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- compile ---

compile_error_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile nonexistent word raises error", fun() ->
            af_type_string:init(),
            ?assertError(#af_error{type = compile_error},
                eval("\"nonexistent_word_abc\" compile", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- format_sig edge cases ---

format_sig_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"see word with value-constrained sig shows typed value", fun() ->
            %% Define a word with value-constrained sig_in: "0 Int" means {Int, 0}
            C1 = eval(": zero? 0 Int -> Bool ; drop True .", af_interpreter:new_continuation()),
            %% zero? has sig_in = [{Int, 0}] which only matches Int(0)
            %% On empty stack, zero? won't match so it pushes as Atom
            C2 = eval("zero? see", C1),
            ?assertEqual([], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"see word with empty sig_out triggers format_sig empty", fun() ->
            %% 'print' has sig_in = ['Any'], sig_out = []
            %% On empty stack, 'print' can't match sig_in so it's pushed as Atom
            %% Then see looks it up and calls format_sig([]) for sig_out
            C1 = eval("print see", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end
    ]}.
