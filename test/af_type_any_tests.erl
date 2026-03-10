-module(af_type_any_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

setup() ->
    af_type:reset().

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
        end} end,
        fun(_) -> {"debug on then off round-trip", fun() ->
            C1 = eval("debug on", af_interpreter:new_continuation()),
            ?assertEqual(true, C1#continuation.debug),
            C2 = eval("debug off", C1),
            ?assertEqual(false, C2#continuation.debug),
            C3 = eval("debug on", C2),
            ?assertEqual(true, C3#continuation.debug)
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

%% --- file-exists? ---

file_exists_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"file-exists? returns true for existing file", fun() ->
            C1 = eval("\"rebar.config\" file-exists?", af_interpreter:new_continuation()),
            [{'Bool', true} | _] = C1#continuation.data_stack
        end} end,
        fun(_) -> {"file-exists? returns false for nonexistent file", fun() ->
            C1 = eval("\"/nonexistent/path/xyz.txt\" file-exists?", af_interpreter:new_continuation()),
            [{'Bool', false} | _] = C1#continuation.data_stack
        end} end,
        fun(_) -> {"file-exists? returns false for directory", fun() ->
            %% file-exists? uses filelib:is_regular which returns false for dirs
            C1 = eval("\"/tmp\" file-exists?", af_interpreter:new_continuation()),
            [{'Bool', false} | _] = C1#continuation.data_stack
        end} end
    ]}.

%% --- read-file ---

read_file_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"read-file reads existing file", fun() ->
            %% Write a temp file, read it back
            TmpFile = "/tmp/af_read_test_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".txt",
            ok = file:write_file(TmpFile, <<"test content">>),
            C1 = eval("\"" ++ TmpFile ++ "\" read-file", af_interpreter:new_continuation()),
            [{'String', Content}] = C1#continuation.data_stack,
            ?assertEqual(<<"test content">>, Content),
            file:delete(TmpFile)
        end} end,
        fun(_) -> {"read-file raises error for nonexistent file", fun() ->
            ?assertError(#af_error{type = file_error},
                eval("\"/nonexistent/af_test_xyz.txt\" read-file", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- write-file ---

write_file_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"write-file writes and read-file reads back", fun() ->
            TmpFile = "/tmp/af_write_test_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".txt",
            %% write-file expects: content path on stack (path=TOS)
            WriteCmd = "\"hello from a4\" \"" ++ TmpFile ++ "\" write-file",
            C1 = eval(WriteCmd, af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack),
            %% Verify the file was written
            {ok, Bin} = file:read_file(TmpFile),
            ?assertEqual(<<"hello from a4">>, Bin),
            file:delete(TmpFile)
        end} end,
        fun(_) -> {"write-file then read-file round trip", fun() ->
            TmpFile = "/tmp/af_wrt_rnd_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".txt",
            WriteCmd = "\"round trip data\" \"" ++ TmpFile ++ "\" write-file",
            C1 = eval(WriteCmd, af_interpreter:new_continuation()),
            C2 = eval("\"" ++ TmpFile ++ "\" read-file", C1),
            [{'String', Content}] = C2#continuation.data_stack,
            ?assertEqual(<<"round trip data">>, Content),
            file:delete(TmpFile)
        end} end
    ]}.

%% --- tokenize ---

tokenize_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"tokenize simple tokens", fun() ->
            C1 = eval("\"1 int 2 int +\" \"test\" tokenize", af_interpreter:new_continuation()),
            [{'List', Tokens}] = C1#continuation.data_stack,
            %% Should have 5 tokens: 1, int, 2, int, +
            ?assertEqual(5, length(Tokens))
        end} end,
        fun(_) -> {"tokenize with quoted string", fun() ->
            %% Use escaped quotes in A4 — actually A4 doesn't escape quotes
            %% Let's tokenize a simpler example
            C1 = eval("\"hello world\" \"test\" tokenize", af_interpreter:new_continuation()),
            [{'List', Tokens}] = C1#continuation.data_stack,
            %% "hello" and "world" are two tokens
            ?assertEqual(2, length(Tokens))
        end} end,
        fun(_) -> {"tokenize returns maps with correct keys", fun() ->
            C1 = eval("\"foo\" \"myfile\" tokenize", af_interpreter:new_continuation()),
            [{'List', [{'Map', TokMap}]}] = C1#continuation.data_stack,
            %% Check that the token map has the expected keys
            ?assertEqual({'String', <<"foo">>}, maps:get({'String', <<"value">>}, TokMap)),
            ?assertEqual({'String', <<"myfile">>}, maps:get({'String', <<"file">>}, TokMap)),
            ?assertEqual({'Int', 1}, maps:get({'String', <<"line">>}, TokMap)),
            ?assertEqual({'Bool', false}, maps:get({'String', <<"quoted">>}, TokMap))
        end} end,
        fun(_) -> {"tokenize empty string returns empty list", fun() ->
            C1 = eval("\"\" \"test\" tokenize", af_interpreter:new_continuation()),
            [{'List', []}] = C1#continuation.data_stack
        end} end
    ]}.

%% --- get-all-words ---

get_all_words_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"get-all-words returns a list", fun() ->
            C1 = eval("get-all-words", af_interpreter:new_continuation()),
            [{'List', Words} | _] = C1#continuation.data_stack,
            ?assert(is_list(Words))
        end} end,
        fun(_) -> {"get-all-words includes user-defined words", fun() ->
            C1 = eval(": my-test-word Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval("get-all-words", C1),
            [{'List', Words} | _] = C2#continuation.data_stack,
            %% Check that our word is in the list
            HasWord = lists:any(fun({'String', Name}) ->
                Name =:= <<"my-test-word">>
            end, Words),
            ?assert(HasWord)
        end} end
    ]}.

%% --- get-word-defs ---

get_word_defs_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"get-word-defs for defined word returns list of maps", fun() ->
            C1 = eval(": my-def-test Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval("\"my-def-test\" get-word-defs", C1),
            [{'List', Defs} | _] = C2#continuation.data_stack,
            ?assert(length(Defs) >= 1),
            %% Each def should be a Map
            [{'Map', DefMap} | _] = Defs,
            ?assert(maps:is_key({'String', <<"name">>}, DefMap)),
            ?assertEqual({'String', <<"my-def-test">>},
                         maps:get({'String', <<"name">>}, DefMap))
        end} end,
        fun(_) -> {"get-word-defs for nonexistent word returns empty list", fun() ->
            C1 = eval("\"no-such-word-xyz\" get-word-defs", af_interpreter:new_continuation()),
            [{'List', []} | _] = C1#continuation.data_stack
        end} end
    ]}.

%% --- print / stack (I/O ops - just verify they don't crash) ---

print_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"print consumes TOS", fun() ->
            C1 = eval("42 int print", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end,
        fun(_) -> {"print preserves rest of stack", fun() ->
            C1 = eval("1 int 2 int print", af_interpreter:new_continuation()),
            [{'Int', 1}] = C1#continuation.data_stack
        end} end
    ]}.

stack_display_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"stack does not modify stack", fun() ->
            C1 = eval("1 int 2 int stack", af_interpreter:new_continuation()),
            [{'Int', 2}, {'Int', 1}] = C1#continuation.data_stack
        end} end,
        fun(_) -> {"stack on empty stack does not crash", fun() ->
            C1 = eval("stack", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end
    ]}.

%% --- words / types (I/O display ops) ---

words_test() ->
    setup(),
    C1 = eval("words", af_interpreter:new_continuation()),
    %% words just prints, stack unchanged
    ?assertEqual([], C1#continuation.data_stack).

types_test() ->
    setup(),
    C1 = eval("types", af_interpreter:new_continuation()),
    ?assertEqual([], C1#continuation.data_stack).
