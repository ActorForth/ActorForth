-module(af_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").

%% Helpers
values(Tokens) -> [T#token.value || T <- Tokens].

%% --- basic tokenization ---

basic_test_() ->
    [
        {"empty input", fun() ->
            ?assertEqual([], af_parser:parse("", "test"))
        end},
        {"single word", fun() ->
            Tokens = af_parser:parse("hello", "test"),
            ?assertEqual(["hello"], values(Tokens))
        end},
        {"multiple words", fun() ->
            Tokens = af_parser:parse("hello world", "test"),
            ?assertEqual(["hello", "world"], values(Tokens))
        end},
        {"extra whitespace", fun() ->
            Tokens = af_parser:parse("  hello   world  ", "test"),
            ?assertEqual(["hello", "world"], values(Tokens))
        end},
        {"tabs as delimiters", fun() ->
            Tokens = af_parser:parse("hello\tworld", "test"),
            ?assertEqual(["hello", "world"], values(Tokens))
        end},
        {"newlines as delimiters", fun() ->
            Tokens = af_parser:parse("hello\nworld", "test"),
            ?assertEqual(["hello", "world"], values(Tokens))
        end}
    ].

%% --- punctuation ---

punctuation_test_() ->
    [
        {"period is self-delimiting", fun() ->
            Tokens = af_parser:parse("word.", "test"),
            ?assertEqual(["word", "."], values(Tokens))
        end},
        {"colon is self-delimiting", fun() ->
            Tokens = af_parser:parse(": myword", "test"),
            ?assertEqual([":", "myword"], values(Tokens))
        end},
        {"semicolon is self-delimiting", fun() ->
            Tokens = af_parser:parse("a;b", "test"),
            ?assertEqual(["a", ";", "b"], values(Tokens))
        end},
        {"mixed punctuation", fun() ->
            Tokens = af_parser:parse(": double Int -> Int ; dup + .", "test"),
            ?assertEqual([":", "double", "Int", "->", "Int", ";", "dup", "+", "."],
                         values(Tokens))
        end}
    ].

%% --- comments ---

comment_test_() ->
    [
        {"comment discards to end of line", fun() ->
            Tokens = af_parser:parse("hello # this is a comment\nworld", "test"),
            ?assertEqual(["hello", "world"], values(Tokens))
        end},
        {"comment at end of input", fun() ->
            Tokens = af_parser:parse("hello # trailing", "test"),
            ?assertEqual(["hello"], values(Tokens))
        end}
    ].

%% --- quoted strings ---

string_test_() ->
    [
        {"quoted string", fun() ->
            Tokens = af_parser:parse("\"hello world\"", "test"),
            ?assertEqual(["hello world"], values(Tokens))
        end},
        {"string among words", fun() ->
            Tokens = af_parser:parse("say \"hello world\" done", "test"),
            ?assertEqual(["say", "hello world", "done"], values(Tokens))
        end},
        {"escape: \\n is newline", fun() ->
            [T] = af_parser:parse("\"a\\nb\"", "test"),
            ?assertEqual([$a, $\n, $b], T#token.value)
        end},
        {"escape: \\t is tab", fun() ->
            [T] = af_parser:parse("\"a\\tb\"", "test"),
            ?assertEqual([$a, $\t, $b], T#token.value)
        end},
        {"escape: \\r is CR", fun() ->
            [T] = af_parser:parse("\"a\\rb\"", "test"),
            ?assertEqual([$a, $\r, $b], T#token.value)
        end},
        {"escape: \\e is ESC", fun() ->
            [T] = af_parser:parse("\"\\e[2J\"", "test"),
            ?assertEqual([27, $[, $2, $J], T#token.value)
        end},
        {"escape: \\\\ is backslash", fun() ->
            [T] = af_parser:parse("\"a\\\\b\"", "test"),
            ?assertEqual([$a, $\\, $b], T#token.value)
        end},
        {"escape: \\\" is quote", fun() ->
            [T] = af_parser:parse("\"a\\\"b\"", "test"),
            ?assertEqual([$a, $", $b], T#token.value)
        end},
        {"escape: \\0 is NUL", fun() ->
            [T] = af_parser:parse("\"a\\0b\"", "test"),
            ?assertEqual([$a, 0, $b], T#token.value)
        end},
        {"escape: \\xNN is hex byte", fun() ->
            [T] = af_parser:parse("\"\\x1bX\\xff\"", "test"),
            ?assertEqual([16#1b, $X, 16#ff], T#token.value)
        end},
        {"escape: unknown escape passes char through", fun() ->
            [T] = af_parser:parse("\"a\\zb\"", "test"),
            ?assertEqual([$a, $z, $b], T#token.value)
        end}
    ].

%% --- source positions ---

position_test_() ->
    [
        {"first token starts at line 1 col 1", fun() ->
            [T] = af_parser:parse("hello", "myfile"),
            ?assertEqual(1, T#token.line),
            ?assertEqual(1, T#token.column),
            ?assertEqual("myfile", T#token.file)
        end},
        {"second token has correct column", fun() ->
            [T1, T2] = af_parser:parse("hi there", "f"),
            ?assertEqual(1, T1#token.column),
            ?assertEqual(4, T2#token.column)
        end},
        {"newline advances line number", fun() ->
            [T1, T2] = af_parser:parse("hi\nthere", "f"),
            ?assertEqual(1, T1#token.line),
            ?assertEqual(2, T2#token.line),
            ?assertEqual(1, T2#token.column)
        end},
        {"punctuation has correct position", fun() ->
            Tokens = af_parser:parse("a.b", "f"),
            ?assertEqual(["a", ".", "b"], values(Tokens)),
            [Ta, Tdot, Tb] = Tokens,
            ?assertEqual(1, Ta#token.column),
            ?assertEqual(2, Tdot#token.column),
            ?assertEqual(3, Tb#token.column)
        end}
    ].

%% --- unterminated and multiline strings ---

string_edge_test_() ->
    [
        {"unterminated string returns what we have", fun() ->
            Tokens = af_parser:parse("\"unterminated", "test"),
            ?assertEqual(["unterminated"], values(Tokens))
        end},
        {"string with newline inside", fun() ->
            Tokens = af_parser:parse("\"line1\nline2\"", "test"),
            ?assertEqual(["line1\nline2"], values(Tokens))
        end}
    ].

%% --- ActorForth expressions ---

expression_test_() ->
    [
        {"forth arithmetic", fun() ->
            Tokens = af_parser:parse("4 int 13 int + .", "test"),
            ?assertEqual(["4", "int", "13", "int", "+", "."], values(Tokens))
        end},
        {"word definition", fun() ->
            Tokens = af_parser:parse(": double Int -> Int ; dup + .", "test"),
            ?assertEqual([":", "double", "Int", "->", "Int", ";", "dup", "+", "."],
                         values(Tokens))
        end}
    ].
