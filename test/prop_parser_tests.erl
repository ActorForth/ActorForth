-module(prop_parser_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").

%% ============================================================
%% Generators
%% ============================================================

%% Generate a simple word (alphanumeric, no whitespace or special chars)
word() ->
    ?LET(Chars, non_empty(list(oneof([
        range($a, $z),
        range($A, $Z),
        range($0, $9),
        $_, $-
    ]))),
    Chars).

%% Generate a list of simple words
word_list() ->
    non_empty(list(word())).

%% Generate a quoted string (no internal quotes or newlines for simplicity)
string_content() ->
    list(oneof([
        range($a, $z),
        range($A, $Z),
        range($0, $9),
        $\s, $_, $-, $!
    ])).

%% Generate a comment line (# followed by non-newline chars)
comment_text() ->
    list(oneof([range($a, $z), range($A, $Z), $\s, range($0, $9)])).

%% ============================================================
%% Properties
%% ============================================================

%% Property 1: Roundtrip - parsing words produces tokens whose values
%% reconstruct the original (modulo whitespace)
roundtrip_test() ->
    Prop = ?FORALL(Words, word_list(),
        begin
            Input = string:join(Words, " "),
            Tokens = af_parser:parse(Input, "test"),
            Values = [T#token.value || T <- Tokens],
            Values =:= Words
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Property 2: Token count - N space-separated simple words produce N tokens
%% (words here contain no self-delimiting characters)
token_count_test() ->
    Prop = ?FORALL(Words, word_list(),
        begin
            Input = string:join(Words, " "),
            Tokens = af_parser:parse(Input, "test"),
            length(Tokens) =:= length(Words)
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Property 3: Line tracking - tokens on the same line have the same line number
line_tracking_test() ->
    Prop = ?FORALL(Words, word_list(),
        begin
            %% All words on a single line
            Input = string:join(Words, " "),
            Tokens = af_parser:parse(Input, "test"),
            lists:all(fun(T) -> T#token.line =:= 1 end, Tokens)
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Property 3b: Multi-line - tokens on different lines get different line numbers
multiline_tracking_test() ->
    Prop = ?FORALL({W1, W2}, {word(), word()},
        begin
            Input = W1 ++ "\n" ++ W2,
            Tokens = af_parser:parse(Input, "test"),
            case Tokens of
                [T1, T2] ->
                    T1#token.line =:= 1 andalso T2#token.line =:= 2;
                _ -> false
            end
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Property 4: String literal - quoted strings are preserved as single tokens
string_literal_test() ->
    Prop = ?FORALL(Content, string_content(),
        begin
            Input = "\"" ++ Content ++ "\"",
            Tokens = af_parser:parse(Input, "test"),
            case Tokens of
                [T] ->
                    T#token.value =:= Content andalso T#token.quoted =:= true;
                _ -> false
            end
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Property 5: Comment stripping - # comments produce no tokens for that content
comment_stripping_test() ->
    Prop = ?FORALL(Comment, comment_text(),
        begin
            Input = "# " ++ Comment,
            Tokens = af_parser:parse(Input, "test"),
            Tokens =:= []
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Property 5b: Comment after content - only content before # produces tokens
comment_after_content_test() ->
    Prop = ?FORALL({W, Comment}, {word(), comment_text()},
        begin
            Input = W ++ " # " ++ Comment,
            Tokens = af_parser:parse(Input, "test"),
            case Tokens of
                [T] -> T#token.value =:= W;
                _ -> false
            end
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Property 6: Self-delimiting - . : ; always produce separate tokens
%% even without surrounding whitespace
%% For dot self-delimiting, we must avoid the case where W1 is all digits
%% and W2 starts with a digit, which triggers float literal detection.
alpha_word() ->
    ?LET(Chars, non_empty(list(oneof([
        range($a, $z),
        range($A, $Z),
        $_, $-
    ]))),
    Chars).

self_delimiting_dot_test() ->
    Prop = ?FORALL({W1, W2}, {alpha_word(), alpha_word()},
        begin
            Input = W1 ++ "." ++ W2,
            Tokens = af_parser:parse(Input, "test"),
            Values = [T#token.value || T <- Tokens],
            Values =:= [W1, ".", W2]
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

self_delimiting_colon_test() ->
    Prop = ?FORALL({W1, W2}, {word(), word()},
        begin
            Input = W1 ++ ":" ++ W2,
            Tokens = af_parser:parse(Input, "test"),
            Values = [T#token.value || T <- Tokens],
            Values =:= [W1, ":", W2]
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

self_delimiting_semicolon_test() ->
    Prop = ?FORALL({W1, W2}, {word(), word()},
        begin
            Input = W1 ++ ";" ++ W2,
            Tokens = af_parser:parse(Input, "test"),
            Values = [T#token.value || T <- Tokens],
            Values =:= [W1, ";", W2]
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).
