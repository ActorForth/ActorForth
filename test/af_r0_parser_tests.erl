-module(af_r0_parser_tests).

-include_lib("eunit/include/eunit.hrl").

%%% === Basic Tokenization ===

simple_tokens_test() ->
    Tokens = af_r0_parser:parse(<<"a b c">>, <<"test">>),
    ?assertEqual(3, length(Tokens)),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Values).

tab_whitespace_test() ->
    Tokens = af_r0_parser:parse(<<"a\tb">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>, <<"b">>], Values).

multiple_spaces_test() ->
    Tokens = af_r0_parser:parse(<<"  a   b  ">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>, <<"b">>], Values).

empty_input_test() ->
    ?assertEqual([], af_r0_parser:parse(<<>>, <<"test">>)).

whitespace_only_test() ->
    ?assertEqual([], af_r0_parser:parse(<<"   \n\t  ">>, <<"test">>)).

%%% === Self-Delimiting Punctuation ===

colon_test() ->
    Tokens = af_r0_parser:parse(<<":foo">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<":">>, <<"foo">>], Values).

semicolon_test() ->
    Tokens = af_r0_parser:parse(<<"a;b">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>, <<";">>, <<"b">>], Values).

dot_standalone_test() ->
    Tokens = af_r0_parser:parse(<<"x .">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"x">>, <<".">>], Values).

dot_attached_test() ->
    Tokens = af_r0_parser:parse(<<"x.">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"x">>, <<".">>], Values).

%%% === Float Literals ===

float_simple_test() ->
    Tokens = af_r0_parser:parse(<<"3.14">>, <<"test">>),
    ?assertEqual(1, length(Tokens)),
    ?assertEqual(<<"3.14">>, maps:get(value, hd(Tokens))).

float_negative_test() ->
    Tokens = af_r0_parser:parse(<<"-2.5">>, <<"test">>),
    ?assertEqual(1, length(Tokens)),
    ?assertEqual(<<"-2.5">>, maps:get(value, hd(Tokens))).

float_in_context_test() ->
    Tokens = af_r0_parser:parse(<<"a 3.14 b">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>, <<"3.14">>, <<"b">>], Values).

dot_not_float_test() ->
    %% "abc.def" — not digits, so dot is self-delimiting
    Tokens = af_r0_parser:parse(<<"abc.def">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"abc">>, <<".">>, <<"def">>], Values).

%%% === Comments ===

comment_eol_test() ->
    Tokens = af_r0_parser:parse(<<"a # comment\nb">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>, <<"b">>], Values).

comment_eof_test() ->
    Tokens = af_r0_parser:parse(<<"a # comment">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>], Values).

comment_empty_test() ->
    Tokens = af_r0_parser:parse(<<"#">>, <<"test">>),
    ?assertEqual([], Tokens).

%%% === Quoted Strings ===

quoted_string_test() ->
    Tokens = af_r0_parser:parse(<<"\"hello\"">>, <<"test">>),
    ?assertEqual(1, length(Tokens)),
    T = hd(Tokens),
    ?assertEqual(<<"hello">>, maps:get(value, T)),
    ?assertEqual(true, maps:get(quoted, T)).

quoted_with_spaces_test() ->
    Tokens = af_r0_parser:parse(<<"\"hello world\"">>, <<"test">>),
    ?assertEqual(<<"hello world">>, maps:get(value, hd(Tokens))).

quoted_empty_test() ->
    Tokens = af_r0_parser:parse(<<"\"\"">>, <<"test">>),
    ?assertEqual(<<>>, maps:get(value, hd(Tokens))).

quoted_with_newline_test() ->
    Tokens = af_r0_parser:parse(<<"\"a\nb\"">>, <<"test">>),
    ?assertEqual(<<"a\nb">>, maps:get(value, hd(Tokens))).

non_quoted_test() ->
    Tokens = af_r0_parser:parse(<<"hello">>, <<"test">>),
    ?assertEqual(false, maps:get(quoted, hd(Tokens))).

%%% === Line/Column Tracking ===

line_tracking_test() ->
    Tokens = af_r0_parser:parse(<<"a\nb\nc">>, <<"test">>),
    Lines = [maps:get(line, T) || T <- Tokens],
    ?assertEqual([1, 2, 3], Lines).

col_tracking_test() ->
    Tokens = af_r0_parser:parse(<<"ab cd ef">>, <<"test">>),
    Cols = [maps:get(col, T) || T <- Tokens],
    ?assertEqual([1, 4, 7], Cols).

file_tracking_test() ->
    Tokens = af_r0_parser:parse(<<"a">>, <<"myfile.a4">>),
    ?assertEqual(<<"myfile.a4">>, maps:get(file, hd(Tokens))).

%%% === Word Definition ===

word_def_tokens_test() ->
    Tokens = af_r0_parser:parse(<<": double Int -> Int ; dup + .">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<":">>, <<"double">>, <<"Int">>, <<"->">>,
                  <<"Int">>, <<";">>, <<"dup">>, <<"+">>, <<".">>], Values).

multi_word_def_test() ->
    Src = <<": inc Int -> Int ; 1 + .\n: dec Int -> Int ; 1 - .">>,
    Tokens = af_r0_parser:parse(Src, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<":">>, <<"inc">>, <<"Int">>, <<"->">>, <<"Int">>,
                  <<";">>, <<"1">>, <<"+">>, <<".">>,
                  <<":">>, <<"dec">>, <<"Int">>, <<"->">>, <<"Int">>,
                  <<";">>, <<"1">>, <<"-">>, <<".">>], Values).

%%% === Unterminated Strings ===

unterminated_string_test() ->
    %% String without closing quote
    Tokens = af_r0_parser:parse(<<"\"hello">>, <<"test">>),
    ?assertEqual(1, length(Tokens)),
    T = hd(Tokens),
    ?assertEqual(<<"hello">>, maps:get(value, T)),
    ?assertEqual(true, maps:get(quoted, T)).

%%% === Carriage Return Whitespace ===

carriage_return_ws_test() ->
    Tokens = af_r0_parser:parse(<<"a\r\nb">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>, <<"b">>], Values).

carriage_return_only_test() ->
    Tokens = af_r0_parser:parse(<<"a\rb">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"a">>, <<"b">>], Values).

%%% === Float Dot Edge Cases ===

dot_after_quoted_string_not_float_test() ->
    %% A quoted string followed by .digit should NOT be treated as float
    Tokens = af_r0_parser:parse(<<"\"123\".4">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    %% quoted "123" then "." then "4"
    ?assertEqual([<<"123">>, <<".">>, <<"4">>], Values).

dot_non_adjacent_not_float_test() ->
    %% "123 .4" — space between digits and dot means not a float
    Tokens = af_r0_parser:parse(<<"123 .4">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"123">>, <<".">>, <<"4">>], Values).

dot_at_start_not_float_test() ->
    %% ".5" at the very start (no previous token)
    Tokens = af_r0_parser:parse(<<".5">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<".">>, <<"5">>], Values).

dot_after_empty_token_test() ->
    %% Ensure dot after non-digit token doesn't merge
    Tokens = af_r0_parser:parse(<<"abc.123">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"abc">>, <<".">>, <<"123">>], Values).

dot_at_end_of_input_test() ->
    %% "123." where dot has no following char
    Tokens = af_r0_parser:parse(<<"123.">>, <<"test">>),
    Values = [maps:get(value, T) || T <- Tokens],
    ?assertEqual([<<"123">>, <<".">>], Values).
