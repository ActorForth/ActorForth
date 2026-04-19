-module(af_parser).

-include("token.hrl").

-export([parse/2]).

-spec parse(string(), string()) -> [#token{}].
parse(Input, Filename) ->
    lists:reverse(tokenize(Input, Filename, 1, 1, [], [])).

%%% Internal

%% tokenize(Remaining, File, Line, Col, CurrentToken, Tokens)
%% Accumulates characters into tokens, emits on delimiters.

%% End of input
tokenize([], File, Line, Col, Current, Tokens) ->
    emit(Current, File, Line, Col, Tokens);

%% Newline
tokenize([$\n | Rest], File, Line, Col, Current, Tokens) ->
    Tokens1 = emit(Current, File, Line, Col, Tokens),
    tokenize(Rest, File, Line + 1, 1, [], Tokens1);

%% Space
tokenize([$\s | Rest], File, Line, Col, Current, Tokens) ->
    Tokens1 = emit(Current, File, Line, Col, Tokens),
    tokenize(Rest, File, Line, Col + 1, [], Tokens1);

%% Tab
tokenize([$\t | Rest], File, Line, Col, Current, Tokens) ->
    Tokens1 = emit(Current, File, Line, Col, Tokens),
    tokenize(Rest, File, Line, Col + 4, [], Tokens1);

%% Comment: # to end of line
tokenize([$# | Rest], File, Line, Col, Current, Tokens) ->
    Tokens1 = emit(Current, File, Line, Col, Tokens),
    Rest1 = skip_comment(Rest),
    tokenize(Rest1, File, Line + 1, 1, [], Tokens1);

%% Quoted string
tokenize([$" | Rest], File, Line, Col, Current, Tokens) ->
    Tokens1 = emit(Current, File, Line, Col, Tokens),
    {StringVal, Rest1, Line1, Col1} = read_string(Rest, File, Line, Col + 1, []),
    Token = #token{value = StringVal, line = Line, column = Col, file = File, quoted = true},
    tokenize(Rest1, File, Line1, Col1, [], [Token | Tokens1]);

%% Self-delimiting punctuation: : ;
tokenize([C | Rest], File, Line, Col, Current, Tokens)
  when C =:= $:; C =:= $; ->
    Tokens1 = emit(Current, File, Line, Col, Tokens),
    PuncToken = #token{value = [C], line = Line, column = Col, file = File},
    tokenize(Rest, File, Line, Col + 1, [], [PuncToken | Tokens1]);

%% Dot handling. Three cases, in order:
%%   1. Part of a float literal (digits.digits) — accumulate with Current.
%%   2. Start of a positional-field-access token (empty Current + next char
%%      is another dot or an identifier character) — accumulate the dot as
%%      the start of a new token. Examples:  .x  ..x  .field  ...tag  .s
%%   3. Standalone self-delimiting punctuation — emit Current, emit "."
%%      as its own token.
tokenize([$. | Rest], File, Line, Col, Current, Tokens) ->
    case is_float_dot(Current, Rest) of
        true ->
            tokenize(Rest, File, Line, Col + 1, Current ++ [{$., Line, Col}], Tokens);
        false ->
            case starts_dot_ident(Current, Rest) of
                true ->
                    tokenize(Rest, File, Line, Col + 1,
                             Current ++ [{$., Line, Col}], Tokens);
                false ->
                    Tokens1 = emit(Current, File, Line, Col, Tokens),
                    PuncToken = #token{value = ".", line = Line, column = Col, file = File},
                    tokenize(Rest, File, Line, Col + 1, [], [PuncToken | Tokens1])
            end
    end;

%% Regular character
tokenize([C | Rest], File, Line, Col, [], Tokens) ->
    tokenize(Rest, File, Line, Col + 1, [{C, Line, Col}], Tokens);
tokenize([C | Rest], File, Line, Col, Current, Tokens) ->
    tokenize(Rest, File, Line, Col + 1, Current ++ [{C, Line, Col}], Tokens).

%% Emit accumulated characters as a token (if any)
emit([], _File, _Line, _Col, Tokens) ->
    Tokens;
emit(CharList, File, _Line, _Col, Tokens) ->
    {_, StartLine, StartCol} = hd(CharList),
    Value = [C || {C, _, _} <- CharList],
    Token = #token{value = Value, line = StartLine, column = StartCol, file = File},
    [Token | Tokens].

%% Check if a dot is part of a float literal: digits before dot AND digit after.
is_float_dot([], _) -> false;
is_float_dot(Current, [C | _]) when C >= $0, C =< $9 ->
    %% Check that all chars before the dot are digits (optionally with leading -)
    Chars = [Ch || {Ch, _, _} <- Current],
    all_digits(Chars);
is_float_dot(_, _) -> false.

%% Dot-prefixed identifier: the buffer is either empty or consists only
%% of accumulated leading dots, AND the next char is either another dot
%% or an identifier character. Used for positional field access —
%% `.x` (pos 1), `..x` (pos 2), `...x` (pos 3), etc. — and fixes the
%% long-standing `.s` dispatch bug (previously tokenized as two tokens).
starts_dot_ident([], [$. | _]) -> true;
starts_dot_ident([], [C | _]) -> is_ident_char(C);
starts_dot_ident(Current, Rest) ->
    case all_leading_dots(Current) of
        true ->
            case Rest of
                [$. | _] -> true;
                [C | _] -> is_ident_char(C);
                [] -> false
            end;
        false -> false
    end.

all_leading_dots([]) -> true;
all_leading_dots([{$., _, _} | Rest]) -> all_leading_dots(Rest);
all_leading_dots(_) -> false.

is_ident_char(C) when C >= $a, C =< $z -> true;
is_ident_char(C) when C >= $A, C =< $Z -> true;
is_ident_char(C) when C >= $0, C =< $9 -> true;
is_ident_char($_) -> true;
is_ident_char($-) -> true;
is_ident_char($!) -> true;
is_ident_char($?) -> true;
is_ident_char(_) -> false.

all_digits([]) -> false;
all_digits([$- | Rest]) -> all_digits_strict(Rest);
all_digits(Chars) -> all_digits_strict(Chars).

all_digits_strict([]) -> false;
all_digits_strict(Chars) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Chars).

skip_comment([]) -> [];
skip_comment([$\n | Rest]) -> Rest;
skip_comment([_ | Rest]) -> skip_comment(Rest).

read_string([], _File, Line, Col, Acc) ->
    %% Unterminated string — return what we have
    {lists:reverse(Acc), [], Line, Col};
read_string([$" | Rest], _File, Line, Col, Acc) ->
    {lists:reverse(Acc), Rest, Line, Col + 1};
read_string([$\n | Rest], File, Line, _Col, Acc) ->
    read_string(Rest, File, Line + 1, 1, [$\n | Acc]);
read_string([C | Rest], File, Line, Col, Acc) ->
    read_string(Rest, File, Line, Col + 1, [C | Acc]).
