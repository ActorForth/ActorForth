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
    Token = #token{value = StringVal, line = Line, column = Col, file = File},
    tokenize(Rest1, File, Line1, Col1, [], [Token | Tokens1]);

%% Self-delimiting punctuation: . : ;
tokenize([C | Rest], File, Line, Col, Current, Tokens)
  when C =:= $.; C =:= $:; C =:= $; ->
    Tokens1 = emit(Current, File, Line, Col, Tokens),
    PuncToken = #token{value = [C], line = Line, column = Col, file = File},
    tokenize(Rest, File, Line, Col + 1, [], [PuncToken | Tokens1]);

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
