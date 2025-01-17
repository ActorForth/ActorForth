-module(actorforth).

-export([interpret/1, make_token/1, make_token/2, make_token/3, make_token/4]).

-record(token, {
    value :: string(),
    column :: non_neg_integer(),
    line :: non_neg_integer(),
    file :: string()
}).

-spec interpret([#token{}]) -> ok.
interpret(Tokens) ->
    lists:foreach(fun interpret_token/1, Tokens),
    io:format("ok.~n"),
    ok.

-spec interpret_token(#token{}) -> ok.
interpret_token(#token{value = Token, line = Line, column = Column, file = File}) ->
    case string:to_integer(Token) of
        {Int, []} -> % Conversion successful, no remaining characters
            io:format("int(~p) at ~s, line ~p, column ~p ", [Int, File, Line, Column]);
        _ -> % Conversion failed or partial
            io:format("'~s' at ~s, line ~p, column ~p ", [Token, File, Line, Column])
    end,
    ok.

-spec make_token(string()) -> #token{}.
make_token(Value) ->
    make_token(Value, 0, 0, "(unknown)").

-spec make_token(string(), non_neg_integer()) -> #token{}.
make_token(Value, Line) ->
    make_token(Value, Line, 0, "(unknown)").

-spec make_token(string(), non_neg_integer(), non_neg_integer()) -> #token{}.
make_token(Value, Line, Column) ->
    make_token(Value, Line, Column, "(unknown)").

-spec make_token(string(), non_neg_integer(), non_neg_integer(), string()) -> #token{}.
make_token(Value, Line, Column, File) ->
    #token{
        value = Value,
        column = Column,
        line = Line,
        file = File
    }.