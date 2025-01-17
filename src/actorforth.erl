-module(actorforth).

-export([interpret/1]).

-spec interpret([string()]) -> ok.
interpret(Tokens) ->
    lists:foreach( fun interpret_token/1, Tokens),
    io:format("ok.~n"),
    ok.


-spec interpret_token(string()) -> ok.
interpret_token(Token) ->
    case string:to_integer(Token) of
        {Int, []} -> % Conversion successful, no remaining characters
            io:format("int(~p) ", [Int]);
        _ -> % Conversion failed or partial
            io:format("~s ", [Token])
    end,
    ok.