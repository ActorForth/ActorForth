-module(repl).

-include("token.hrl"). 
-include("continuation.hrl").

-export([interpret_tokens/1, interpret_tokens/2, make_token/1, make_token/2, make_token/3, make_token/4]).

-export_type([token/0]).

-type token() :: #token{
    value :: string(),
    column :: non_neg_integer(),
    line :: non_neg_integer(),
    file :: string()
}.

-spec interpret_tokens([#token{}]) -> thread:continuation().
interpret_tokens(Tokens) ->
    Result = interpret_tokens(Tokens, thread:make_continuation()),
    io:format("ok.~n"),
    Result.

-spec interpret_tokens([#token{}], thread:continuation()) -> thread:continuation().
interpret_tokens([], Cont) ->
    Cont;
interpret_tokens([Token | Tokens], Cont) ->
    NewCont = interpret_token(Token, Cont),
    interpret_tokens(Tokens, NewCont).


-spec interpret_token(#token{}, thread:continuation()) -> thread:continuation().
interpret_token(#token{value = Value}, Cont) ->
    %% Create a new stack item 
    StackItem = {atom, Value},
    
    %% Directly update the continuation record
    Cont#continuation{data_stack = [StackItem | Cont#continuation.data_stack]}.     


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