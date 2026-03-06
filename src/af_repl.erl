-module(af_repl).

-include("token.hrl").
-include("continuation.hrl").

-export([start/0, interpret_line/2]).

start() ->
    af_type:init(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_actor:init(),
    io:format("ActorForth REPL. ^C to exit.~n"),
    loop(af_interpreter:new_continuation()).

loop(Cont) ->
    case io:get_line("ok: ") of
        eof ->
            io:format("~nbye.~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]);
        Line ->
            case catch interpret_line(Line, Cont) of
                {'EXIT', Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    loop(Cont);
                NewCont ->
                    loop(NewCont)
            end
    end.

interpret_line(Line, Cont) ->
    Tokens = af_parser:parse(Line, "stdin"),
    af_interpreter:interpret_tokens(Tokens, Cont).
