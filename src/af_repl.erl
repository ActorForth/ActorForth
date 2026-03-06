-module(af_repl).

-include("token.hrl").
-include("continuation.hrl").

-export([start/0, run_file/1, run_file_repl/1, interpret_line/2]).

init_types() ->
    af_type:init(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_list:init(),
    af_type_actor:init().

start() ->
    init_types(),
    io:format("ActorForth REPL. ^C to exit.~n"),
    loop(af_interpreter:new_continuation()).

%% Execute a .a4 file and return the final continuation.
run_file(Filename) ->
    init_types(),
    {ok, Content} = file:read_file(Filename),
    Tokens = af_parser:parse(binary_to_list(Content), Filename),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()).

%% Execute a .a4 file then enter the REPL with resulting state.
run_file_repl(Filename) ->
    init_types(),
    {ok, Content} = file:read_file(Filename),
    Tokens = af_parser:parse(binary_to_list(Content), Filename),
    Cont = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    io:format("ActorForth REPL (loaded ~s). ^C to exit.~n", [Filename]),
    loop(Cont).

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
