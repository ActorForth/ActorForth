#!/usr/bin/env escript
%%! -pa _build/default/lib/actorforth/ebin
-include_lib("actorforth/include/token.hrl").
-include_lib("actorforth/include/continuation.hrl").

main([Pattern, Size]) ->
    [WStr, HStr] = string:split(Size, "x"),
    W = list_to_integer(WStr),
    H = list_to_integer(HStr),
    af_repl:init_types(),
    af_compile_file:compile("samples/gol/terminal.a4"),
    af_compile_file:compile("samples/gol/patterns.a4"),
    af_compile_file:compile("samples/gol/cell.a4"),
    af_compile_file:compile("samples/gol/game.a4"),
    af_compile_file:compile("samples/gol/standalone.a4"),
    Code = io_lib:format("~B ~B ~s game-loop", [W, H, Pattern]),
    Tokens = af_parser:parse(lists:flatten(Code), "args"),
    af_interpreter:interpret_tokens(Tokens, #continuation{}),
    halt(0);
main(_) ->
    io:format(standard_error,
        "Usage: standalone.escript <pattern> <WxH>~n"
        "  patterns: glider blinker toad beacon pulsar lwss r-pentomino acorn gun~n",
        []),
    halt(1).
