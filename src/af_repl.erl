-module(af_repl).

-include("token.hrl").
-include("continuation.hrl").
-include("af_error.hrl").

-export([start/0, run_file/1, run_file_repl/1, interpret_line/2, init_types/0]).
-export([load_env/0, load_env/1]).

init_types() ->
    af_type:init(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_string:init(),
    af_type_map:init(),
    af_type_list:init(),
    af_type_actor:init(),
    af_type_ffi:init(),
    af_type_float:init(),
    af_type_tuple:init(),
    af_type_beam:init(),
    af_type_otp:init(),
    af_type_python:init().

start() ->
    load_env(),
    init_types(),
    Cont = load_stdlib(af_interpreter:new_continuation()),
    io:format("ActorForth REPL. ^C to exit.~n"),
    loop(Cont).

%% Execute a .a4 file and return the final continuation.
run_file(Filename) ->
    init_types(),
    Cont0 = load_stdlib(af_interpreter:new_continuation()),
    {ok, Content} = file:read_file(Filename),
    Tokens = af_parser:parse(binary_to_list(Content), Filename),
    af_interpreter:interpret_tokens(Tokens, Cont0).

%% Execute a .a4 file then enter the REPL with resulting state.
run_file_repl(Filename) ->
    init_types(),
    Cont0 = load_stdlib(af_interpreter:new_continuation()),
    {ok, Content} = file:read_file(Filename),
    Tokens = af_parser:parse(binary_to_list(Content), Filename),
    Cont = af_interpreter:interpret_tokens(Tokens, Cont0),
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
                {'EXIT', {#af_error{} = Err, _Stacktrace}} ->
                    io:format("~s~n", [af_error:format(Err)]),
                    loop(Cont);
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

%% Load the stdlib.a4 standard library if it exists.
load_stdlib(Cont) ->
    PrivDir = case code:priv_dir(actorforth) of
        {error, _} -> "priv";
        Dir -> Dir
    end,
    StdlibPath = filename:join(PrivDir, "stdlib.a4"),
    case file:read_file(StdlibPath) of
        {ok, Content} ->
            Tokens = af_parser:parse(binary_to_list(Content), StdlibPath),
            af_interpreter:interpret_tokens(Tokens, Cont);
        {error, _} ->
            Cont
    end.

%% Load environment variables from a .env file.
%% Looks for .env in the current directory by default.
load_env() ->
    load_env(".env").

load_env(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            Lines = binary:split(Content, [<<"\n">>, <<"\r\n">>], [global]),
            lists:foreach(fun(Line) ->
                case parse_env_line(Line) of
                    {Key, Value} ->
                        os:putenv(binary_to_list(Key), binary_to_list(Value));
                    skip ->
                        ok
                end
            end, Lines);
        {error, enoent} ->
            ok;
        {error, _Reason} ->
            ok
    end.

parse_env_line(Line) ->
    Trimmed = string:trim(binary_to_list(Line)),
    case Trimmed of
        [] -> skip;
        [$# | _] -> skip;
        _ ->
            case string:split(Trimmed, "=", leading) of
                [Key, Value] ->
                    K = string:trim(Key),
                    V = unquote(string:trim(Value)),
                    {list_to_binary(K), list_to_binary(V)};
                _ -> skip
            end
    end.

unquote([$" | Rest]) ->
    case lists:reverse(Rest) of
        [$" | Inner] -> lists:reverse(Inner);
        _ -> [$" | Rest]
    end;
unquote([$' | Rest]) ->
    case lists:reverse(Rest) of
        [$' | Inner] -> lists:reverse(Inner);
        _ -> [$' | Rest]
    end;
unquote(V) -> V.
