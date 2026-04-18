-module(af_cli).

-export([main/1, start/0, parse_args/1]).

%% Called from release boot script via -s af_cli start
start() ->
    Args = init:get_plain_arguments(),
    try
        main(Args)
    catch
        Class:Reason:Stack ->
            io:format(standard_error, "Fatal: ~p:~p~n~p~n", [Class, Reason, Stack]),
            halt(1)
    end.

%% CLI entry point
main([]) ->
    af_repl:start();
main(["--help"]) ->
    usage(),
    halt(0);
main(["--version"]) ->
    io:format("a4c 0.1.0~n"),
    halt(0);
main(["repl" | _]) ->
    af_repl:start();
main(["compile" | Rest]) ->
    compile_cmd(Rest);
main(["build" | Rest]) ->
    build_cmd(Rest);
main(["run" | Rest]) ->
    run_cmd(Rest);
main(["test" | Rest]) ->
    test_cmd(Rest);
main([File | _]) ->
    %% Default: compile if it looks like a file
    case filename:extension(File) of
        ".a4" -> compile_cmd([File]);
        _ ->
            io:format(standard_error, "Unknown command: ~s~n", [File]),
            usage(),
            halt(1)
    end.

%% --- Commands ---

compile_cmd([]) ->
    io:format(standard_error, "Usage: a4c compile <file.a4> [-o <dir>] [--selfhosted]~n", []),
    halt(1);
compile_cmd(Args) ->
    {Files, Opts} = parse_args(Args),
    OutDir = proplists:get_value(o, Opts, "."),
    Selfhosted = proplists:get_bool(selfhosted, Opts),
    init_runtime(Selfhosted),
    lists:foreach(fun(File) ->
        case compile_one(File, OutDir, Selfhosted) of
            {ok, Mod} ->
                io:format("~s -> ~s.beam~n", [File, Mod]);
            {error, Reason} ->
                io:format(standard_error, "Error compiling ~s: ~p~n", [File, Reason]),
                halt(1)
        end
    end, Files),
    halt(0).

build_cmd([]) ->
    io:format(standard_error, "Usage: a4c build <file.a4> [-o <output>] [--selfhosted]~n", []),
    halt(1);
build_cmd(Args) ->
    {Files, Opts} = parse_args(Args),
    Output = proplists:get_value(o, Opts, undefined),
    Selfhosted = proplists:get_bool(selfhosted, Opts),
    init_runtime(Selfhosted),
    case Files of
        [File] ->
            BaseName = filename:basename(File, ".a4"),
            EscriptOut = case Output of
                undefined -> BaseName;
                O -> O
            end,
            case build_escript(File, EscriptOut, Selfhosted) of
                ok ->
                    io:format("~s -> ~s~n", [File, EscriptOut]),
                    halt(0);
                {error, Reason} ->
                    io:format(standard_error, "Error building ~s: ~p~n", [File, Reason]),
                    halt(1)
            end;
        _ ->
            io:format(standard_error, "build takes exactly one file~n", []),
            halt(1)
    end.

run_cmd([]) ->
    io:format(standard_error, "Usage: a4c run <file.a4> [--selfhosted]~n", []),
    halt(1);
run_cmd(Args) ->
    {Files, Opts} = parse_args(Args),
    Selfhosted = proplists:get_bool(selfhosted, Opts),
    init_runtime(Selfhosted),
    case Files of
        [File] ->
            case Selfhosted of
                true ->
                    case af_compile_file:compile_selfhosted(File) of
                        {ok, _} -> halt(0);
                        {error, Reason} ->
                            io:format(standard_error, "Error: ~p~n", [Reason]),
                            halt(1)
                    end;
                false ->
                    try
                        af_repl:run_file(File),
                        halt(0)
                    catch
                        _:Reason ->
                            io:format(standard_error, "Error: ~p~n", [Reason]),
                            halt(1)
                    end
            end;
        _ ->
            io:format(standard_error, "run takes exactly one file~n", []),
            halt(1)
    end.

%% --- test ---

test_cmd([]) ->
    test_cmd(["lib/testing", "test"]);
test_cmd(Args) ->
    {Paths, Opts} = parse_args(Args),
    init_runtime(false),
    Paths1 = case Paths of [] -> ["lib/testing", "test"]; _ -> Paths end,
    {Passed, Failed, Errors} = af_test_runner:run(Paths1, Opts),
    case Failed + Errors of
        0 -> halt(0);
        _ ->
            io:format(standard_error, "~p failed, ~p errors~n", [Failed, Errors]),
            _ = Passed,
            halt(1)
    end.

%% --- Helpers ---

init_runtime(true) ->
    ok;
init_runtime(false) ->
    af_repl:init_types().

compile_one(File, OutDir, true) ->
    af_compile_file:compile_to_dir_selfhosted(File, OutDir);
compile_one(File, OutDir, false) ->
    af_compile_file:compile_to_dir(File, OutDir).

build_escript(File, Output, Selfhosted) ->
    BaseName = filename:basename(File, ".a4"),
    ModAtom = list_to_atom(BaseName),
    case Selfhosted of
        true ->
            case af_compile_file:compile_selfhosted(File) of
                {ok, _} -> make_escript(ModAtom, main, Output);
                {error, _} = Err -> Err
            end;
        false ->
            case af_compile_file:compile(File) of
                {ok, _} -> make_escript(ModAtom, main, Output);
                {error, _} = Err -> Err
            end
    end.

make_escript(Mod, _Fun, Output) ->
    %% Get the compiled binary
    case code:get_object_code(Mod) of
        {Mod, Binary, _Filename} ->
            Sections = [
                {shebang, default},
                {archive, [{atom_to_list(Mod) ++ ".beam", Binary}], []}
            ],
            case escript:create(Output, Sections) of
                ok ->
                    file:change_mode(Output, 8#755),
                    ok;
                {error, _} = Err -> Err
            end;
        error ->
            {error, {no_object_code, Mod}}
    end.

parse_args(Args) ->
    parse_args(Args, [], []).

parse_args([], Files, Opts) ->
    {lists:reverse(Files), Opts};
parse_args(["-o", Dir | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{o, Dir} | Opts]);
parse_args(["--selfhosted" | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{selfhosted, true} | Opts]);
%% Test-runner flags
parse_args(["--quiet" | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{render, quiet} | Opts]);
parse_args(["--verbose" | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{verbose, true} | Opts]);
parse_args(["--dashboard" | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{render, dashboard} | Opts]);
parse_args(["--stream" | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{render, stream} | Opts]);
parse_args(["--trace" | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{trace, true} | Opts]);
parse_args(["--match", Pattern | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{match, Pattern} | Opts]);
parse_args(["--seed", N | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{seed, list_to_integer(N)} | Opts]);
parse_args(["--tap" | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{tap, true} | Opts]);
parse_args(["--junit", Path | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{junit, Path} | Opts]);
parse_args(["--json", Path | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{json, Path} | Opts]);
parse_args(["--coverage" | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{coverage, true} | Opts]);
parse_args(["--lax-coverage" | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{lax_coverage, true} | Opts]);
parse_args(["--coverage-threshold", N | Rest], Files, Opts) ->
    parse_args(Rest, Files, [{coverage_threshold, list_to_integer(N)} | Opts]);
parse_args(["-" ++ _ = Flag | Rest], Files, Opts) ->
    io:format(standard_error, "Unknown flag: ~s~n", [Flag]),
    parse_args(Rest, Files, Opts);
parse_args([File | Rest], Files, Opts) ->
    parse_args(Rest, [File | Files], Opts).

usage() ->
    io:format(
        "a4c - ActorForth Compiler~n"
        "~n"
        "Usage:~n"
        "  a4c <file.a4>                  Compile to .beam in current dir~n"
        "  a4c compile <file.a4> [-o dir]  Compile to .beam~n"
        "  a4c build <file.a4> [-o out]    Build standalone escript~n"
        "  a4c run <file.a4>               Compile and run~n"
        "  a4c repl                        Start interactive REPL~n"
        "~n"
        "Options:~n"
        "  -o <path>        Output directory or file~n"
        "  --selfhosted     Use self-hosted compiler pipeline~n"
        "  --version        Show version~n"
        "  --help           Show this help~n"
    ).
