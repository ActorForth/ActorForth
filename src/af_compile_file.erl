-module(af_compile_file).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([compile/1, compile/2, compile_to_dir/2]).

%% Compile an .a4 file to a BEAM module.
%% Module name defaults to the file basename (without .a4).
%% Returns {ok, ModuleName} | {error, Reason}.
-spec compile(string()) -> {ok, atom()} | {error, term()}.
compile(File) ->
    BaseName = filename:basename(File, ".a4"),
    compile(File, list_to_atom(BaseName)).

-spec compile(string(), atom()) -> {ok, atom()} | {error, term()}.
compile(File, ModAtom) ->
    ensure_types(),
    case file:read_file(File) of
        {ok, Content} ->
            Tokens = af_parser:parse(binary_to_list(Content), File),
            _Cont = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
            AllDefs = find_all_compiled_words(),
            case AllDefs of
                [] -> {error, {no_compilable_words, File}};
                _ ->
                    af_word_compiler:compile_words_to_module(ModAtom, AllDefs)
            end;
        {error, Reason} ->
            {error, {read_error, File, Reason}}
    end.

%% Compile an .a4 file and write the .beam to a directory.
-spec compile_to_dir(string(), string()) -> {ok, atom()} | {error, term()}.
compile_to_dir(File, OutDir) ->
    BaseName = filename:basename(File, ".a4"),
    ModAtom = list_to_atom(BaseName),
    ensure_types(),
    case file:read_file(File) of
        {ok, Content} ->
            Tokens = af_parser:parse(binary_to_list(Content), File),
            _Cont = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
            AllDefs = find_all_compiled_words(),
            case AllDefs of
                [] -> {error, {no_compilable_words, File}};
                _ ->
                    case af_word_compiler:compile_words_to_binary(ModAtom, AllDefs) of
                        {ok, ModAtom, Binary} ->
                            ok = filelib:ensure_dir(filename:join(OutDir, "x")),
                            BeamFile = filename:join(OutDir, atom_to_list(ModAtom) ++ ".beam"),
                            ok = file:write_file(BeamFile, Binary),
                            {ok, ModAtom};
                        {error, _} = Err -> Err
                    end
            end;
        {error, Reason} ->
            {error, {read_error, File, Reason}}
    end.

%%% Internal

ensure_types() ->
    case ets:info(af_type_registry) of
        undefined -> af_repl:init_types();
        _ -> ok
    end.

find_all_compiled_words() ->
    AllTypes = af_type:all_types(),
    lists:flatmap(fun(#af_type{ops = Ops}) ->
        AllOps = lists:flatmap(fun({_Name, OpList}) -> OpList end, maps:to_list(Ops)),
        lists:filtermap(fun(#operation{name = N, sig_in = SI, sig_out = SO, source = {compiled, Body}}) ->
            {true, {N, SI, SO, Body}};
        (_) -> false
        end, AllOps)
    end, AllTypes).
