-module(rebar3_actorforth_compile).

-behaviour(provider).

-include("operation.hrl").
-include("af_type.hrl").

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile_a4).
-define(DEPS, [compile]).
-define(NAMESPACE, actorforth).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 actorforth compile_a4"},
        {opts, []},
        {short_desc, "Compile ActorForth (.a4) files to BEAM"},
        {desc, "Compiles .a4 files found in src/ into .beam modules "
               "in the ebin directory."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
        undefined -> rebar_state:project_apps(State);
        AppInfo -> [AppInfo]
    end,
    lists:foreach(fun(AppInfo) -> compile_app(AppInfo) end, Apps),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("ActorForth compile error: ~p", [Reason]).

%%% Internal

compile_app(AppInfo) ->
    SrcDir = rebar_app_info:dir(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    A4Files0 = filelib:wildcard(filename:join(SrcDir, "src/*.a4")),
    %% `.test.a4` files are test sources, not production code.
    A4Files = [F || F <- A4Files0, not lists:suffix(".test.a4", F)],
    case A4Files of
        [] -> ok;
        _ ->
            rebar_api:info("Compiling ~p ActorForth files", [length(A4Files)]),
            init_actorforth(),
            lists:foreach(fun(File) ->
                compile_a4_file(File, EbinDir)
            end, A4Files)
    end.

init_actorforth() ->
    %% Initialize the ActorForth type system (idempotent)
    case ets:info(af_type_registry) of
        undefined -> af_repl:init_types();
        _ -> ok
    end.

compile_a4_file(File, EbinDir) ->
    BaseName = filename:basename(File, ".a4"),
    ModAtom = list_to_atom(BaseName),
    rebar_api:info("  Compiling ~s -> ~s.beam", [BaseName, BaseName]),
    {ok, Content} = file:read_file(File),
    %% Parse and interpret to define words
    Tokens = af_parser:parse(binary_to_list(Content), File),
    Cont = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    %% Find all compiled words
    AllDefs = find_compiled_words_from_cont(Cont),
    case AllDefs of
        [] ->
            rebar_api:warn("  No compilable words found in ~s", [BaseName]);
        _ ->
            case af_word_compiler:compile_words_to_binary(ModAtom, AllDefs) of
                {ok, ModAtom, Binary} ->
                    BeamFile = filename:join(EbinDir, atom_to_list(ModAtom) ++ ".beam"),
                    ok = filelib:ensure_dir(BeamFile),
                    ok = file:write_file(BeamFile, Binary),
                    rebar_api:info("  Wrote ~s", [BeamFile]);
                {error, Reason} ->
                    rebar_api:error("  Failed to compile ~s: ~p", [BaseName, Reason])
            end
    end.

%% Find all compiled word definitions across all types.
find_compiled_words_from_cont(_Cont) ->
    AllTypes = af_type:all_types(),
    lists:flatmap(fun(#af_type{ops = Ops}) ->
        AllOps = lists:flatmap(fun({_Name, OpList}) -> OpList end, maps:to_list(Ops)),
        lists:filtermap(fun(#operation{name = N, sig_in = SI, sig_out = SO, source = {compiled, Body}}) ->
            {true, {N, SI, SO, Body}};
        (_) -> false
        end, AllOps)
    end, AllTypes).
