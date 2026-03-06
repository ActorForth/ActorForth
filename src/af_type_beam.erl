-module(af_type_beam).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).
-export([build_escript/3]).

%% The BEAM assembler: ActorForth types for building and compiling BEAM modules.
%%
%% BeamModule — represents a module being assembled
%% BeamFunction — represents a function being assembled within a module
%%
%% Workflow:
%%   beam-module       — ( Atom -- BeamModule )  start a new module
%%   beam-fun          — ( Atom Int BeamModule -- BeamFunction )  start function (name, arity)
%%   beam-arg          — ( Int BeamFunction -- BeamFunction )  reference argument N
%%   beam-int          — ( Int BeamFunction -- BeamFunction )  push integer literal
%%   beam-atom         — ( Atom BeamFunction -- BeamFunction )  push atom literal
%%   beam-call         — ( Atom Atom Int BeamFunction -- BeamFunction )  call Mod:Fun/Arity
%%   beam-op           — ( Atom Int BeamFunction -- BeamFunction )  apply BIF operator
%%   beam-return       — ( BeamFunction -- BeamModule )  finish function, return to module
%%   beam-compile      — ( BeamModule -- Atom )  compile module, returns module name
%%
%% High-level compilation:
%%   compile-to-beam   — ( Atom Atom -- Atom )  compile one word to BEAM, replace with native wrapper
%%   compile-all       — ( Atom -- Atom )  compile all user words to BEAM module
%%   save-module       — ( Atom String -- )  write compiled module to .beam file
%%   build-escript     — ( Atom Atom String -- )  build standalone escript executable
%%
%% The assembler builds Erlang abstract forms and compiles via compile:forms/2.

init() ->
    af_type:register_type(#af_type{name = 'BeamModule'}),
    af_type:register_type(#af_type{name = 'BeamFunction'}),

    %% beam-module: ( Atom -- BeamModule )
    af_type:add_op('Any', #operation{
        name = "beam-module",
        sig_in = ['Atom'],
        sig_out = ['BeamModule'],
        impl = fun op_beam_module/1
    }),

    af_type:add_op('Any', #operation{
        name = "beam-fun",
        sig_in = ['Int', 'Atom', 'BeamModule'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_fun/1
    }),

    af_type:add_op('Any', #operation{
        name = "beam-arg",
        sig_in = ['Int', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_arg/1
    }),

    af_type:add_op('Any', #operation{
        name = "beam-int",
        sig_in = ['Int', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_int/1
    }),

    af_type:add_op('Any', #operation{
        name = "beam-atom",
        sig_in = ['Atom', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_atom/1
    }),

    af_type:add_op('Any', #operation{
        name = "beam-call",
        sig_in = ['Int', 'Atom', 'Atom', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_call/1
    }),

    af_type:add_op('Any', #operation{
        name = "beam-op",
        sig_in = ['Int', 'Atom', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_op/1
    }),

    af_type:add_op('BeamFunction', #operation{
        name = "beam-return",
        sig_in = ['BeamFunction'],
        sig_out = ['BeamModule'],
        impl = fun op_beam_return/1
    }),

    af_type:add_op('BeamModule', #operation{
        name = "beam-compile",
        sig_in = ['BeamModule'],
        sig_out = ['Atom'],
        impl = fun op_beam_compile/1
    }),

    %% compile-to-beam: ( Atom(mod_name) Atom(word_name) -- Atom(mod_name) )
    %% Compiles a defined word to native BEAM and replaces it with a native wrapper.
    af_type:add_op('Any', #operation{
        name = "compile-to-beam",
        sig_in = ['Atom', 'Atom'],
        sig_out = ['Atom'],
        impl = fun op_compile_to_beam/1
    }),

    %% compile-all: ( Atom(mod_name) -- Atom(mod_name) )
    %% Compiles ALL user-defined words into one BEAM module with native wrappers.
    af_type:add_op('Any', #operation{
        name = "compile-all",
        sig_in = ['Atom'],
        sig_out = ['Atom'],
        impl = fun op_compile_all/1
    }),

    %% save-module: ( String(dir) Atom(mod_name) -- )
    %% Writes a compiled module's .beam file to disk.
    af_type:add_op('Any', #operation{
        name = "save-module",
        sig_in = ['String', 'Atom'],
        sig_out = [],
        impl = fun op_save_module/1
    }),

    %% build-escript: ( String(output_path) Atom(entry_fun) Atom(mod_name) -- )
    %% Builds a standalone escript executable.
    af_type:add_op('Any', #operation{
        name = "build-escript",
        sig_in = ['String', 'Atom', 'Atom'],
        sig_out = [],
        impl = fun op_build_escript/1
    }),

    ok.

%%% --- Low-level BEAM assembler ---

op_beam_module(Cont) ->
    [{'Atom', Name} | Rest] = Cont#continuation.data_stack,
    ModName = list_to_atom(Name),
    ModVal = #{
        name => ModName,
        functions => [],
        exports => []
    },
    Cont#continuation{data_stack = [{'BeamModule', ModVal} | Rest]}.

op_beam_fun(Cont) ->
    [{'Int', Arity}, {'Atom', FunName}, {'BeamModule', Mod} | Rest] = Cont#continuation.data_stack,
    FunAtom = list_to_atom(FunName),
    ArgVars = [{var, 1, list_to_atom("Arg" ++ integer_to_list(I))} || I <- lists:seq(1, Arity)],
    FunVal = #{
        name => FunAtom,
        arity => Arity,
        args => ArgVars,
        body_exprs => [],
        module => Mod
    },
    Cont#continuation{data_stack = [{'BeamFunction', FunVal} | Rest]}.

op_beam_arg(Cont) ->
    [{'Int', N}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{args := ArgVars, body_exprs := Exprs} = Fun,
    ArgVar = lists:nth(N, ArgVars),
    NewFun = Fun#{body_exprs => Exprs ++ [ArgVar]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

op_beam_int(Cont) ->
    [{'Int', Val}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{body_exprs := Exprs} = Fun,
    Expr = {integer, 1, Val},
    NewFun = Fun#{body_exprs => Exprs ++ [Expr]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

op_beam_atom(Cont) ->
    [{'Atom', Val}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{body_exprs := Exprs} = Fun,
    Expr = {atom, 1, list_to_atom(Val)},
    NewFun = Fun#{body_exprs => Exprs ++ [Expr]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

op_beam_call(Cont) ->
    [{'Int', Arity}, {'Atom', FName}, {'Atom', MName}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{body_exprs := Exprs} = Fun,
    {Before, Args} = lists:split(length(Exprs) - Arity, Exprs),
    CallExpr = {call, 1,
        {remote, 1, {atom, 1, list_to_atom(MName)}, {atom, 1, list_to_atom(FName)}},
        Args},
    NewFun = Fun#{body_exprs => Before ++ [CallExpr]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

op_beam_op(Cont) ->
    [{'Int', Arity}, {'Atom', OpName}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{body_exprs := Exprs} = Fun,
    {Before, Args} = lists:split(length(Exprs) - Arity, Exprs),
    OpExpr = {op, 1, list_to_atom(OpName), hd(Args), lists:nth(2, Args)},
    NewFun = Fun#{body_exprs => Before ++ [OpExpr]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

op_beam_return(Cont) ->
    [{'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{name := FunName, arity := Arity, args := ArgVars,
      body_exprs := Exprs, module := Mod} = Fun,
    LastExpr = lists:last(Exprs),
    Clause = {clause, 1, ArgVars, [], [LastExpr]},
    FunForm = {function, 1, FunName, Arity, [Clause]},
    #{functions := Funs, exports := Exports} = Mod,
    NewMod = Mod#{
        functions => Funs ++ [FunForm],
        exports => Exports ++ [{FunName, Arity}]
    },
    Cont#continuation{data_stack = [{'BeamModule', NewMod} | Rest]}.

op_beam_compile(Cont) ->
    [{'BeamModule', Mod} | Rest] = Cont#continuation.data_stack,
    #{name := ModName, functions := Funs, exports := Exports} = Mod,
    Forms = [
        {attribute, 1, module, ModName},
        {attribute, 1, export, Exports}
        | Funs
    ],
    case compile:forms(Forms, [binary, return_errors]) of
        {ok, ModName, Binary} ->
            code:load_binary(ModName, atom_to_list(ModName) ++ ".beam", Binary),
            Cont#continuation{data_stack = [{'Atom', atom_to_list(ModName)} | Rest]};
        {ok, ModName, Binary, _Warnings} ->
            code:load_binary(ModName, atom_to_list(ModName) ++ ".beam", Binary),
            Cont#continuation{data_stack = [{'Atom', atom_to_list(ModName)} | Rest]};
        {error, Errors, _Warnings} ->
            error({beam_compile_error, Errors})
    end.

%%% --- High-level compilation ---

%% compile-to-beam: compile a single word to native BEAM and replace
%% its ActorForth op with a native wrapper.
op_compile_to_beam(Cont) ->
    [{'Atom', ModNameStr}, {'Atom', WordName} | Rest] = Cont#continuation.data_stack,
    ModAtom = list_to_atom(ModNameStr),
    WordDefs = find_compiled_words(WordName),
    case WordDefs of
        [] -> error({compile_to_beam, no_word_found, WordName});
        _ ->
            case af_word_compiler:compile_words_to_module(ModAtom, WordDefs) of
                {ok, ModAtom} ->
                    register_wrappers(ModAtom, WordDefs),
                    Cont#continuation{data_stack = [{'Atom', ModNameStr} | Rest]};
                {error, Reason} ->
                    error({compile_to_beam, Reason})
            end
    end.

%% compile-all: compile ALL user-defined words into one BEAM module.
op_compile_all(Cont) ->
    [{'Atom', ModNameStr} | Rest] = Cont#continuation.data_stack,
    ModAtom = list_to_atom(ModNameStr),
    AllDefs = find_all_compiled_words(),
    case AllDefs of
        [] -> error({compile_all, no_compiled_words_found});
        _ ->
            case af_word_compiler:compile_words_to_module(ModAtom, AllDefs) of
                {ok, ModAtom} ->
                    register_wrappers(ModAtom, AllDefs),
                    Cont#continuation{data_stack = [{'Atom', ModNameStr} | Rest]};
                {error, Reason} ->
                    error({compile_all, Reason})
            end
    end.

%% save-module: write a loaded module's .beam to disk.
%% Stack: [String(dir_path), Atom(mod_name), ...]
op_save_module(Cont) ->
    [{'String', DirPath}, {'Atom', ModNameStr} | Rest] = Cont#continuation.data_stack,
    ModAtom = list_to_atom(ModNameStr),
    case af_word_compiler:get_module_binary(ModAtom) of
        {ok, Binary} ->
            Dir = binary_to_list(DirPath),
            ok = filelib:ensure_dir(Dir ++ "/"),
            FilePath = filename:join(Dir, atom_to_list(ModAtom) ++ ".beam"),
            ok = file:write_file(FilePath, Binary),
            Cont#continuation{data_stack = Rest};
        not_found ->
            error({save_module, module_not_compiled, ModAtom})
    end.

%% build-escript: create a standalone executable.
%% Stack: [String(output_path), Atom(entry_fun), Atom(mod_name), ...]
%% The entry function must take 0 arguments.
op_build_escript(Cont) ->
    [{'String', OutputPath}, {'Atom', EntryFunStr}, {'Atom', ModNameStr} | Rest] = Cont#continuation.data_stack,
    ModAtom = list_to_atom(ModNameStr),
    EntryFun = list_to_atom(EntryFunStr),
    OutputFile = binary_to_list(OutputPath),
    case build_escript(ModAtom, EntryFun, OutputFile) of
        ok ->
            Cont#continuation{data_stack = Rest};
        {error, Reason} ->
            error({build_escript, Reason})
    end.

%%% --- Internal helpers ---

%% Find all compiled word definitions with a given name across all types.
find_compiled_words(WordName) ->
    AllTypes = af_type:all_types(),
    lists:flatmap(fun(#af_type{ops = Ops}) ->
        AllOps = lists:flatmap(fun({_Name, OpList}) -> OpList end, maps:to_list(Ops)),
        lists:filtermap(fun(#operation{name = N, sig_in = SI, sig_out = SO, source = {compiled, Body}}) when N =:= WordName ->
            {true, {N, SI, SO, Body}};
        (_) -> false
        end, AllOps)
    end, AllTypes).

%% Find ALL compiled word definitions across all types.
find_all_compiled_words() ->
    AllTypes = af_type:all_types(),
    lists:flatmap(fun(#af_type{ops = Ops}) ->
        AllOps = lists:flatmap(fun({_Name, OpList}) -> OpList end, maps:to_list(Ops)),
        lists:filtermap(fun(#operation{name = N, sig_in = SI, sig_out = SO, source = {compiled, Body}}) ->
            {true, {N, SI, SO, Body}};
        (_) -> false
        end, AllOps)
    end, AllTypes).

%% Register native wrapper ops, replacing the interpreted versions.
register_wrappers(ModAtom, WordDefs) ->
    lists:foreach(fun({Name, SigIn, SigOut, _Body}) ->
        FunAtom = list_to_atom(Name),
        Wrapper = af_word_compiler:make_wrapper(ModAtom, FunAtom, SigIn, SigOut),
        %% Find and replace in the appropriate type dict
        TargetType = case SigIn of
            [T | _] -> T;
            [] -> 'Any'
        end,
        %% Replace all ops with this name in the target type
        case af_type:get_type(TargetType) of
            {ok, #af_type{ops = Ops}} ->
                case maps:get(Name, Ops, []) of
                    [] ->
                        %% Not found in target type, try Any
                        replace_word_ops('Any', Name, Wrapper);
                    _Existing ->
                        replace_word_ops(TargetType, Name, Wrapper)
                end;
            not_found ->
                replace_word_ops('Any', Name, Wrapper)
        end
    end, WordDefs).

%% Replace interpreted word ops with the native wrapper.
%% Only replaces ops that have source = {compiled, _}.
replace_word_ops(TypeName, OpName, Wrapper) ->
    case af_type:get_type(TypeName) of
        {ok, #af_type{ops = Ops}} ->
            case maps:get(OpName, Ops, []) of
                [] -> ok;
                OpList ->
                    NewList = lists:map(fun(#operation{source = {compiled, _}}) ->
                        Wrapper;
                    (Op) ->
                        Op
                    end, OpList),
                    af_type:replace_ops(TypeName, OpName, NewList)
            end;
        not_found -> ok
    end.

%% Build an escript executable from a compiled module.
%% Generates a main wrapper module, bundles both into an escript.
build_escript(ModAtom, EntryFun, OutputFile) ->
    %% Get the application module binary
    case af_word_compiler:get_module_binary(ModAtom) of
        {ok, AppBinary} ->
            %% Generate a main module that calls ModAtom:EntryFun()
            MainMod = list_to_atom(atom_to_list(ModAtom) ++ "_main"),
            MainBinary = build_main_module(MainMod, ModAtom, EntryFun),
            %% Build escript sections
            Sections = [
                {shebang, "/usr/bin/env escript"},
                {emu_args, "-escript main " ++ atom_to_list(MainMod)},
                {archive, [
                    {atom_to_list(MainMod) ++ ".beam", MainBinary},
                    {atom_to_list(ModAtom) ++ ".beam", AppBinary}
                ], []}
            ],
            case escript:create(OutputFile, Sections) of
                ok ->
                    %% Make executable
                    file:change_mode(OutputFile, 8#755),
                    ok;
                {error, Reason} ->
                    {error, {escript_create, Reason}}
            end;
        not_found ->
            {error, {module_not_compiled, ModAtom}}
    end.

%% Build a main wrapper module: main(_Args) -> Mod:Fun(), halt(0).
build_main_module(MainMod, AppMod, EntryFun) ->
    L = 1,
    Forms = [
        {attribute, L, module, MainMod},
        {attribute, L, export, [{main, 1}]},
        {function, L, main, 1, [
            {clause, L,
                [{var, L, '_Args'}],
                [],
                [
                    {call, L,
                        {remote, L, {atom, L, AppMod}, {atom, L, EntryFun}},
                        []},
                    {call, L,
                        {remote, L, {atom, L, erlang}, {atom, L, halt}},
                        [{integer, L, 0}]}
                ]}
        ]}
    ],
    {ok, MainMod, Binary} = compile:forms(Forms, [binary]),
    Binary.
