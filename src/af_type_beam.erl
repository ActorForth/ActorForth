-module(af_type_beam).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

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

    %% All beam-* words registered in BeamModule/BeamFunction dicts
    %% so they dispatch when those types are TOS.

    %% beam-fun: ( BeamModule Arity:Int Name:Atom -- BeamFunction )
    %% Usage: my_mod beam-module 1 int double beam-fun
    %% Note: BeamModule must be TOS before pushing arity and name...
    %% Actually, let's use a "method" style where BeamModule stays TOS.
    %% Usage: my_mod beam-module   → [BeamModule]
    %%        beam-fun reads next two tokens as name and arity.
    %% Simpler: register in Any so args below BeamModule work.

    %% beam-fun: BeamModule is deepest, name and arity on top
    %% Stack at call: [Int(arity), Atom(name), BeamModule, ...]
    %% TOS = Int, so we must register in Any.
    af_type:add_op('Any', #operation{
        name = "beam-fun",
        sig_in = ['Int', 'Atom', 'BeamModule'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_fun/1
    }),

    %% beam-arg: [Int(n), BeamFunction, ...]
    af_type:add_op('Any', #operation{
        name = "beam-arg",
        sig_in = ['Int', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_arg/1
    }),

    %% beam-int: [Int(val), BeamFunction, ...]
    af_type:add_op('Any', #operation{
        name = "beam-int",
        sig_in = ['Int', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_int/1
    }),

    %% beam-atom: [Atom(val), BeamFunction, ...]
    af_type:add_op('Any', #operation{
        name = "beam-atom",
        sig_in = ['Atom', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_atom/1
    }),

    %% beam-call: [Int(arity), Atom(fun), Atom(mod), BeamFunction, ...]
    af_type:add_op('Any', #operation{
        name = "beam-call",
        sig_in = ['Int', 'Atom', 'Atom', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_call/1
    }),

    %% beam-op: [Int(arity), Atom(op), BeamFunction, ...]
    af_type:add_op('Any', #operation{
        name = "beam-op",
        sig_in = ['Int', 'Atom', 'BeamFunction'],
        sig_out = ['BeamFunction'],
        impl = fun op_beam_op/1
    }),

    %% beam-return: [BeamFunction, ...]
    af_type:add_op('BeamFunction', #operation{
        name = "beam-return",
        sig_in = ['BeamFunction'],
        sig_out = ['BeamModule'],
        impl = fun op_beam_return/1
    }),

    %% beam-compile: [BeamModule, ...]
    af_type:add_op('BeamModule', #operation{
        name = "beam-compile",
        sig_in = ['BeamModule'],
        sig_out = ['Atom'],
        impl = fun op_beam_compile/1
    }),

    %% compile-to-beam: [Atom(module_name), Atom(word_name), ...]
    %% Looks up a defined ActorForth word and compiles it to a BEAM module.
    af_type:add_op('Any', #operation{
        name = "compile-to-beam",
        sig_in = ['Atom', 'Atom'],
        sig_out = ['Atom'],
        impl = fun op_compile_to_beam/1
    }),

    ok.

%%% --- Implementation ---

%% beam-module: create a new module structure
op_beam_module(Cont) ->
    [{'Atom', Name} | Rest] = Cont#continuation.data_stack,
    ModName = list_to_atom(Name),
    ModVal = #{
        name => ModName,
        functions => [],
        exports => []
    },
    Cont#continuation{data_stack = [{'BeamModule', ModVal} | Rest]}.

%% beam-fun: start defining a function
%% Stack: [Int(arity), Atom(name), BeamModule, ...]
op_beam_fun(Cont) ->
    [{'Int', Arity}, {'Atom', FunName}, {'BeamModule', Mod} | Rest] = Cont#continuation.data_stack,
    FunAtom = list_to_atom(FunName),
    %% Generate argument variables
    ArgVars = [{var, 1, list_to_atom("Arg" ++ integer_to_list(I))} || I <- lists:seq(1, Arity)],
    FunVal = #{
        name => FunAtom,
        arity => Arity,
        args => ArgVars,
        body_exprs => [],          %% accumulated expressions
        module => Mod              %% parent module
    },
    Cont#continuation{data_stack = [{'BeamFunction', FunVal} | Rest]}.

%% beam-arg: push a reference to argument N (1-based)
%% Stack: [Int(n), BeamFunction, ...]
op_beam_arg(Cont) ->
    [{'Int', N}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{args := ArgVars, body_exprs := Exprs} = Fun,
    ArgVar = lists:nth(N, ArgVars),
    NewFun = Fun#{body_exprs => Exprs ++ [ArgVar]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

%% beam-int: push an integer literal expression
%% Stack: [Int(val), BeamFunction, ...]
op_beam_int(Cont) ->
    [{'Int', Val}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{body_exprs := Exprs} = Fun,
    Expr = {integer, 1, Val},
    NewFun = Fun#{body_exprs => Exprs ++ [Expr]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

%% beam-atom: push an atom literal expression
%% Stack: [Atom(val), BeamFunction, ...]
op_beam_atom(Cont) ->
    [{'Atom', Val}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{body_exprs := Exprs} = Fun,
    Expr = {atom, 1, list_to_atom(Val)},
    NewFun = Fun#{body_exprs => Exprs ++ [Expr]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

%% beam-call: emit a remote call Mod:Fun(last N exprs)
%% Stack: [Int(arity), Atom(fun), Atom(mod), BeamFunction, ...]
op_beam_call(Cont) ->
    [{'Int', Arity}, {'Atom', FName}, {'Atom', MName}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{body_exprs := Exprs} = Fun,
    %% Pop the last Arity expressions as arguments
    {Before, Args} = lists:split(length(Exprs) - Arity, Exprs),
    CallExpr = {call, 1,
        {remote, 1, {atom, 1, list_to_atom(MName)}, {atom, 1, list_to_atom(FName)}},
        Args},
    NewFun = Fun#{body_exprs => Before ++ [CallExpr]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

%% beam-op: apply a BIF operator to the last N expressions
%% Stack: [Int(arity), Atom(op), BeamFunction, ...]
op_beam_op(Cont) ->
    [{'Int', Arity}, {'Atom', OpName}, {'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{body_exprs := Exprs} = Fun,
    {Before, Args} = lists:split(length(Exprs) - Arity, Exprs),
    OpExpr = {op, 1, list_to_atom(OpName), hd(Args), lists:nth(2, Args)},
    NewFun = Fun#{body_exprs => Before ++ [OpExpr]},
    Cont#continuation{data_stack = [{'BeamFunction', NewFun} | Rest]}.

%% beam-return: finish a function, add it to the module
op_beam_return(Cont) ->
    [{'BeamFunction', Fun} | Rest] = Cont#continuation.data_stack,
    #{name := FunName, arity := Arity, args := ArgVars,
      body_exprs := Exprs, module := Mod} = Fun,
    %% Build the function form — last expression is the return value
    LastExpr = lists:last(Exprs),
    Clause = {clause, 1, ArgVars, [], [LastExpr]},
    FunForm = {function, 1, FunName, Arity, [Clause]},
    #{functions := Funs, exports := Exports} = Mod,
    NewMod = Mod#{
        functions => Funs ++ [FunForm],
        exports => Exports ++ [{FunName, Arity}]
    },
    Cont#continuation{data_stack = [{'BeamModule', NewMod} | Rest]}.

%% beam-compile: compile the module and load it
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

%% compile-to-beam: look up a defined word and compile it to a BEAM module.
%% Stack: [Atom(module_name), Atom(word_name), ...]
%% Searches all types for ops named WordName with source = {compiled, Body}.
op_compile_to_beam(Cont) ->
    [{'Atom', ModNameStr}, {'Atom', WordName} | Rest] = Cont#continuation.data_stack,
    ModAtom = list_to_atom(ModNameStr),
    WordDefs = find_compiled_words(WordName),
    case WordDefs of
        [] -> error({compile_to_beam, no_word_found, WordName});
        _ ->
            case af_word_compiler:compile_words_to_module(ModAtom, WordDefs) of
                {ok, ModAtom} ->
                    Cont#continuation{data_stack = [{'Atom', ModNameStr} | Rest]};
                {error, Reason} ->
                    error({compile_to_beam, Reason})
            end
    end.

%% Find all compiled word definitions with the given name across all types.
%% Returns [{Name, SigIn, SigOut, Body}] suitable for af_word_compiler.
find_compiled_words(WordName) ->
    AllTypes = af_type:all_types(),
    lists:flatmap(fun(#af_type{ops = Ops}) ->
        AllOps = lists:flatmap(fun({_Name, OpList}) -> OpList end, maps:to_list(Ops)),
        lists:filtermap(fun(#operation{name = N, sig_in = SI, sig_out = SO, source = {compiled, Body}}) when N =:= WordName ->
            {true, {N, SI, SO, Body}};
        (_) -> false
        end, AllOps)
    end, AllTypes).
