-module(af_compile).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([compile_word/4, compile_module/2]).
-export([apply_impl/2]).  %% Used by generated code

%% Compile a word body into a direct-call chain, bypassing interpreter dispatch.
%% Returns a compiled #operation{} that can replace the interpreted version.
%%
%% The compiled impl pre-resolves all body tokens to their implementations
%% at compile time, eliminating ETS lookups at runtime.
compile_word(Name, SigIn, SigOut, Body) ->
    CompiledSteps = [compile_step(Op) || Op <- Body],
    Impl = fun(Cont) ->
        execute_compiled(CompiledSteps, Cont)
    end,
    #operation{
        name = Name,
        sig_in = SigIn,
        sig_out = SigOut,
        impl = Impl,
        source = compiled
    }.

%% Compile a list of word definitions into a dynamically-loaded BEAM module.
%% Each word becomes an exported function: word(Stack) -> NewStack.
%% WordDefs = [{Name, SigIn, SigOut, Body}]
compile_module(ModuleName, WordDefs) when is_atom(ModuleName) ->
    try
        Forms = build_module_forms(ModuleName, WordDefs),
        case compile:forms(Forms, [binary, return_errors]) of
            {ok, ModuleName, Binary} ->
                code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", Binary),
                {ok, ModuleName};
            {ok, ModuleName, Binary, _Warnings} ->
                code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", Binary),
                {ok, ModuleName};
            {error, Errors, _Warnings} ->
                {error, {compile_error, Errors}}
        end
    catch
        _:Reason -> {error, Reason}
    end.

%%% Internal: Direct compilation (closure-based)

compile_step(#operation{name = Name, impl = Impl}) ->
    %% For known stack primitives, use optimized direct implementations
    case Name of
        "dup"  -> fun(S) -> [hd(S) | S] end;
        "drop" -> fun([_ | R]) -> R end;
        "swap" -> fun([A, B | R]) -> [B, A | R] end;
        "rot"  -> fun([A, B, C | R]) -> [C, A, B | R] end;
        "over" -> fun([A, B | R]) -> [B, A, B | R] end;
        "+"    -> fun([{'Int', A}, {'Int', B} | R]) -> [{'Int', B + A} | R] end;
        "-"    -> fun([{'Int', A}, {'Int', B} | R]) -> [{'Int', B - A} | R] end;
        "*"    -> fun([{'Int', A}, {'Int', B} | R]) -> [{'Int', B * A} | R] end;
        _ ->
            %% General case: wrap the impl to work on stack directly
            fun(Stack) ->
                Cont = #continuation{data_stack = Stack},
                Result = Impl(Cont),
                Result#continuation.data_stack
            end
    end.

execute_compiled([], Cont) -> Cont;
execute_compiled([Step | Rest], Cont) ->
    NewStack = Step(Cont#continuation.data_stack),
    execute_compiled(Rest, Cont#continuation{data_stack = NewStack}).

%%% Internal: Module compilation (BEAM generation)

build_module_forms(ModuleName, WordDefs) ->
    L = 1,
    ModAttr = {attribute, L, module, ModuleName},
    ExportAttr = {attribute, L, export,
        [{list_to_atom(Name), 1} || {Name, _, _, _} <- WordDefs]},

    Functions = [build_word_function(Name, Body, L) || {Name, _, _, Body} <- WordDefs],

    [ModAttr, ExportAttr | Functions].

build_word_function(Name, Body, L) ->
    FunName = list_to_atom(Name),
    StackVar = {var, L, 'Stack0'},
    BodyExpr = build_body_chain(Body, StackVar, L, 0),
    Clause = {clause, L, [StackVar], [], [BodyExpr]},
    {function, L, FunName, 1, [Clause]}.

build_body_chain([], CurrentVar, _L, _N) ->
    CurrentVar;
build_body_chain([Op | Rest], CurrentVar, L, N) ->
    NextVar = {var, L, list_to_atom("Stack" ++ integer_to_list(N + 1))},
    OpExpr = build_op_call(Op, CurrentVar, L),
    case Rest of
        [] ->
            OpExpr;
        _ ->
            {block, L, [
                {match, L, NextVar, OpExpr},
                build_body_chain(Rest, NextVar, L, N + 1)
            ]}
    end.

build_op_call(#operation{name = Name}, StackVar, L) ->
    %% Call af_compile:apply_impl(Name, Stack)
    {call, L,
        {remote, L, {atom, L, af_compile}, {atom, L, apply_impl}},
        [{string, L, Name}, StackVar]}.

%% Called by generated BEAM code to execute an operation on a stack.
apply_impl(Name, Stack) ->
    case af_type:find_op(Name, Stack) of
        {ok, #operation{impl = Impl}} ->
            Cont = #continuation{data_stack = Stack},
            Result = Impl(Cont),
            Result#continuation.data_stack;
        not_found ->
            %% Push as atom
            [{'Atom', Name} | Stack]
    end.
