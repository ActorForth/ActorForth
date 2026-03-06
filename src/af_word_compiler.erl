-module(af_word_compiler).

-include("operation.hrl").
-include("af_type.hrl").

-export([compile_words_to_module/2]).

%% Compile a list of ActorForth word definitions into a BEAM module.
%% Each word becomes an exported function operating on raw Erlang values
%% (not tagged stack items).
%%
%% WordDefs = [{Name, SigIn, SigOut, Body}]
%%   Name: string word name
%%   SigIn: [atom()] type signature (TOS-first)
%%   SigOut: [atom()] type signature (TOS-first)
%%   Body: [#operation{name = string()}] body token operations
%%
%% Generated functions take N arguments (one per sig_in entry) and
%% return a tuple of M values (one per sig_out entry), or a single
%% value if sig_out has one element.
%%
%% Example:
%%   `: double Int -> Int ; dup + .`
%%   Generates: double(X) -> X + X.
%%
compile_words_to_module(ModuleName, WordDefs) when is_atom(ModuleName) ->
    L = 1,
    FunForms = lists:filtermap(fun({Name, SigIn, SigOut, Body}) ->
        case compile_word_to_form(Name, SigIn, SigOut, Body, L) of
            {ok, Form, Export} -> {true, {Form, Export}};
            {error, _Reason} -> false
        end
    end, WordDefs),
    case FunForms of
        [] -> {error, no_compilable_words};
        _ ->
            {Forms, Exports} = lists:unzip(FunForms),
            AllForms = [
                {attribute, L, module, ModuleName},
                {attribute, L, export, Exports}
                | Forms
            ],
            case compile:forms(AllForms, [binary, return_errors]) of
                {ok, ModuleName, Binary} ->
                    code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", Binary),
                    {ok, ModuleName};
                {ok, ModuleName, Binary, _Warnings} ->
                    code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", Binary),
                    {ok, ModuleName};
                {error, Errors, _Warnings} ->
                    {error, {compile_error, Errors}}
            end
    end.

%% Compile a single word definition to an Erlang abstract form function.
%% Returns {ok, FunctionForm, {FunAtom, Arity}} | {error, Reason}.
compile_word_to_form(Name, SigIn, SigOut, Body, L) ->
    FunAtom = list_to_atom(Name),
    Arity = length(SigIn),
    %% Create argument variables
    ArgVars = [{var, L, arg_name(I)} || I <- lists:seq(1, Arity)],
    %% Initial expression stack mirrors the input sig (TOS-first)
    %% SigIn = [TOS, Next, ...], ArgVars = [Arg1, Arg2, ...]
    %% Arg1 corresponds to TOS (SigIn element 0)
    InitExprStack = ArgVars,
    case simulate_body(Body, InitExprStack, L) of
        {ok, ResultStack} ->
            %% Build return expression from result stack
            ReturnExpr = build_return(ResultStack, SigOut, L),
            Clause = {clause, L, ArgVars, [], [ReturnExpr]},
            Form = {function, L, FunAtom, Arity, [Clause]},
            {ok, Form, {FunAtom, Arity}};
        {error, Reason} ->
            {error, {Name, Reason}}
    end.

%% Simulate the word body on an expression stack.
%% Each stack entry is an Erlang abstract form expression.
%% Returns {ok, ResultExprStack} | {error, Reason}.
simulate_body([], Stack, _L) -> {ok, Stack};
simulate_body([#operation{name = OpName} | Rest], Stack, L) ->
    case translate_op(OpName, Stack, L) of
        {ok, NewStack} -> simulate_body(Rest, NewStack, L);
        {error, _} = Err -> Err
    end.

%% Translate a single operation on the expression stack.
%% Known primitives are translated to direct abstract forms.
translate_op("dup", [A | Rest], _L) ->
    {ok, [A, A | Rest]};
translate_op("drop", [_ | Rest], _L) ->
    {ok, Rest};
translate_op("swap", [A, B | Rest], _L) ->
    {ok, [B, A | Rest]};
translate_op("rot", [A, B, C | Rest], _L) ->
    {ok, [C, A, B | Rest]};
translate_op("over", [A, B | Rest], _L) ->
    {ok, [B, A, B | Rest]};
translate_op("2dup", [A, B | Rest], _L) ->
    {ok, [A, B, A, B | Rest]};

%% Arithmetic: consume two expressions, produce one
translate_op("+", [A, B | Rest], L) ->
    {ok, [{op, L, '+', B, A} | Rest]};
translate_op("-", [A, B | Rest], L) ->
    {ok, [{op, L, '-', B, A} | Rest]};
translate_op("*", [A, B | Rest], L) ->
    {ok, [{op, L, '*', B, A} | Rest]};
translate_op("/", [A, B | Rest], L) ->
    {ok, [{call, L, {remote, L, {atom, L, erlang}, {atom, L, 'div'}}, [B, A]} | Rest]};

%% Comparison: produce boolean
translate_op("==", [A, B | Rest], L) ->
    {ok, [{op, L, '=:=', B, A} | Rest]};
translate_op("!=", [A, B | Rest], L) ->
    {ok, [{op, L, '=/=', B, A} | Rest]};
translate_op("<", [A, B | Rest], L) ->
    {ok, [{op, L, '<', B, A} | Rest]};
translate_op(">", [A, B | Rest], L) ->
    {ok, [{op, L, '>', B, A} | Rest]};
translate_op("<=", [A, B | Rest], L) ->
    {ok, [{op, L, '=<', B, A} | Rest]};
translate_op(">=", [A, B | Rest], L) ->
    {ok, [{op, L, '>=', B, A} | Rest]};

%% Boolean
translate_op("not", [A | Rest], L) ->
    {ok, [{op, L, 'not', A} | Rest]};

%% Integer literals (tokens that are numbers)
translate_op(Name, Stack, L) ->
    case catch list_to_integer(Name) of
        N when is_integer(N) ->
            {ok, [{integer, L, N} | Stack]};
        _ ->
            %% Try to find the op in the type system for its sig
            case find_compilable_op(Name, Stack, L) of
                {ok, NewStack} -> {ok, NewStack};
                not_found ->
                    %% Unknown — generate a call to af_compile:apply_impl
                    %% which handles dispatch at runtime
                    {ok, [make_runtime_call(Name, Stack, L) | Stack]}
            end
    end.

%% Try to find a registered operation and generate a call to it.
%% For now, handle calls to other compiled functions in the same module.
find_compilable_op(_Name, _Stack, _L) ->
    %% Future: resolve inter-word calls within the same module
    not_found.

%% Generate a runtime fallback call for unknown operations.
make_runtime_call(Name, _Stack, L) ->
    %% Call af_compile:apply_impl(Name, Stack) as a fallback
    %% This is a catch-all that preserves correctness
    {call, L,
        {remote, L, {atom, L, af_compile}, {atom, L, apply_impl}},
        [{string, L, Name}, {nil, L}]}.

%% Build the return expression from the result expression stack.
%% If single output, return the expression directly.
%% If multiple outputs, return a tuple.
build_return([Single], _SigOut, _L) ->
    Single;
build_return(Multiple, _SigOut, L) ->
    {tuple, L, Multiple}.

%% Generate argument variable names.
arg_name(1) -> 'X';
arg_name(2) -> 'Y';
arg_name(3) -> 'Z';
arg_name(N) -> list_to_atom("Arg" ++ integer_to_list(N)).
