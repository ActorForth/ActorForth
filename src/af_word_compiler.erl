-module(af_word_compiler).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([compile_words_to_module/2, compile_words_to_binary/2]).
-export([make_wrapper/4, get_module_binary/1, store_module_binary/2]).
-export([find_native_word/1]).

-define(BINARY_TABLE, af_module_binaries).

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
    case compile_words_to_binary(ModuleName, WordDefs) of
        {ok, ModuleName, Binary} ->
            code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", Binary),
            store_binary(ModuleName, Binary),
            {ok, ModuleName};
        {error, _} = Err ->
            Err
    end.

%% Retrieve a stored module binary. Returns {ok, Binary} | not_found.
get_module_binary(ModuleName) ->
    ensure_binary_table(),
    case ets:lookup(?BINARY_TABLE, ModuleName) of
        [{ModuleName, Binary}] -> {ok, Binary};
        [] -> not_found
    end.

%% Compile word definitions to a BEAM binary without loading.
%% Returns {ok, ModuleName, Binary} | {error, Reason}.
compile_words_to_binary(ModuleName, WordDefs) when is_atom(ModuleName) ->
    L = 1,
    %% Build compile context: maps word names to {Arity, NumOutputs} for inter-word calls
    Ctx = #{module => ModuleName, words => build_word_index(WordDefs)},
    FunForms = lists:filtermap(fun({Name, SigIn, SigOut, Body}) ->
        case compile_word_to_form(Name, SigIn, SigOut, Body, L, Ctx) of
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
                    {ok, ModuleName, Binary};
                {ok, ModuleName, Binary, _Warnings} ->
                    {ok, ModuleName, Binary};
                {error, Errors, _Warnings} ->
                    {error, {compile_error, Errors}}
            end
    end.

%% Build an ActorForth wrapper operation that calls a native BEAM function.
%% The wrapper unwraps {Type, Value} stack items, calls the native function,
%% and re-wraps the result with the correct output types.
make_wrapper(ModAtom, FunAtom, SigIn, SigOut) ->
    Arity = length(SigIn),
    Impl = fun(Cont) ->
        {Items, Rest} = lists:split(Arity, Cont#continuation.data_stack),
        RawArgs = [Val || {_Type, Val} <- Items],
        Result = erlang:apply(ModAtom, FunAtom, RawArgs),
        NewStack = wrap_results(Result, SigOut) ++ Rest,
        Cont#continuation{data_stack = NewStack}
    end,
    #operation{
        name = atom_to_list(FunAtom),
        sig_in = SigIn,
        sig_out = SigOut,
        impl = Impl,
        source = {native, ModAtom}
    }.

%% Compile a single word definition to an Erlang abstract form function.
%% Returns {ok, FunctionForm, {FunAtom, Arity}} | {error, Reason}.
compile_word_to_form(Name, SigIn, SigOut, Body, L, Ctx) ->
    FunAtom = list_to_atom(Name),
    Arity = length(SigIn),
    %% Create argument variables
    ArgVars = [{var, L, arg_name(I)} || I <- lists:seq(1, Arity)],
    %% Initial expression stack mirrors the input sig (TOS-first)
    %% SigIn = [TOS, Next, ...], ArgVars = [Arg1, Arg2, ...]
    %% Arg1 corresponds to TOS (SigIn element 0)
    InitExprStack = ArgVars,
    case simulate_body(Body, InitExprStack, L, Ctx) of
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
simulate_body([], Stack, _L, _Ctx) -> {ok, Stack};
simulate_body([#operation{name = OpName} | Rest], Stack, L, Ctx) ->
    case translate_op(OpName, Stack, L, Ctx) of
        {ok, NewStack} -> simulate_body(Rest, NewStack, L, Ctx);
        {error, _} = Err -> Err
    end.

%% Translate a single operation on the expression stack.
%% Known primitives are translated to direct abstract forms.
translate_op("dup", [A | Rest], _L, _Ctx) ->
    {ok, [A, A | Rest]};
translate_op("drop", [_ | Rest], _L, _Ctx) ->
    {ok, Rest};
translate_op("swap", [A, B | Rest], _L, _Ctx) ->
    {ok, [B, A | Rest]};
translate_op("rot", [A, B, C | Rest], _L, _Ctx) ->
    {ok, [C, A, B | Rest]};
translate_op("over", [A, B | Rest], _L, _Ctx) ->
    {ok, [B, A, B | Rest]};
translate_op("2dup", [A, B | Rest], _L, _Ctx) ->
    {ok, [A, B, A, B | Rest]};

%% Arithmetic: consume two expressions, produce one
translate_op("+", [A, B | Rest], L, _Ctx) ->
    {ok, [{op, L, '+', B, A} | Rest]};
translate_op("-", [A, B | Rest], L, _Ctx) ->
    {ok, [{op, L, '-', B, A} | Rest]};
translate_op("*", [A, B | Rest], L, _Ctx) ->
    {ok, [{op, L, '*', B, A} | Rest]};
translate_op("/", [A, B | Rest], L, _Ctx) ->
    {ok, [{call, L, {remote, L, {atom, L, erlang}, {atom, L, 'div'}}, [B, A]} | Rest]};

%% Comparison: produce boolean
translate_op("==", [A, B | Rest], L, _Ctx) ->
    {ok, [{op, L, '=:=', B, A} | Rest]};
translate_op("!=", [A, B | Rest], L, _Ctx) ->
    {ok, [{op, L, '=/=', B, A} | Rest]};
translate_op("<", [A, B | Rest], L, _Ctx) ->
    {ok, [{op, L, '<', B, A} | Rest]};
translate_op(">", [A, B | Rest], L, _Ctx) ->
    {ok, [{op, L, '>', B, A} | Rest]};
translate_op("<=", [A, B | Rest], L, _Ctx) ->
    {ok, [{op, L, '=<', B, A} | Rest]};
translate_op(">=", [A, B | Rest], L, _Ctx) ->
    {ok, [{op, L, '>=', B, A} | Rest]};

%% Boolean
translate_op("not", [A | Rest], L, _Ctx) ->
    {ok, [{op, L, 'not', A} | Rest]};

%% Integer literals and inter-word/cross-module calls
translate_op(Name, Stack, L, Ctx) ->
    case catch list_to_integer(Name) of
        N when is_integer(N) ->
            {ok, [{integer, L, N} | Stack]};
        _ ->
            case resolve_word_call(Name, Stack, L, Ctx) of
                {ok, NewStack} -> {ok, NewStack};
                not_found ->
                    {error, {unknown_op, Name}}
            end
    end.

%% Resolve a word call: same-module, cross-module native, or error.
resolve_word_call(Name, Stack, L, #{module := Mod, words := Words}) ->
    case maps:get(Name, Words, undefined) of
        {Arity, NumOut} ->
            %% Same-module call: generate local function call
            generate_local_call(Name, Arity, NumOut, Stack, L, Mod);
        undefined ->
            %% Try cross-module: look for already-compiled native words
            case find_native_word(Name) of
                {ok, NativeMod, Arity, NumOut} ->
                    generate_remote_call(Name, NativeMod, Arity, NumOut, Stack, L);
                not_found ->
                    not_found
            end
    end;
resolve_word_call(Name, Stack, L, _NoCtx) ->
    case find_native_word(Name) of
        {ok, NativeMod, Arity, NumOut} ->
            generate_remote_call(Name, NativeMod, Arity, NumOut, Stack, L);
        not_found ->
            not_found
    end.

%% Generate a local function call (same module).
generate_local_call(Name, Arity, NumOut, Stack, L, _Mod) ->
    case length(Stack) >= Arity of
        true ->
            {Args, Rest} = lists:split(Arity, Stack),
            %% Args are TOS-first; function expects them in order
            CallExpr = {call, L, {atom, L, list_to_atom(Name)}, Args},
            case NumOut of
                1 -> {ok, [CallExpr | Rest]};
                0 -> {ok, Rest};  %% void call — result discarded
                _ ->
                    %% Multiple returns: destructure tuple
                    expand_tuple_result(CallExpr, NumOut, Rest, L)
            end;
        false ->
            not_found
    end.

%% Generate a remote function call (different module).
generate_remote_call(Name, NativeMod, Arity, NumOut, Stack, L) ->
    case length(Stack) >= Arity of
        true ->
            {Args, Rest} = lists:split(Arity, Stack),
            CallExpr = {call, L,
                {remote, L, {atom, L, NativeMod}, {atom, L, list_to_atom(Name)}},
                Args},
            case NumOut of
                1 -> {ok, [CallExpr | Rest]};
                0 -> {ok, Rest};
                _ -> expand_tuple_result(CallExpr, NumOut, Rest, L)
            end;
        false ->
            not_found
    end.

%% Expand a tuple result into multiple stack entries using element/2.
expand_tuple_result(CallExpr, NumOut, Rest, L) ->
    %% Bind result to a variable, then extract elements
    TmpVar = {var, L, '__Result'},
    Elements = [{call, L,
        {remote, L, {atom, L, erlang}, {atom, L, element}},
        [{integer, L, I}, TmpVar]} || I <- lists:seq(1, NumOut)],
    %% Use a block expression: begin __Result = Call, ... end
    BlockExpr = {block, L, [
        {match, L, TmpVar, CallExpr}
        | Elements
    ]},
    %% Only push the last element as the result (single expression stack entry)
    %% For multi-return, we'd need to restructure — for now push as tuple
    {ok, [{call, L, {atom, L, list_to_atom("__tuple_result")}, [BlockExpr]} | Rest]}.

%% Look up a word that has been compiled to a native BEAM module.
%% Returns {ok, Module, Arity, NumOutputs} | not_found.
find_native_word(Name) ->
    AllTypes = af_type:all_types(),
    find_native_in_types(Name, AllTypes).

find_native_in_types(_Name, []) -> not_found;
find_native_in_types(Name, [#af_type{ops = Ops} | Rest]) ->
    case maps:get(Name, Ops, []) of
        [] -> find_native_in_types(Name, Rest);
        OpList ->
            case find_native_op(OpList) of
                {ok, Mod, Arity, NumOut} -> {ok, Mod, Arity, NumOut};
                not_found -> find_native_in_types(Name, Rest)
            end
    end.

find_native_op([]) -> not_found;
find_native_op([#operation{source = {native, Mod}, sig_in = SigIn, sig_out = SigOut} | _]) ->
    {ok, Mod, length(SigIn), length(SigOut)};
find_native_op([_ | Rest]) ->
    find_native_op(Rest).

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

%% Build an index of word names -> {Arity, NumOutputs} for inter-word call resolution.
build_word_index(WordDefs) ->
    lists:foldl(fun({Name, SigIn, SigOut, _Body}, Acc) ->
        maps:put(Name, {length(SigIn), length(SigOut)}, Acc)
    end, #{}, WordDefs).

%%% Internal

ensure_binary_table() ->
    case ets:info(?BINARY_TABLE) of
        undefined -> ets:new(?BINARY_TABLE, [named_table, set, public]);
        _ -> ok
    end.

store_binary(ModuleName, Binary) ->
    ensure_binary_table(),
    ets:insert(?BINARY_TABLE, {ModuleName, Binary}).

%% Public version for other modules that compile BEAM modules.
store_module_binary(ModuleName, Binary) ->
    store_binary(ModuleName, Binary).

%% Wrap a native function result back into tagged stack items.
%% Single value -> [{Type, Value}]
%% Tuple -> zip with SigOut types
wrap_results(Value, [SingleType]) ->
    [{SingleType, Value}];
wrap_results(Tuple, Types) when is_tuple(Tuple) ->
    lists:zipwith(fun(Type, Val) -> {Type, Val} end, Types, tuple_to_list(Tuple)).
