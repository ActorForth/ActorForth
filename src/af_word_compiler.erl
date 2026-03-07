-module(af_word_compiler).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([compile_words_to_module/2, compile_words_to_binary/2]).
-export([make_wrapper/4, get_module_binary/1, store_module_binary/2]).
-export([find_native_word/1]).
-export([find_compiled_word_defs/1]).
-export([group_by_name/1]).

-define(BINARY_TABLE, af_module_binaries).

%% Compile a list of ActorForth word definitions into a BEAM module.
%% Generated functions operate on tagged stacks: [{Type, Value}, ...].
%%
%% WordDefs = [{Name, SigIn, SigOut, Body}]
%%   Name: string word name
%%   SigIn: [atom() | {atom(), term()}] type signature (TOS-first)
%%   SigOut: [atom()] type signature (TOS-first)
%%   Body: [#operation{name = string()}] body token operations
%%
%% Generated functions take a tagged stack list and return a tagged stack list.
%%
%% Example:
%%   `: double Int -> Int ; dup + .`
%%   Generates: double([{Int, X} | Rest]) -> [{Int, X + X} | Rest].
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
%% Groups same-name words into multi-clause functions for pattern matching.
%% Returns {ok, ModuleName, Binary} | {error, Reason}.
compile_words_to_binary(ModuleName, WordDefs) when is_atom(ModuleName) ->
    L = 1,
    Ctx = #{module => ModuleName, words => build_word_index(WordDefs)},
    Groups = group_by_name(WordDefs),
    FunForms = lists:filtermap(fun({Name, Defs}) ->
        case compile_word_group(Name, Defs, L, Ctx) of
            {ok, Form, Export} -> {true, {Form, Export}};
            {error, _Reason} -> false
        end
    end, Groups),
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

%% Group word definitions by name, preserving order.
group_by_name(WordDefs) ->
    {Groups, Order} = lists:foldl(fun({Name, SI, SO, Body}, {Acc, Ord}) ->
        Existing = maps:get(Name, Acc, []),
        NewAcc = maps:put(Name, Existing ++ [{Name, SI, SO, Body}], Acc),
        NewOrd = case lists:member(Name, Ord) of
            true -> Ord;
            false -> Ord ++ [Name]
        end,
        {NewAcc, NewOrd}
    end, {#{}, []}, WordDefs),
    [{Name, maps:get(Name, Groups)} || Name <- Order].

%% Compile a group of same-name word defs.
%% All functions take arity 1 (the tagged stack list).
compile_word_group(Name, [{_, SigIn, SigOut, Body}], L, Ctx) ->
    compile_single_word(Name, SigIn, SigOut, Body, L, Ctx);
compile_word_group(Name, Defs, L, Ctx) ->
    FunAtom = list_to_atom(Name),
    Clauses = lists:filtermap(fun({_N, SigIn, SigOut, Body}) ->
        case compile_clause(SigIn, SigOut, Body, L, Ctx) of
            {ok, Clause} -> {true, Clause};
            {error, _} -> false
        end
    end, Defs),
    case Clauses of
        [] -> {error, no_compilable_clauses};
        _ ->
            Form = {function, L, FunAtom, 1, Clauses},
            {ok, Form, {FunAtom, 1}}
    end.

%% Compile a single-clause word function.
%% Function signature: fun([{Type, Val}, ...]) -> [{Type, Val}, ...]
compile_single_word(Name, SigIn, SigOut, Body, L, Ctx) ->
    FunAtom = list_to_atom(Name),
    {HeadPat, InitExprStack, RestVar} = build_head_pattern(SigIn, L),
    case simulate_body(Body, InitExprStack, L, Ctx#{rest_var => RestVar}) of
        {ok, ResultStack, SideEffects} ->
            ReturnExpr = build_tagged_return(ResultStack, SigOut, RestVar, L),
            BodyExprs = SideEffects ++ [ReturnExpr],
            Clause = {clause, L, [HeadPat], [], BodyExprs},
            Form = {function, L, FunAtom, 1, [Clause]},
            {ok, Form, {FunAtom, 1}};
        {error, Reason} ->
            {error, {Name, Reason}}
    end.

%% Compile a single clause for a multi-clause function.
compile_clause(SigIn, SigOut, Body, L, Ctx) ->
    {HeadPat, InitExprStack, RestVar} = build_head_pattern(SigIn, L),
    case simulate_body(Body, InitExprStack, L, Ctx#{rest_var => RestVar}) of
        {ok, ResultStack, SideEffects} ->
            ReturnExpr = build_tagged_return(ResultStack, SigOut, RestVar, L),
            BodyExprs = SideEffects ++ [ReturnExpr],
            Clause = {clause, L, [HeadPat], [], BodyExprs},
            {ok, Clause};
        {error, Reason} ->
            {error, Reason}
    end.

%% Build a function head pattern for tagged stack items.
%% Returns {HeadPattern, [{TaggedExpr, Type}], RestVar}
%% HeadPattern is a cons-list pattern like [{Int, X}, {Int, Y} | Rest]
%% suitable for use directly in the function head (not body match).
build_head_pattern(SigIn, L) ->
    {TuplePats, ExprStack, _} = lists:foldl(fun(SigEntry, {Pats, Exps, I}) ->
        case SigEntry of
            {Type, Value} when is_integer(Value) ->
                ValPat = {integer, L, Value},
                TuplePat = {tuple, L, [{atom, L, Type}, ValPat]},
                {Pats ++ [TuplePat], Exps ++ [{TuplePat, Type}], I + 1};
            {Type, Value} when is_float(Value) ->
                ValPat = {float, L, Value},
                TuplePat = {tuple, L, [{atom, L, Type}, ValPat]},
                {Pats ++ [TuplePat], Exps ++ [{TuplePat, Type}], I + 1};
            {Type, Value} when is_boolean(Value) ->
                ValPat = {atom, L, Value},
                TuplePat = {tuple, L, [{atom, L, Type}, ValPat]},
                {Pats ++ [TuplePat], Exps ++ [{TuplePat, Type}], I + 1};
            Type when is_atom(Type) ->
                Var = {var, L, arg_name(I)},
                TuplePat = {tuple, L, [{atom, L, Type}, Var]},
                {Pats ++ [TuplePat], Exps ++ [{TuplePat, Type}], I + 1}
        end
    end, {[], [], 1}, SigIn),
    FinalRestVar = {var, L, '__RestFinal'},
    HeadPat = lists:foldr(fun(Pat, Acc) ->
        {cons, L, Pat, Acc}
    end, FinalRestVar, TuplePats),
    {HeadPat, ExprStack, FinalRestVar}.

%% Build an ActorForth wrapper operation that calls a native BEAM function.
%% The wrapper pops tagged items, passes the tagged stack to the native function,
%% and pushes the result (which is already a tagged stack).
make_wrapper(ModAtom, FunAtom, SigIn, SigOut) ->
    Impl = fun(Cont) ->
        Stack = Cont#continuation.data_stack,
        %% Pass the full stack to the compiled function
        %% which will pattern match what it needs and leave the rest
        Result = erlang:apply(ModAtom, FunAtom, [Stack]),
        Cont#continuation{data_stack = Result}
    end,
    #operation{
        name = atom_to_list(FunAtom),
        sig_in = SigIn,
        sig_out = SigOut,
        impl = Impl,
        source = {native, ModAtom}
    }.

%% Simulate the word body on a typed expression stack.
%% Each stack entry is {Expr, Type} where Expr is an Erlang abstract form
%% and Type is an atom or 'unknown'.
%% Returns {ok, ResultExprStack, SideEffects} | {error, Reason}.
simulate_body(Ops, Stack, L, Ctx) ->
    simulate_body(Ops, Stack, L, Ctx, []).

simulate_body([], Stack, _L, _Ctx, SideEffects) ->
    {ok, Stack, lists:reverse(SideEffects)};
%% Opaque stack: check for same-module words first, then fall back to apply_impl
simulate_body([#operation{name = OpName} | Rest], [{StackExpr, stack}], L, Ctx, SideEffects) ->
    Words = maps:get(words, Ctx, #{}),
    NewExpr = case maps:get(OpName, Words, undefined) of
        {_Arity, _NumOut} ->
            %% Same-module word: call locally
            {call, L, {atom, L, list_to_atom(OpName)}, [StackExpr]};
        undefined ->
            {call, L,
                {remote, L, {atom, L, af_compile}, {atom, L, apply_impl}},
                [{string, L, OpName}, StackExpr]}
    end,
    simulate_body(Rest, [{NewExpr, stack}], L, Ctx, SideEffects);
simulate_body([#operation{name = OpName} | Rest], Stack, L, Ctx, SideEffects) ->
    case translate_op(OpName, Stack, L, Ctx) of
        {ok, NewStack} ->
            simulate_body(Rest, NewStack, L, Ctx, SideEffects);
        {side_effect, Expr, NewStack} ->
            simulate_body(Rest, NewStack, L, Ctx, [Expr | SideEffects]);
        {error, _} = Err -> Err
    end.

%% Helper: extract raw value from a typed expression
extract_val({Expr, _Type}, L) ->
    %% Generate: element(2, {Type, Val}) — but if Expr is already a variable
    %% that holds the raw value, just use it directly
    {call, L, {remote, L, {atom, L, erlang}, {atom, L, element}},
        [{integer, L, 2}, Expr]}.

%% Helper: make a tagged tuple expression {Type, ValExpr}
make_tagged(Type, ValExpr, L) ->
    {{tuple, L, [{atom, L, Type}, ValExpr]}, Type}.

%% Translate a single operation on the typed expression stack.

%% Stack operations — work on tagged items directly
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

%% Arithmetic: extract raw values, compute, re-tag
translate_op("+", [A, B | Rest], L, _Ctx) ->
    {_, TA} = A, {_, _TB} = B,
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {op, L, '+', ValB, ValA},
    {ok, [make_tagged(TA, ResultExpr, L) | Rest]};
translate_op("-", [A, B | Rest], L, _Ctx) ->
    {_, TA} = A,
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {op, L, '-', ValB, ValA},
    {ok, [make_tagged(TA, ResultExpr, L) | Rest]};
translate_op("*", [A, B | Rest], L, _Ctx) ->
    {_, TA} = A,
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {op, L, '*', ValB, ValA},
    {ok, [make_tagged(TA, ResultExpr, L) | Rest]};
translate_op("/", [A, B | Rest], L, _Ctx) ->
    {_, TA} = A,
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, 'div'}}, [ValB, ValA]},
    {ok, [make_tagged(TA, ResultExpr, L) | Rest]};

%% Comparison: produce Bool
translate_op("==", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    {ok, [make_tagged('Bool', {op, L, '=:=', ValB, ValA}, L) | Rest]};
translate_op("!=", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    {ok, [make_tagged('Bool', {op, L, '=/=', ValB, ValA}, L) | Rest]};
translate_op("<", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    {ok, [make_tagged('Bool', {op, L, '<', ValB, ValA}, L) | Rest]};
translate_op(">", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    {ok, [make_tagged('Bool', {op, L, '>', ValB, ValA}, L) | Rest]};
translate_op("<=", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    {ok, [make_tagged('Bool', {op, L, '=<', ValB, ValA}, L) | Rest]};
translate_op(">=", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    {ok, [make_tagged('Bool', {op, L, '>=', ValB, ValA}, L) | Rest]};

%% Boolean not
translate_op("not", [A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    {ok, [make_tagged('Bool', {op, L, 'not', ValA}, L) | Rest]};

%% Literals and word calls
translate_op(Name, Stack, L, Ctx) ->
    %% Try integer literal
    case catch list_to_integer(Name) of
        N when is_integer(N) ->
            {ok, [make_tagged('Int', {integer, L, N}, L) | Stack]};
        _ ->
            %% Try float literal
            case catch list_to_float(Name) of
                F when is_float(F) ->
                    {ok, [make_tagged('Float', {float, L, F}, L) | Stack]};
                _ ->
                    case Name of
                        "true" ->
                            {ok, [make_tagged('Bool', {atom, L, true}, L) | Stack]};
                        "false" ->
                            {ok, [make_tagged('Bool', {atom, L, false}, L) | Stack]};
                        _ ->
                            %% Try inter-word call or runtime fallback
                            case resolve_word_call(Name, Stack, L, Ctx) of
                                {ok, NewStack} -> {ok, NewStack};
                                not_found ->
                                    runtime_dispatch(Name, Stack, L, Ctx)
                            end
                    end
            end
    end.

%% Runtime dispatch: generate a call to af_compile:apply_impl(Name, Stack)
%% which will look up the op in the type system at runtime.
%% Result is opaque — subsequent ops dispatch via apply_impl.
runtime_dispatch(Name, Stack, L, Ctx) ->
    RestVar = maps:get(rest_var, Ctx, {nil, L}),
    StackExpr = build_stack_list(Stack, L, RestVar),
    CallExpr = {call, L,
        {remote, L, {atom, L, af_compile}, {atom, L, apply_impl}},
        [{string, L, Name}, StackExpr]},
    case lookup_op_effect(Name) of
        {ok, _NumIn, 0, _OutTypes} ->
            %% Side-effect only (like print) — execute and keep current stack
            {side_effect, CallExpr, Stack};
        _ ->
            %% Has outputs or unknown — result is opaque stack
            {ok, [{CallExpr, stack}]}
    end.

%% Build a cons-list expression from the typed expression stack.
%% RestVar is appended as the tail (represents unconsumed stack items).
build_stack_list([], _L, RestVar) ->
    RestVar;
build_stack_list([{Expr, _Type} | Rest], L, RestVar) ->
    {cons, L, Expr, build_stack_list(Rest, L, RestVar)}.

%% Look up an operation's stack effect from the type registry.
%% Returns {ok, NumIn, NumOut, OutTypes} | not_found
lookup_op_effect(Name) ->
    AllTypes = af_type:all_types(),
    lookup_op_in_types(Name, AllTypes).

lookup_op_in_types(_Name, []) -> not_found;
lookup_op_in_types(Name, [#af_type{ops = Ops} | Rest]) ->
    case maps:get(Name, Ops, []) of
        [] -> lookup_op_in_types(Name, Rest);
        [#operation{sig_in = SI, sig_out = SO} | _] ->
            OutTypes = [case S of {T, _} -> T; T -> T end || S <- SO],
            {ok, length(SI), length(SO), OutTypes};
        _ -> lookup_op_in_types(Name, Rest)
    end.

%% Resolve a word call: same-module or cross-module native.
resolve_word_call(Name, Stack, L, #{module := Mod, words := Words} = Ctx) ->
    case maps:get(Name, Words, undefined) of
        {_Arity, _NumOut} ->
            generate_local_call(Name, Stack, L, Ctx);
        undefined ->
            case find_native_word(Name) of
                {ok, NativeMod, _Arity, _NumOut} ->
                    generate_remote_call(Name, NativeMod, Stack, L, Ctx);
                not_found ->
                    not_found
            end
    end;
resolve_word_call(Name, Stack, L, Ctx) ->
    case find_native_word(Name) of
        {ok, NativeMod, _Arity, _NumOut} ->
            generate_remote_call(Name, NativeMod, Stack, L, Ctx);
        not_found ->
            not_found
    end.

%% Generate a local function call (same module).
%% The function takes a tagged stack list and returns a tagged stack list.
%% Result is opaque — subsequent ops dispatch via apply_impl.
generate_local_call(Name, Stack, L, Ctx) ->
    RestVar = maps:get(rest_var, Ctx, {nil, L}),
    StackExpr = build_stack_list(Stack, L, RestVar),
    CallExpr = {call, L, {atom, L, list_to_atom(Name)}, [StackExpr]},
    {ok, [{CallExpr, stack}]}.

%% Generate a remote function call (different module).
generate_remote_call(Name, NativeMod, Stack, L, Ctx) ->
    RestVar = maps:get(rest_var, Ctx, {nil, L}),
    StackExpr = build_stack_list(Stack, L, RestVar),
    CallExpr = {call, L,
        {remote, L, {atom, L, NativeMod}, {atom, L, list_to_atom(Name)}},
        [StackExpr]},
    {ok, [{CallExpr, stack}]}.

%% Build the return expression: a tagged stack list.
%% For opaque stack results, return the expression directly.
%% Otherwise, prepend result items to the rest-of-stack variable.
build_tagged_return([{Expr, stack}], _SigOut, _RestVar, _L) ->
    Expr;
build_tagged_return(ResultStack, _SigOut, RestVar, L) ->
    lists:foldr(fun({Expr, _Type}, Acc) ->
        {cons, L, Expr, Acc}
    end, RestVar, ResultStack).

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

%% Find all compiled word definitions for a given name across all types.
%% Returns [{Name, SigIn, SigOut, Body}] grouped by target type.
find_compiled_word_defs(Name) ->
    AllTypes = af_type:all_types(),
    lists:flatmap(fun(#af_type{ops = Ops}) ->
        case maps:get(Name, Ops, []) of
            [] -> [];
            OpList ->
                lists:filtermap(fun
                    (#operation{name = N, sig_in = SI, sig_out = SO, source = {compiled, Body}}) ->
                        {true, {N, SI, SO, Body}};
                    (_) -> false
                end, OpList)
        end
    end, AllTypes).

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
