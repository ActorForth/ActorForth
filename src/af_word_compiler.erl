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

%% Binary storage key prefix for process dictionary
-define(BIN_KEY(Mod), {af_module_binary, Mod}).

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
    case get(?BIN_KEY(ModuleName)) of
        undefined -> not_found;
        Binary -> {ok, Binary}
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
            {ok, FormList, ExportList} when is_list(FormList) ->
                {true, {FormList, ExportList}};
            {ok, Form, Export} ->
                {true, {[Form], [Export]}};
            {error, _Reason} -> false
        end
    end, Groups),
    case FunForms of
        [] -> {error, no_compilable_words};
        _ ->
            {FormLists, ExportLists} = lists:unzip(FunForms),
            Forms = lists:flatten(FormLists),
            Exports = lists:flatten(ExportLists),
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
    %% Try loop optimization for self-recursive words with << >> blocks
    case try_loop_opt(Name, Defs, L, Ctx) of
        {ok, Forms, Exports} ->
            {ok, Forms, Exports};
        not_applicable ->
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
            end
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

%% Loop optimization for self-recursive words.
%% Detects words like blast that have << >> send blocks and self-recursive
%% tail calls, and generates an inner loop with unpacked arguments to avoid
%% per-iteration list reconstruction and map lookups.

try_loop_opt(Name, Defs, L, Ctx) ->
    %% Partition into base cases and recursive cases
    {BaseCases, RecCases} = lists:partition(fun({_, _, _, Body}) ->
        case Body of
            [] -> true;
            _ ->
                LastOp = lists:last(Body),
                LastOp#operation.name =/= Name
        end
    end, Defs),
    case {BaseCases, RecCases} of
        {[_ | _], [{_, RecSigIn, _RecSigOut, RecBody}]} ->
            %% Single recursive clause with base case(s)
            %% Check if body starts with << word >> (send block)
            case has_send_block(RecBody) of
                {true, SendWord, BodyAfterSend} ->
                    %% Check if the body after send ends with self-recursive call
                    case lists:last(BodyAfterSend) of
                        #operation{name = Name} ->
                            generate_loop_opt(Name, BaseCases, RecSigIn,
                                              SendWord, BodyAfterSend, L, Ctx);
                        _ -> not_applicable
                    end;
                false -> not_applicable
            end;
        _ -> not_applicable
    end.

%% Check if body starts with << word >> pattern
has_send_block([#operation{name = "<<"}, #operation{name = Word}, #operation{name = ">>"} | Rest]) ->
    {true, Word, Rest};
has_send_block(_) -> false.

%% Generate optimized entry function + inner loop for self-recursive send patterns.
%% Entry: blast([{Actor,Info},{Int,N}|Rest]) -> blast_loop(Pid, ActorTuple, N, Rest)
%% Loop:  blast_loop(_, AT, 0, R) -> [AT|R]
%%        blast_loop(Pid, AT, N, R) -> cast, blast_loop(Pid, AT, N-1, R)
generate_loop_opt(Name, BaseCases, RecSigIn, SendWord, _BodyAfterSend, L, _Ctx) ->
    FunAtom = list_to_atom(Name),
    LoopAtom = list_to_atom(Name ++ "_loop"),

    %% Find which stack position is the Actor (check SigIn for Actor type)
    ActorPos = find_type_pos('Actor', RecSigIn),
    case ActorPos of
        not_found -> not_applicable;
        _ ->
            %% Find the variant position (typically Int for the counter)
            %% It's the position that changes in the recursive call
            VariantPos = find_variant_pos(RecSigIn, ActorPos),

            %% Generate entry function
            %% Pattern: [{Type1, V1}, {Type2, V2} | Rest]
            {EntryPat, TuplePats, ValVars, RestVar} = build_loop_entry_pattern(RecSigIn, L),
            ActorTuplePat = lists:nth(ActorPos + 1, TuplePats),
            ActorValVar = lists:nth(ActorPos + 1, ValVars),
            PidVar = {var, L, '__Pid'},
            PidBind = {match, L, PidVar, {call, L,
                {remote, L, {atom, L, maps}, {atom, L, get}},
                [{atom, L, pid}, ActorValVar]}},

            %% Build loop call args: Pid, ActorTuple, N, Rest
            NVar = lists:nth(VariantPos + 1, ValVars),
            LoopCallArgs = [PidVar, ActorTuplePat, NVar, RestVar],
            LoopCall = {call, L, {atom, L, LoopAtom}, LoopCallArgs},

            EntryClause = {clause, L, [EntryPat], [], [PidBind, LoopCall]},
            EntryForm = {function, L, FunAtom, 1, [EntryClause]},

            %% Generate loop function
            %% Base case(s): from the original base cases
            LoopBaseClauses = lists:filtermap(fun({_, BaseSigIn, _BaseSigOut, BaseBody}) ->
                generate_loop_base_clause(BaseSigIn, BaseBody, ActorPos, VariantPos, L)
            end, BaseCases),

            %% Recursive case: gen_server:cast(Pid, Msg), loop(Pid, AT, N-1, Rest)
            LoopPidVar = {var, L, '__LoopPid'},
            LoopActorVar = {var, L, '__LoopActor'},
            LoopNVar = {var, L, '__LoopN'},
            LoopRestVar = {var, L, '__LoopRest'},

            %% Generate send: Pid ! {cast, Word, []}
            MsgExpr = {tuple, L, [{atom, L, cast}, {atom, L, list_to_atom(SendWord)}, {nil, L}]},
            CastExpr = {op, L, '!', LoopPidVar, MsgExpr},

            %% Recursive call: loop(Pid, AT, N-1, Rest)
            RecCallArgs = [LoopPidVar, LoopActorVar,
                           {op, L, '-', LoopNVar, {integer, L, 1}}, LoopRestVar],
            RecCall = {call, L, {atom, L, LoopAtom}, RecCallArgs},

            RecClause = {clause, L,
                [LoopPidVar, LoopActorVar, LoopNVar, LoopRestVar],
                [], [CastExpr, RecCall]},

            LoopForm = {function, L, LoopAtom, 4,
                LoopBaseClauses ++ [RecClause]},

            {ok, [EntryForm, LoopForm],
                 [{FunAtom, 1}, {LoopAtom, 4}]}
    end.

%% Find the position (0-indexed) of a type in the SigIn list
find_type_pos(Type, SigIn) ->
    find_type_pos(Type, SigIn, 0).
find_type_pos(_Type, [], _I) -> not_found;
find_type_pos(Type, [Type | _], I) -> I;
find_type_pos(Type, [{Type, _} | _], I) -> I;
find_type_pos(Type, [_ | Rest], I) -> find_type_pos(Type, Rest, I + 1).

%% Find the position of the variant (non-Actor) argument
find_variant_pos(SigIn, ActorPos) ->
    find_variant_pos(SigIn, ActorPos, 0).
find_variant_pos([], _AP, _I) -> 0;
find_variant_pos([_ | Rest], AP, I) when I =:= AP ->
    find_variant_pos(Rest, AP, I + 1);
find_variant_pos([_ | _], _AP, I) -> I;
find_variant_pos(_, _, I) -> I.

%% Build pattern for entry function: [{Type, Var}, ... | Rest]
build_loop_entry_pattern(SigIn, L) ->
    {Pats, ValVars, _} = lists:foldl(fun(SigEntry, {PatAcc, VarAcc, I}) ->
        ValVar = {var, L, list_to_atom("__V" ++ integer_to_list(I))},
        Type = case SigEntry of {T, _} -> T; T -> T end,
        Pat = {tuple, L, [{atom, L, Type}, ValVar]},
        {PatAcc ++ [Pat], VarAcc ++ [ValVar], I + 1}
    end, {[], [], 0}, SigIn),
    FinalRestVar = {var, L, '__EntryRest'},
    HeadPat = lists:foldr(fun(Pat, Acc) ->
        {cons, L, Pat, Acc}
    end, FinalRestVar, Pats),
    %% Returns: {HeadPattern, TuplePatterns, ValueVars, RestVar}
    {HeadPat, Pats, ValVars, FinalRestVar}.

%% Generate a loop base case clause
generate_loop_base_clause(BaseSigIn, _BaseBody, _ActorPos, VariantPos, L) ->
    %% Find the value constraint in the base case
    VariantType = lists:nth(VariantPos + 1, BaseSigIn),
    case VariantType of
        {_Type, Value} when is_integer(Value) ->
            %% Base case with value constraint (e.g., 0 Int)
            %% loop(_, ActorTuple, 0, Rest) -> [ActorTuple | Rest]
            LoopPidVar = {var, L, '_BasePid'},
            LoopActorVar = {var, L, '__BaseActor'},
            LoopNPat = {integer, L, Value},
            LoopRestVar = {var, L, '__BaseRest'},

            %% Build return: [ActorTuple | Rest]
            ReturnExpr = {cons, L, LoopActorVar, LoopRestVar},

            Clause = {clause, L,
                [LoopPidVar, LoopActorVar, LoopNPat, LoopRestVar],
                [], [ReturnExpr]},
            {true, Clause};
        _ ->
            false
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
%% Detect << ... >> send blocks and compile them directly
simulate_body([#operation{name = "<<"} | Rest], Stack, L, Ctx, SideEffects) ->
    {SendOps, AfterSend} = collect_send_block(Rest),
    case compile_send_block(SendOps, Stack, L, Ctx) of
        {ok, NewStack, NewSideEffects} ->
            simulate_body(AfterSend, NewStack, L, Ctx,
                          lists:reverse(NewSideEffects) ++ SideEffects);
        {error, _Reason} ->
            %% Can't compile send block — fall through to runtime
            simulate_body(Rest, Stack, L, Ctx, SideEffects)
    end;
%% Quoted string literal on opaque stack: push string, keep opaque rest
simulate_body([#operation{name = OpName, source = quoted_string} | Rest], [{StackExpr, stack}], L, Ctx, SideEffects) ->
    BinExpr = {bin, L, [{bin_element, L, {string, L, OpName}, default, default}]},
    StrTagged = {tuple, L, [{atom, L, 'String'}, BinExpr]},
    NewExpr = {cons, L, StrTagged, StackExpr},
    simulate_body(Rest, [{NewExpr, stack}], L, Ctx, SideEffects);
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
%% Quoted string literals: push as tagged {String, Binary}
simulate_body([#operation{name = OpName, source = quoted_string} | Rest], Stack, L, Ctx, SideEffects) ->
    BinExpr = {bin, L, [{bin_element, L, {string, L, OpName}, default, default}]},
    simulate_body(Rest, [make_tagged('String', BinExpr, L) | Stack], L, Ctx, SideEffects);
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
translate_op("drop", [{_Expr, 'Int'} = A, {_ExprL, 'List'} = B | Rest], L, _Ctx) ->
    %% List drop: Int on TOS, List below -> list without first N items
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    LenExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, length}}, [ValB]},
    MinExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, min}}, [ValA, LenExpr]},
    ResultExpr = {call, L, {remote, L, {atom, L, lists}, {atom, L, nthtail}}, [MinExpr, ValB]},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};
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

%% String operations
translate_op("concat", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    %% Strings are binaries: <<B/binary, A/binary>>
    ResultExpr = {bin, L, [
        {bin_element, L, ValB, default, [binary]},
        {bin_element, L, ValA, default, [binary]}
    ]},
    {ok, [make_tagged('String', ResultExpr, L) | Rest]};
translate_op("length", [{_Expr, 'String'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, byte_size}}, [ValA]},
    {ok, [make_tagged('Int', ResultExpr, L) | Rest]};
translate_op("length", [{_Expr, 'List'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, length}}, [ValA]},
    {ok, [make_tagged('Int', ResultExpr, L) | Rest]};

%% List operations
translate_op("head", [{_Expr, 'List'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, hd}}, [ValA]},
    {ok, [{ResultExpr, 'Any'} | Rest]};
translate_op("tail", [{_Expr, 'List'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, tl}}, [ValA]},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};
translate_op("nil", Stack, L, _Ctx) ->
    {ok, [make_tagged('List', {nil, L}, L) | Stack]};
translate_op("cons", [Item, {ListExpr, 'List'} | Rest], L, _Ctx) ->
    {ItemExpr, _} = Item,
    ResultExpr = {cons, L, ItemExpr, extract_val({ListExpr, 'List'}, L)},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};
translate_op("reverse", [{_Expr, 'List'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, lists}, {atom, L, reverse}}, [ValA]},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};
translate_op("append", [{_ExprA, 'List'} = A, {_ExprB, 'List'} = B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {op, L, '++', ValB, ValA},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};

%% Map operations
translate_op("map-new", Stack, L, _Ctx) ->
    ResultExpr = {map, L, []},
    {ok, [make_tagged('Map', ResultExpr, L) | Stack]};
translate_op("map-size", [{_Expr, 'Map'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, maps}, {atom, L, size}}, [ValA]},
    {ok, [make_tagged('Int', ResultExpr, L) | Rest]};

%% Additional Int operations
translate_op("mod", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {op, L, 'rem', ValB, ValA},
    {ok, [make_tagged('Int', ResultExpr, L) | Rest]};
translate_op("abs", [{_Expr, 'Int'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, abs}}, [ValA]},
    {ok, [make_tagged('Int', ResultExpr, L) | Rest]};
translate_op("max", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, max}}, [ValB, ValA]},
    {ok, [make_tagged('Int', ResultExpr, L) | Rest]};
translate_op("min", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, min}}, [ValB, ValA]},
    {ok, [make_tagged('Int', ResultExpr, L) | Rest]};

%% Boolean logic
translate_op("and", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    {ok, [make_tagged('Bool', {op, L, 'andalso', ValB, ValA}, L) | Rest]};
translate_op("or", [A, B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    {ok, [make_tagged('Bool', {op, L, 'orelse', ValB, ValA}, L) | Rest]};

%% Additional List operations
translate_op("nth", [{_Expr, 'Int'} = A, {_ExprL, 'List'} = B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    %% lists:nth is 1-based, A4 nth is 0-based
    IndexExpr = {op, L, '+', ValA, {integer, L, 1}},
    ResultExpr = {call, L, {remote, L, {atom, L, lists}, {atom, L, nth}}, [IndexExpr, ValB]},
    {ok, [{ResultExpr, 'Any'} | Rest]};
translate_op("last", [{_Expr, 'List'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, lists}, {atom, L, last}}, [ValA]},
    {ok, [{ResultExpr, 'Any'} | Rest]};
translate_op("take", [{_Expr, 'Int'} = A, {_ExprL, 'List'} = B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {call, L, {remote, L, {atom, L, lists}, {atom, L, sublist}}, [ValB, ValA]},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};
translate_op("empty?", [{_Expr, 'List'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {op, L, '=:=', ValA, {nil, L}},
    {ok, [make_tagged('Bool', ResultExpr, L) | Rest]};
translate_op("contains?", [Item, {_ExprL, 'List'} = B | Rest], L, _Ctx) ->
    {ItemExpr, _} = Item,
    ValB = extract_val(B, L),
    ResultExpr = {call, L, {remote, L, {atom, L, lists}, {atom, L, member}}, [ItemExpr, ValB]},
    {ok, [make_tagged('Bool', ResultExpr, L) | Rest]};
translate_op("zip", [{_ExprA, 'List'} = A, {_ExprB, 'List'} = B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {call, L, {remote, L, {atom, L, lists}, {atom, L, zip}}, [ValB, ValA]},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};

%% Additional Map operations
%% Note: Maps store tagged items as keys/values, so we use full tagged exprs (not extract_val)
translate_op("map-put", [{ValExpr, _} = _V, {KeyExpr, _} = _K, {_ExprM, 'Map'} = M | Rest], L, _Ctx) ->
    ValM = extract_val(M, L),
    ResultExpr = {call, L, {remote, L, {atom, L, maps}, {atom, L, put}}, [KeyExpr, ValExpr, ValM]},
    {ok, [make_tagged('Map', ResultExpr, L) | Rest]};
translate_op("map-get", [{KeyExpr, _} = _K, {_ExprM, 'Map'} = M | Rest], L, _Ctx) ->
    ValM = extract_val(M, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, map_get}}, [KeyExpr, ValM]},
    {ok, [{ResultExpr, 'Any'} | Rest]};
translate_op("map-delete", [{KeyExpr, _} = _K, {_ExprM, 'Map'} = M | Rest], L, _Ctx) ->
    ValM = extract_val(M, L),
    ResultExpr = {call, L, {remote, L, {atom, L, maps}, {atom, L, remove}}, [KeyExpr, ValM]},
    {ok, [make_tagged('Map', ResultExpr, L) | Rest]};
translate_op("map-has?", [{KeyExpr, _} = _K, {_ExprM, 'Map'} = M | Rest], L, _Ctx) ->
    ValM = extract_val(M, L),
    ResultExpr = {call, L, {remote, L, {atom, L, maps}, {atom, L, is_key}}, [KeyExpr, ValM]},
    {ok, [make_tagged('Bool', ResultExpr, L) | Rest]};
translate_op("map-keys", [{_Expr, 'Map'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, maps}, {atom, L, keys}}, [ValA]},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};
translate_op("map-values", [{_Expr, 'Map'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, maps}, {atom, L, values}}, [ValA]},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};
translate_op("map-merge", [{_ExprA, 'Map'} = A, {_ExprB, 'Map'} = B | Rest], L, _Ctx) ->
    ValA = extract_val(A, L), ValB = extract_val(B, L),
    ResultExpr = {call, L, {remote, L, {atom, L, maps}, {atom, L, merge}}, [ValB, ValA]},
    {ok, [make_tagged('Map', ResultExpr, L) | Rest]};

%% Additional String operations
translate_op("to-atom", [{_Expr, 'String'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, binary_to_atom}}, [ValA]},
    {ok, [make_tagged('Atom', ResultExpr, L) | Rest]};
translate_op("to-int", [{_Expr, 'String'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, binary_to_integer}}, [ValA]},
    {ok, [make_tagged('Int', ResultExpr, L) | Rest]};
translate_op("to-string", [{_Expr, 'Int'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, integer_to_binary}}, [ValA]},
    {ok, [make_tagged('String', ResultExpr, L) | Rest]};
translate_op("to-string", [{_Expr, 'Atom'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, atom_to_binary}}, [ValA]},
    {ok, [make_tagged('String', ResultExpr, L) | Rest]};
translate_op("split", [{_ExprD, 'String'} = D, {_ExprS, 'String'} = S | Rest], L, _Ctx) ->
    ValD = extract_val(D, L), ValS = extract_val(S, L),
    ResultExpr = {call, L, {remote, L, {atom, L, binary}, {atom, L, split}},
        [ValS, {cons, L, ValD, {nil, L}}, {cons, L, {atom, L, global}, {nil, L}}]},
    {ok, [make_tagged('List', ResultExpr, L) | Rest]};
translate_op("trim", [{_Expr, 'String'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    %% Match interpreted: binary_to_list -> string:trim -> list_to_binary
    ListExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, binary_to_list}}, [ValA]},
    TrimExpr = {call, L, {remote, L, {atom, L, string}, {atom, L, trim}}, [ListExpr]},
    ResultExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, list_to_binary}}, [TrimExpr]},
    {ok, [make_tagged('String', ResultExpr, L) | Rest]};
translate_op("to-upper", [{_Expr, 'String'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    %% Match interpreted: binary_to_list -> string:uppercase -> unicode:characters_to_binary
    ListExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, binary_to_list}}, [ValA]},
    UpperExpr = {call, L, {remote, L, {atom, L, string}, {atom, L, uppercase}}, [ListExpr]},
    ResultExpr = {call, L, {remote, L, {atom, L, unicode}, {atom, L, characters_to_binary}}, [UpperExpr]},
    {ok, [make_tagged('String', ResultExpr, L) | Rest]};
translate_op("to-lower", [{_Expr, 'String'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    %% Match interpreted: binary_to_list -> string:lowercase -> unicode:characters_to_binary
    ListExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, binary_to_list}}, [ValA]},
    LowerExpr = {call, L, {remote, L, {atom, L, string}, {atom, L, lowercase}}, [ListExpr]},
    ResultExpr = {call, L, {remote, L, {atom, L, unicode}, {atom, L, characters_to_binary}}, [LowerExpr]},
    {ok, [make_tagged('String', ResultExpr, L) | Rest]};
translate_op("starts-with", [{_ExprP, 'String'} = P, {_ExprS, 'String'} = S | Rest], L, _Ctx) ->
    ValP = extract_val(P, L), ValS = extract_val(S, L),
    PLen = {call, L, {remote, L, {atom, L, erlang}, {atom, L, byte_size}}, [ValP]},
    PrefixExpr = {call, L, {remote, L, {atom, L, binary}, {atom, L, part}}, [ValS, {integer, L, 0}, PLen]},
    ResultExpr = {op, L, '=:=', PrefixExpr, ValP},
    {ok, [make_tagged('Bool', ResultExpr, L) | Rest]};
translate_op("ends-with", [{_ExprP, 'String'} = P, {_ExprS, 'String'} = S | Rest], L, _Ctx) ->
    ValP = extract_val(P, L), ValS = extract_val(S, L),
    PLen = {call, L, {remote, L, {atom, L, erlang}, {atom, L, byte_size}}, [ValP]},
    SLen = {call, L, {remote, L, {atom, L, erlang}, {atom, L, byte_size}}, [ValS]},
    StartPos = {op, L, '-', SLen, PLen},
    SuffixExpr = {call, L, {remote, L, {atom, L, binary}, {atom, L, part}}, [ValS, StartPos, PLen]},
    ResultExpr = {op, L, '=:=', SuffixExpr, ValP},
    {ok, [make_tagged('Bool', ResultExpr, L) | Rest]};
translate_op("contains", [{_ExprP, 'String'} = P, {_ExprS, 'String'} = S | Rest], L, _Ctx) ->
    ValP = extract_val(P, L), ValS = extract_val(S, L),
    MatchExpr = {call, L, {remote, L, {atom, L, binary}, {atom, L, match}}, [ValS, ValP]},
    ResultExpr = {op, L, '=/=', MatchExpr, {atom, L, nomatch}},
    {ok, [make_tagged('Bool', ResultExpr, L) | Rest]};
translate_op("replace", [{_ExprR, 'String'} = R, {_ExprP, 'String'} = P, {_ExprS, 'String'} = S | Rest], L, _Ctx) ->
    ValR = extract_val(R, L), ValP = extract_val(P, L), ValS = extract_val(S, L),
    ResultExpr = {call, L, {remote, L, {atom, L, binary}, {atom, L, replace}},
        [ValS, ValP, ValR, {cons, L, {atom, L, global}, {nil, L}}]},
    {ok, [make_tagged('String', ResultExpr, L) | Rest]};
translate_op("reverse", [{_Expr, 'String'} = A | Rest], L, _Ctx) ->
    ValA = extract_val(A, L),
    ResultExpr = {call, L, {remote, L, {atom, L, string}, {atom, L, reverse}}, [ValA]},
    {ok, [make_tagged('String', ResultExpr, L) | Rest]};
translate_op("substring", [{_ExprLen, 'Int'} = Len, {_ExprStart, 'Int'} = Start, {_ExprS, 'String'} = S | Rest], L, _Ctx) ->
    ValLen = extract_val(Len, L), ValStart = extract_val(Start, L), ValS = extract_val(S, L),
    ResultExpr = {call, L, {remote, L, {atom, L, binary}, {atom, L, part}}, [ValS, ValStart, ValLen]},
    {ok, [make_tagged('String', ResultExpr, L) | Rest]};

%% Product type getters: generates maps:get(FieldName, FieldMap)
%% Product type setters: generates maps:put(FieldName, NewValue, FieldMap)
translate_op(Name, Stack, L, Ctx) when length(Stack) >= 1 ->
    case try_product_op(Name, Stack, L, Ctx) of
        {ok, NewStack} -> {ok, NewStack};
        not_product -> translate_op_fallback(Name, Stack, L, Ctx)
    end;

%% Literals and word calls
translate_op(Name, Stack, L, Ctx) ->
    translate_op_fallback(Name, Stack, L, Ctx).

translate_op_fallback(Name, Stack, L, Ctx) ->
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

%% Try to compile a product type getter or setter.
%% Getters: instance on TOS, push field value + keep instance
%% Setters (name!): new_value and instance on stack, return updated instance
try_product_op(Name, [{_Expr, TosType} | _] = Stack, L, _Ctx) when is_atom(TosType), TosType =/= stack ->
    case lists:suffix("!", Name) of
        true when length(Stack) >= 2 ->
            %% Setter: TOS is the new value, second item is the instance
            BaseName = lists:droplast(Name),
            [{NewValExpr, _ValType}, {InstanceExpr, InstanceType} | Rest] = Stack,
            case catch af_type:get_type(InstanceType) of
                {ok, #af_type{ops = _Ops}} ->
                    FieldName = list_to_atom(BaseName),
                    FieldMapExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, element}},
                        [{integer, L, 2}, InstanceExpr]},
                    UpdatedMap = {map, L, FieldMapExpr,
                        [{map_field_exact, L, {atom, L, FieldName}, NewValExpr}]},
                    ResultExpr = {tuple, L, [{atom, L, InstanceType}, UpdatedMap]},
                    {ok, [{ResultExpr, InstanceType} | Rest]};
                _ -> not_product
            end;
        _ ->
            %% Getter: look in TOS type's ops
            case catch af_type:get_type(TosType) of
                {ok, #af_type{ops = Ops}} ->
                    case maps:get(Name, Ops, []) of
                        [#operation{source = auto, sig_out = [FieldType, _]} | _] ->
                            [{InstanceExpr, _} | Rest] = Stack,
                            FieldName = list_to_atom(Name),
                            FieldMapExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, element}},
                                [{integer, L, 2}, InstanceExpr]},
                            ValExpr = {call, L, {remote, L, {atom, L, erlang}, {atom, L, map_get}},
                                [{atom, L, FieldName}, FieldMapExpr]},
                            {ok, [{ValExpr, FieldType}, {InstanceExpr, TosType} | Rest]};
                        _ -> not_product
                    end;
                _ -> not_product
            end
    end;
try_product_op(_Name, _Stack, _L, _Ctx) ->
    not_product.

%% Collect operations between << and >>. Returns {SendOps, RemainingOps}.
collect_send_block(Ops) ->
    collect_send_block(Ops, []).
collect_send_block([#operation{name = ">>"} | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
collect_send_block([Op | Rest], Acc) ->
    collect_send_block(Rest, [Op | Acc]);
collect_send_block([], Acc) ->
    {lists:reverse(Acc), []}.

%% Compile a << ... >> send block to native BEAM code.
%% Handles: << word >> (no args, cast)
%%          << arg1 arg2 word >> (with args, cast or call)
%% Returns {ok, NewExprStack, SideEffects} | {error, Reason}.
compile_send_block([#operation{name = WordName}], Stack, L, _Ctx) ->
    %% Simple case: << word >> with no args
    case Stack of
        [{ActorExpr, 'Actor'} | RestStack] ->
            ActorInfoExpr = extract_val({ActorExpr, 'Actor'}, L),
            %% Determine cast vs call by looking up the word
            case is_actor_word_cast(WordName) of
                true ->
                    %% Generate: af_type_actor:send_cast(ActorInfo, Word, [])
                    %% Handles both supervised (gen_server) and raw actors
                    CastExpr = {call, L,
                        {remote, L, {atom, L, af_type_actor}, {atom, L, send_cast}},
                        [ActorInfoExpr, {atom, L, list_to_atom(WordName)}, {nil, L}]},
                    {ok, [{ActorExpr, 'Actor'} | RestStack], [CastExpr]};
                false ->
                    %% Call: generate af_type_actor:send_call and push results
                    CallExpr = {call, L,
                        {remote, L, {atom, L, af_type_actor}, {atom, L, send_call}},
                        [ActorInfoExpr, {atom, L, list_to_atom(WordName)}, {nil, L}]},
                    %% Result is a list of tagged values — go opaque
                    ResultExpr = {op, L, '++', CallExpr,
                        {cons, L, ActorExpr, build_stack_list(RestStack, L, {nil, L})}},
                    {ok, [{ResultExpr, stack}], []}
            end;
        _ ->
            {error, {no_actor_on_tos, Stack}}
    end;
compile_send_block(Ops, Stack, L, Ctx) when length(Ops) > 1 ->
    %% Multiple tokens: compute args on local stack, last token is the word
    WordOp = lists:last(Ops),
    ArgOps = lists:droplast(Ops),
    WordName = WordOp#operation.name,
    case Stack of
        [{ActorExpr, 'Actor'} | RestStack] ->
            %% Simulate arg computation on an empty local stack
            case simulate_body(ArgOps, [], L, Ctx, []) of
                {ok, ArgStack, ArgSideEffects} ->
                    ActorInfoExpr = extract_val({ActorExpr, 'Actor'}, L),
                    %% Build args list from local stack
                    ArgsListExpr = lists:foldr(fun({Expr, _Type}, Acc) ->
                        {cons, L, Expr, Acc}
                    end, {nil, L}, ArgStack),
                    case is_actor_word_cast(WordName) of
                        true ->
                            CastExpr = {call, L,
                                {remote, L, {atom, L, af_type_actor}, {atom, L, send_cast}},
                                [ActorInfoExpr, {atom, L, list_to_atom(WordName)}, ArgsListExpr]},
                            {ok, [{ActorExpr, 'Actor'} | RestStack],
                                 lists:reverse(ArgSideEffects) ++ [CastExpr]};
                        false ->
                            CallExpr = {call, L,
                                {remote, L, {atom, L, af_type_actor}, {atom, L, send_call}},
                                [ActorInfoExpr, {atom, L, list_to_atom(WordName)}, ArgsListExpr]},
                            ResultExpr = {op, L, '++', CallExpr,
                                {cons, L, ActorExpr, build_stack_list(RestStack, L, {nil, L})}},
                            {ok, [{ResultExpr, stack}], lists:reverse(ArgSideEffects)}
                    end;
                {error, _} = Err -> Err
            end;
        _ ->
            {error, {no_actor_on_tos, Stack}}
    end;
compile_send_block(_, _, _, _) ->
    {error, empty_send_block}.

%% Check if an actor word is a cast (no return values beyond state).
%% Looks up the word in all types and checks if sig_out has only one type
%% (the state type, meaning it's a cast).
is_actor_word_cast(WordName) ->
    AllTypes = af_type:all_types(),
    case find_word_sig(WordName, AllTypes) of
        {ok, SigIn, SigOut} ->
            %% Cast if outputs == inputs (only state changes, no return values)
            length(SigOut) =< length(SigIn);
        not_found ->
            %% Unknown word — assume cast (safer default for fire-and-forget)
            true
    end.

find_word_sig(_Name, []) -> not_found;
find_word_sig(Name, [#af_type{ops = Ops} | Rest]) ->
    case maps:get(Name, Ops, []) of
        [] -> find_word_sig(Name, Rest);
        [#operation{sig_in = SI, sig_out = SO} | _] -> {ok, SI, SO};
        _ -> find_word_sig(Name, Rest)
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
resolve_word_call(Name, Stack, L, #{module := _Mod, words := Words} = Ctx) ->
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

store_binary(ModuleName, Binary) ->
    put(?BIN_KEY(ModuleName), Binary),
    ok.

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
