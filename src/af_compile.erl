-module(af_compile).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([compile_word/4, compile_module/2]).
-export([apply_impl/2]).  %% Used by generated code
%% Fast-path primitives: stack-taking versions of common interpreter ops.
%% Called directly from generated code in the opaque-stack path, bypassing
%% apply_impl's find_op lookup and continuation wrapping.
-export([prim_dup/1, prim_drop/1, prim_swap/1, prim_rot/1, prim_over/1,
         prim_2dup/1, prim_add/1, prim_sub/1, prim_mul/1, prim_div/1,
         prim_mod/1, prim_eq/1, prim_ne/1, prim_lt/1, prim_gt/1,
         prim_le/1, prim_ge/1, prim_not/1, prim_and/1, prim_or/1,
         prim_abs/1, prim_neg/1]).
-export([primitive_fast_fun/1]).

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

%% Map from primitive op name to the fast-path function atom exported here.
%% Used by af_word_compiler's opaque-stack path to emit direct calls instead
%% of apply_impl for the most common primitives.
primitive_fast_fun("dup")   -> {ok, prim_dup};
primitive_fast_fun("drop")  -> {ok, prim_drop};
primitive_fast_fun("pop")   -> {ok, prim_drop};
primitive_fast_fun("swap")  -> {ok, prim_swap};
primitive_fast_fun("rot")   -> {ok, prim_rot};
primitive_fast_fun("over")  -> {ok, prim_over};
primitive_fast_fun("2dup")  -> {ok, prim_2dup};
primitive_fast_fun("+")     -> {ok, prim_add};
primitive_fast_fun("-")     -> {ok, prim_sub};
primitive_fast_fun("*")     -> {ok, prim_mul};
primitive_fast_fun("/")     -> {ok, prim_div};
primitive_fast_fun("mod")   -> {ok, prim_mod};
primitive_fast_fun("==")    -> {ok, prim_eq};
primitive_fast_fun("!=")    -> {ok, prim_ne};
primitive_fast_fun("<")     -> {ok, prim_lt};
primitive_fast_fun(">")     -> {ok, prim_gt};
primitive_fast_fun("<=")    -> {ok, prim_le};
primitive_fast_fun(">=")    -> {ok, prim_ge};
primitive_fast_fun("not")   -> {ok, prim_not};
primitive_fast_fun("and")   -> {ok, prim_and};
primitive_fast_fun("or")    -> {ok, prim_or};
primitive_fast_fun("abs")   -> {ok, prim_abs};
primitive_fast_fun("neg")   -> {ok, prim_neg};
primitive_fast_fun(_)       -> not_found.

%%% Stack-taking primitive implementations.
%%% Each takes the tagged-stack list, returns the new tagged-stack list.
%%% No continuation wrapping; no ETS lookup.

prim_dup([H | T]) -> [H, H | T].
prim_drop([_ | T]) -> T.
prim_swap([A, B | T]) -> [B, A | T].
prim_rot([A, B, C | T]) -> [C, A, B | T].
prim_over([A, B | T]) -> [B, A, B | T].
prim_2dup([A, B | T]) -> [A, B, A, B | T].

prim_add([{'Int', A}, {'Int', B} | T]) -> [{'Int', B + A} | T].
prim_sub([{'Int', A}, {'Int', B} | T]) -> [{'Int', B - A} | T].
prim_mul([{'Int', A}, {'Int', B} | T]) -> [{'Int', B * A} | T].
prim_div([{'Int', A}, {'Int', B} | T]) -> [{'Int', B div A} | T].
prim_mod([{'Int', A}, {'Int', B} | T]) -> [{'Int', B rem A} | T].

prim_eq([{_, A}, {_, B} | T]) -> [{'Bool', B =:= A} | T].
prim_ne([{_, A}, {_, B} | T]) -> [{'Bool', B =/= A} | T].
prim_lt([{_, A}, {_, B} | T]) -> [{'Bool', B < A} | T].
prim_gt([{_, A}, {_, B} | T]) -> [{'Bool', B > A} | T].
prim_le([{_, A}, {_, B} | T]) -> [{'Bool', B =< A} | T].
prim_ge([{_, A}, {_, B} | T]) -> [{'Bool', B >= A} | T].

prim_not([{'Bool', B} | T]) -> [{'Bool', not B} | T].
prim_and([{'Bool', A}, {'Bool', B} | T]) -> [{'Bool', B andalso A} | T].
prim_or([{'Bool', A}, {'Bool', B} | T]) -> [{'Bool', B orelse A} | T].

prim_abs([{'Int', N} | T]) -> [{'Int', abs(N)} | T];
prim_abs([{'Float', F} | T]) -> [{'Float', abs(F)} | T].
prim_neg([{'Int', N} | T]) -> [{'Int', -N} | T];
prim_neg([{'Float', F} | T]) -> [{'Float', -F} | T].
