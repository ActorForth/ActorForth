%% af_core_target.erl -- Core Erlang compilation target
%%
%% Experimental alternative to af_word_compiler's Erlang abstract-forms path.
%% Emits Core Erlang (cerl AST) directly, compiled via `compile:forms/2`
%% with the `from_core` option.
%%
%% Status (v1): MVP subset. Supports stack operations (dup, drop, swap),
%% arithmetic (+, -, *, /), Int literals, and single-clause word definitions.
%% Does NOT support multi-clause dispatch, product types, actors, FFI, or
%% most collection ops. Words using unsupported primitives will be rejected
%% with {error, {unsupported_primitive, Name}}.
%%
%% Motivation: Core Erlang gives finer control over the emitted IR than the
%% existing abstract-forms backend. Some optimisations (case coalescing,
%% inline expansion, better TCO visibility) are natural in Core but awkward
%% in abstract forms. This module lays the groundwork; full feature parity
%% with af_word_compiler is a future effort.

-module(af_core_target).

-include("operation.hrl").

-export([compile_word/4, compile_words_to_module/2, compile_words_to_binary/2]).
-export([supported_primitives/0]).

%%====================================================================
%% Public API
%%====================================================================

%% compile_word(Name, SigIn, SigOut, Body) -> {ok, Cerl} | {error, Reason}
%%
%% Compile a single word definition to a Core Erlang function definition.
%% Returns a {FNameCerl, FunCerl} pair suitable for inclusion in a module.
compile_word(Name, SigIn, _SigOut, Body) ->
    try
        case validate_body(Body) of
            ok ->
                FName = cerl:c_fname(list_to_atom(Name), 1),
                Arg   = cerl:c_var('_Stack'),
                BodyCerl = build_body(SigIn, Body),
                Fun   = cerl:c_fun([Arg], BodyCerl),
                {ok, {FName, wrap_with_destructure(Arg, SigIn, Fun)}};
            {error, _} = Err -> Err
        end
    catch
        throw:ThrownReason -> {error, ThrownReason};
        Class:Reason -> {error, {Class, Reason}}
    end.

%% compile_words_to_module(ModName, WordDefs) -> {ok, ModName} | {error, Reason}
%%
%% Compile and load a module.
compile_words_to_module(ModName, WordDefs) when is_atom(ModName) ->
    case compile_words_to_binary(ModName, WordDefs) of
        {ok, ModName, Binary} ->
            code:load_binary(ModName, atom_to_list(ModName) ++ ".beam", Binary),
            {ok, ModName};
        {error, _} = Err -> Err
    end.

%% compile_words_to_binary(ModName, WordDefs) -> {ok, ModName, Binary} | {error, Reason}
%%
%% WordDefs = [{Name, SigIn, SigOut, Body}, ...]
compile_words_to_binary(ModName, WordDefs) when is_atom(ModName) ->
    try
        Defs = lists:map(fun({Name, SigIn, SigOut, Body}) ->
            case compile_word(Name, SigIn, SigOut, Body) of
                {ok, Pair} -> Pair;
                {error, Reason} -> throw({compile_error, Name, Reason})
            end
        end, WordDefs),
        Exports = [FName || {FName, _} <- Defs],
        ModCerl = cerl:c_module(
            cerl:c_atom(ModName),
            Exports,
            [],
            Defs
        ),
        case compile:forms(ModCerl, [from_core, binary, return_errors]) of
            {ok, ModName, Bin} -> {ok, ModName, Bin};
            {ok, ModName, Bin, _Warnings} -> {ok, ModName, Bin};
            {error, Errs, _Warns} -> {error, {core_compile, Errs}};
            error -> {error, core_compile_failed}
        end
    catch
        throw:Reason -> {error, Reason}
    end.

%% supported_primitives/0 -> [string()]
%%
%% List of primitive names this target currently handles. Useful for tooling
%% that wants to pre-filter word definitions before attempting compilation.
supported_primitives() ->
    ["dup", "drop", "swap", "+", "-", "*", "/"].

%%====================================================================
%% Body validation
%%====================================================================

validate_body([]) -> ok;
validate_body([#operation{name = Name} | Rest]) ->
    case is_supported_primitive(Name) orelse parse_int_literal(Name) =/= error of
        true -> validate_body(Rest);
        false -> {error, {unsupported_primitive, Name}}
    end.

is_supported_primitive(Name) ->
    lists:member(Name, supported_primitives()).

%%====================================================================
%% Body construction
%%====================================================================

%% build_body(SigIn, Body) -> cerl()
%%
%% Emit a Core Erlang expression that represents the word body. The input
%% stack is bound to the variable '_Stack' (see compile_word). The body is
%% built as a fold that threads an "abstract stack" of cerl expressions.
build_body(SigIn, Body) ->
    %% Destructure the input sig into concrete cerl vars, then interpret
    %% body ops against that cerl stack.
    {Vars, RestVar} = destructure_vars(SigIn),
    Destructured = cerl:make_list(Vars, cerl:c_var(RestVar)),
    InputMatch = cerl:c_var('_Stack'),
    CaseClause = cerl:c_clause([Destructured],
                                 interpret_body(Vars, RestVar, Body)),
    cerl:c_case(InputMatch, [CaseClause]).

wrap_with_destructure(_Arg, _SigIn, Fun) ->
    %% Destructuring is folded into the body's case expression, so the fun
    %% itself is just fun(_Stack) -> case _Stack of ... end end.
    Fun.

destructure_vars(SigIn) ->
    %% Bind a cerl variable for each sig_in entry. Order is TOS-first
    %% (index 0 = TOS) matching the af_type convention.
    Vars = [cerl:c_var(list_to_atom("_V" ++ integer_to_list(I)))
            || I <- lists:seq(0, length(SigIn) - 1)],
    %% Each var actually binds the {Type, Value} tuple.
    Tuples = [cerl:make_list(
                [cerl:c_var(list_to_atom("_V" ++ integer_to_list(I)))], cerl:c_nil())
              || I <- lists:seq(0, length(SigIn) - 1)],
    _ = Tuples,
    {Vars, '_Rest'}.

interpret_body(Vars, RestVar, Body) ->
    %% Starting stack: the tagged {Type, Value} tuples one per var, TOS first.
    InitStack = [cerl:c_var(cerl:var_name(V)) || V <- Vars],
    FinalStack = lists:foldl(fun(Op, Stack) ->
        apply_op(Op, Stack)
    end, InitStack, Body),
    rebuild_list(FinalStack, RestVar).

apply_op(#operation{name = "dup"}, [Top | Rest]) ->
    [Top, Top | Rest];
apply_op(#operation{name = "drop"}, [_ | Rest]) ->
    Rest;
apply_op(#operation{name = "swap"}, [A, B | Rest]) ->
    [B, A | Rest];
apply_op(#operation{name = "+"}, Stack) -> binop('+', Stack);
apply_op(#operation{name = "-"}, Stack) -> binop('-', Stack);
apply_op(#operation{name = "*"}, Stack) -> binop('*', Stack);
apply_op(#operation{name = "/"}, Stack) -> binop('div', Stack);
apply_op(#operation{name = Name}, Stack) ->
    case parse_int_literal(Name) of
        {ok, N} ->
            Lit = cerl:c_tuple([cerl:c_atom('Int'), cerl:c_int(N)]),
            [Lit | Stack];
        error ->
            throw({unsupported_primitive, Name})
    end.

binop(Op, [B, A | Rest]) ->
    %% Stack is TOS-first: top was pushed last. For `a b -`: a - b.
    AVal = extract_int(A),
    BVal = extract_int(B),
    Sum = cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(Op), [AVal, BVal]),
    Tup = cerl:c_tuple([cerl:c_atom('Int'), Sum]),
    [Tup | Rest].

%% Extract the integer value from a {Int, N} tagged tuple expression.
%% Works for both literal tuples we built and variables referring to input
%% {Int, X} cells.
extract_int(Expr) ->
    case cerl:is_c_tuple(Expr) of
        true ->
            %% It's a literal {Int, N} we constructed. Grab element 2.
            [_TypeAtom, ValExpr] = cerl:tuple_es(Expr),
            ValExpr;
        false ->
            %% It's a variable; project element 2 via erlang:element/2.
            cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(element),
                        [cerl:c_int(2), Expr])
    end.

%% Rebuild the Erlang list representation of the stack from cerl expressions.
rebuild_list(Stack, RestVar) ->
    %% Each element of `Stack` is either a cerl var referring to a
    %% {Type, Value} tuple (passed through) or a fresh cerl tuple we built.
    Elems = lists:map(fun lift_stack_item/1, Stack),
    cerl:make_list(Elems, cerl:c_var(RestVar)).

lift_stack_item(Expr) ->
    case cerl:is_c_tuple(Expr) of
        true -> Expr;
        false -> Expr  %% variable: already a tuple at runtime
    end.

parse_int_literal(Str) ->
    try {ok, list_to_integer(Str)}
    catch _:_ -> error
    end.
