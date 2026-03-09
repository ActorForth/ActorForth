%% af_ring1.erl -- Ring 1: BEAM target backend for Ring 0 programs
%%
%% Compiles Ring 0 instruction sequences to BEAM modules via abstract forms.
%% Generated functions operate on tagged stacks: [{Type, Value}, ...]
%%
%% Two compilation modes:
%%   interpreted - wraps af_ring0:run (correct, supports all primitives)
%%   native      - direct abstract form translation (fast, subset of ops)
%%
%% Usage:
%%   WordDefs = [{"double", ['Int'], ['Int'], [dup, add]}],
%%   {ok, my_mod} = af_ring1:compile_module(my_mod, WordDefs).
%%   my_mod:double([{'Int', 21}]).  %% => [{'Int', 42}]

-module(af_ring1).

-export([
    compile_module/2,
    compile_module/3,
    compile_to_binary/2,
    compile_to_binary/3,
    save_module/2,
    get_module_binary/1
]).

-define(BINARY_TABLE, af_ring1_binaries).

%%% === Public API ===

compile_module(ModAtom, WordDefs) ->
    compile_module(ModAtom, WordDefs, []).

compile_module(ModAtom, WordDefs, Opts) ->
    case compile_to_binary(ModAtom, WordDefs, Opts) of
        {ok, Binary} ->
            code:load_binary(ModAtom, atom_to_list(ModAtom) ++ ".beam", Binary),
            {ok, ModAtom};
        {error, _} = Err -> Err
    end.

compile_to_binary(ModAtom, WordDefs) ->
    compile_to_binary(ModAtom, WordDefs, []).

compile_to_binary(ModAtom, WordDefs, Opts) ->
    Mode = proplists:get_value(mode, Opts, interpreted),
    L = 1,
    Forms = build_forms(ModAtom, WordDefs, Mode, L),
    case compile:forms(Forms, [binary, return_errors]) of
        {ok, ModAtom, Binary} ->
            store_binary(ModAtom, Binary),
            {ok, Binary};
        {error, Errors, _} ->
            {error, Errors}
    end.

save_module(ModAtom, Path) ->
    case get_module_binary(ModAtom) of
        {ok, Binary} -> file:write_file(Path, Binary);
        error -> {error, {no_binary, ModAtom}}
    end.

get_module_binary(ModAtom) ->
    ensure_binary_table(),
    case ets:lookup(?BINARY_TABLE, ModAtom) of
        [{_, Binary}] -> {ok, Binary};
        [] -> error
    end.

%%% === Form Generation ===

build_forms(ModAtom, WordDefs, Mode, L) ->
    Grouped = group_by_name(WordDefs),
    ModForm = {attribute, L, module, ModAtom},
    Exports = lists:usort([{list_to_atom(Name), 1} || {Name, _, _, _} <- WordDefs]),
    ExportForm = {attribute, L, export, Exports},
    WordsMap = build_words_map(WordDefs),
    FunForms = [gen_function(Name, Defs, Mode, L, WordsMap) || {Name, Defs} <- Grouped],
    [ModForm, ExportForm | FunForms].

group_by_name(WordDefs) ->
    lists:foldl(fun({Name, SI, SO, Body}, Acc) ->
        Existing = proplists:get_value(Name, Acc, []),
        lists:keystore(Name, 1, Acc, {Name, Existing ++ [{Name, SI, SO, Body}]})
    end, [], WordDefs).

build_words_map(WordDefs) ->
    maps:from_list([{Name, Body} || {Name, _, _, Body} <- WordDefs]).

gen_function(Name, Defs, Mode, L, WordsMap) ->
    FunAtom = list_to_atom(Name),
    Clauses = [gen_clause(Def, Mode, L, WordsMap) || Def <- Defs],
    {function, L, FunAtom, 1, Clauses}.

gen_clause({_Name, SigIn, _SigOut, Body}, interpreted, L, WordsMap) ->
    {HeadPat, _RestVar} = build_head_pattern(SigIn, L),
    %% Alias pattern: HeadPat = Stack
    FullPat = case SigIn of
        [] -> {var, L, 'Stack'};
        _  -> {match, L, HeadPat, {var, L, 'Stack'}}
    end,
    BodyExpr = gen_interpreted_body(Body, WordsMap, L),
    {clause, L, [FullPat], [], [BodyExpr]};

gen_clause({_Name, SigIn, _SigOut, Body}, native, L, WordsMap) ->
    {HeadPat, _RestVar} = build_head_pattern(SigIn, L),
    FullPat = case SigIn of
        [] -> {var, L, 'Stack'};
        _  -> {match, L, HeadPat, {var, L, 'Stack'}}
    end,
    BodyExpr = gen_native_body(Body, WordsMap, L),
    {clause, L, [FullPat], [], [BodyExpr]}.

%%% === Interpreted Mode ===
%%% Each word runs through af_ring0:run with embedded instruction list.

gen_interpreted_body(Body, WordsMap, L) ->
    BodyAbstract = erl_parse:abstract(Body),
    WordsAbstract = erl_parse:abstract(WordsMap),
    %% af_ring0:get_ds(af_ring0:run(Body, af_ring0:set_ds(Stack, af_ring0:set_words(Words, af_ring0:new()))))
    NewState = rcall(L, af_ring0, new, []),
    WithWords = rcall(L, af_ring0, set_words, [WordsAbstract, NewState]),
    WithStack = rcall(L, af_ring0, set_ds, [{var, L, 'Stack'}, WithWords]),
    RunResult = rcall(L, af_ring0, run, [BodyAbstract, WithStack]),
    rcall(L, af_ring0, get_ds, [RunResult]).

%%% === Native Mode ===
%%% Direct abstract form translation for common Ring 0 instructions.
%%% Falls back to af_ring0:exec for complex operations.

gen_native_body(Body, WordsMap, L) ->
    {ResultExpr, _VarIdx} = translate_body(Body, {var, L, 'Stack'}, L, WordsMap, 1),
    ResultExpr.

translate_body([], StackExpr, _L, _WordsMap, VarIdx) ->
    {StackExpr, VarIdx};
translate_body([Inst | Rest], StackExpr, L, WordsMap, VarIdx) ->
    case translate_native(Inst, StackExpr, L, WordsMap, VarIdx) of
        {inline, NewStackExpr, NewIdx} ->
            translate_body(Rest, NewStackExpr, L, WordsMap, NewIdx);
        {fallback, NewStackExpr, NewIdx} ->
            translate_body(Rest, NewStackExpr, L, WordsMap, NewIdx)
    end.

%% Inline translations for common ops
translate_native(dup, StackExpr, L, _WM, Idx) ->
    %% [H|T] = Stack, [H,H|T]
    H = var(L, Idx), T = var(L, Idx+1),
    Expr = {block, L, [
        {match, L, {cons, L, H, T}, StackExpr},
        {cons, L, H, {cons, L, H, T}}
    ]},
    {inline, Expr, Idx+2};

translate_native(drop, StackExpr, L, _WM, Idx) ->
    T = var(L, Idx),
    Expr = {block, L, [
        {match, L, {cons, L, {var, L, '_'}, T}, StackExpr},
        T
    ]},
    {inline, Expr, Idx+1};

translate_native(swap, StackExpr, L, _WM, Idx) ->
    A = var(L, Idx), B = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, A, {cons, L, B, R}}, StackExpr},
        {cons, L, B, {cons, L, A, R}}
    ]},
    {inline, Expr, Idx+3};

translate_native({lit, Value}, StackExpr, L, _WM, Idx) ->
    ValAbstract = erl_parse:abstract(Value),
    {inline, {cons, L, ValAbstract, StackExpr}, Idx};

translate_native(add, StackExpr, L, _WM, Idx) ->
    translate_arith_op('+', StackExpr, L, Idx);
translate_native(sub, StackExpr, L, _WM, Idx) ->
    translate_arith_op('-', StackExpr, L, Idx);
translate_native(mul, StackExpr, L, _WM, Idx) ->
    translate_arith_op('*', StackExpr, L, Idx);

translate_native(eq, StackExpr, L, _WM, Idx) ->
    A = var(L, Idx), B = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, A, {cons, L, B, R}}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'Bool'}, {op, L, '=:=', B, A}]}, R}
    ]},
    {inline, Expr, Idx+3};

translate_native(lt, StackExpr, L, _WM, Idx) ->
    AV = var(L, Idx+1),
    BV = var(L, Idx+3), R = var(L, Idx+4),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{var, L, '_'}, AV]},
                    {cons, L, {tuple, L, [{var, L, '_'}, BV]}, R}}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'Bool'}, {op, L, '<', BV, AV}]}, R}
    ]},
    {inline, Expr, Idx+5};

translate_native(not_op, StackExpr, L, _WM, Idx) ->
    V = var(L, Idx), R = var(L, Idx+1),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{atom, L, 'Bool'}, V]}, R}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'Bool'}, {op, L, 'not', V}]}, R}
    ]},
    {inline, Expr, Idx+2};

translate_native({call, Name}, StackExpr, L, WM, Idx) ->
    case maps:is_key(Name, WM) of
        true ->
            %% Same-module call: direct function call
            FunAtom = list_to_atom(Name),
            CallExpr = {call, L, {atom, L, FunAtom}, [StackExpr]},
            {inline, CallExpr, Idx};
        false ->
            translate_fallback({call, Name}, StackExpr, L, Idx)
    end;

%% Fallback: use af_ring0:exec for anything not inlined
translate_native(Inst, StackExpr, L, _WM, Idx) ->
    translate_fallback(Inst, StackExpr, L, Idx).

translate_arith_op(Op, StackExpr, L, Idx) ->
    AV = var(L, Idx), BV = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{var, L, '_'}, AV]},
                    {cons, L, {tuple, L, [{var, L, '_'}, BV]}, R}}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'Int'}, {op, L, Op, BV, AV}]}, R}
    ]},
    {inline, Expr, Idx+3}.

translate_fallback(Inst, StackExpr, L, Idx) ->
    InstAbstract = erl_parse:abstract(Inst),
    StateVar = var(L, Idx),
    ResultVar = var(L, Idx+1),
    Expr = {block, L, [
        {match, L, StateVar,
            rcall(L, af_ring0, set_ds, [StackExpr, rcall(L, af_ring0, new, [])])},
        {match, L, ResultVar,
            rcall(L, af_ring0, exec, [InstAbstract, StateVar])},
        rcall(L, af_ring0, get_ds, [ResultVar])
    ]},
    {fallback, Expr, Idx+2}.

%%% === Head Pattern Generation ===

build_head_pattern(SigIn, L) ->
    RestVar = {var, L, '__Rest'},
    Pats = lists:map(fun(Entry) ->
        case Entry of
            {Type, Value} when is_integer(Value) ->
                {tuple, L, [{atom, L, Type}, {integer, L, Value}]};
            {Type, Value} when is_float(Value) ->
                {tuple, L, [{atom, L, Type}, {float, L, Value}]};
            {Type, Value} when is_boolean(Value) ->
                {tuple, L, [{atom, L, Type}, {atom, L, Value}]};
            {Type, Value} when is_atom(Value) ->
                {tuple, L, [{atom, L, Type}, {atom, L, Value}]};
            Type when is_atom(Type), Type =:= 'Any' ->
                {var, L, '_'};
            Type when is_atom(Type) ->
                {tuple, L, [{atom, L, Type}, {var, L, '_'}]}
        end
    end, SigIn),
    HeadPat = lists:foldr(fun(Pat, Acc) ->
        {cons, L, Pat, Acc}
    end, RestVar, Pats),
    {HeadPat, RestVar}.

%%% === Helpers ===

var(L, Idx) ->
    Name = list_to_atom("V" ++ integer_to_list(Idx)),
    {var, L, Name}.

rcall(L, Mod, Fun, Args) ->
    {call, L, {remote, L, {atom, L, Mod}, {atom, L, Fun}}, Args}.

ensure_binary_table() ->
    case ets:info(?BINARY_TABLE) of
        undefined -> ets:new(?BINARY_TABLE, [named_table, public, set]);
        _ -> ok
    end.

store_binary(ModAtom, Binary) ->
    ensure_binary_table(),
    ets:insert(?BINARY_TABLE, {ModAtom, Binary}).
