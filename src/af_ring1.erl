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
%% Recognize select_clause pattern: {lit, {'List', Clauses}} followed by select_clause
translate_body([{lit, {'List', Clauses}}, select_clause | Rest], StackExpr, L, WordsMap, VarIdx) ->
    {CaseExpr, NewIdx} = translate_select_clause(Clauses, StackExpr, L, WordsMap, VarIdx),
    translate_body(Rest, CaseExpr, L, WordsMap, NewIdx);
%% Recognize rot: to_r, swap, from_r, swap → [A,B,C|R] → [C,A,B|R]
translate_body([to_r, swap, from_r, swap | Rest], StackExpr, L, WordsMap, VarIdx) ->
    A = var(L, VarIdx), B = var(L, VarIdx+1), C = var(L, VarIdx+2), R = var(L, VarIdx+3),
    Expr = {block, L, [
        {match, L, {cons, L, A, {cons, L, B, {cons, L, C, R}}}, StackExpr},
        {cons, L, C, {cons, L, A, {cons, L, B, R}}}
    ]},
    translate_body(Rest, Expr, L, WordsMap, VarIdx+4);
%% Recognize 2dup: two over patterns → [A,B|R] → [A,B,A,B|R]
translate_body([to_r, dup, from_r, swap, to_r, dup, from_r, swap | Rest], StackExpr, L, WordsMap, VarIdx) ->
    A = var(L, VarIdx), B = var(L, VarIdx+1), R = var(L, VarIdx+2),
    Expr = {block, L, [
        {match, L, {cons, L, A, {cons, L, B, R}}, StackExpr},
        {cons, L, A, {cons, L, B, {cons, L, A, {cons, L, B, R}}}}
    ]},
    translate_body(Rest, Expr, L, WordsMap, VarIdx+3);
%% Recognize over: to_r, dup, from_r, swap → [A,B|R] → [B,A,B|R]
translate_body([to_r, dup, from_r, swap | Rest], StackExpr, L, WordsMap, VarIdx) ->
    A = var(L, VarIdx), B = var(L, VarIdx+1), R = var(L, VarIdx+2),
    Expr = {block, L, [
        {match, L, {cons, L, A, {cons, L, B, R}}, StackExpr},
        {cons, L, B, {cons, L, A, {cons, L, B, R}}}
    ]},
    translate_body(Rest, Expr, L, WordsMap, VarIdx+3);
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

translate_native(divop, StackExpr, L, _WM, Idx) ->
    AV = var(L, Idx), BV = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{var, L, '_'}, AV]},
                    {cons, L, {tuple, L, [{var, L, '_'}, BV]}, R}}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'Int'}, {op, L, 'div', BV, AV}]}, R}
    ]},
    {inline, Expr, Idx+3};

translate_native(modop, StackExpr, L, _WM, Idx) ->
    AV = var(L, Idx), BV = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{var, L, '_'}, AV]},
                    {cons, L, {tuple, L, [{var, L, '_'}, BV]}, R}}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'Int'}, {op, L, 'rem', BV, AV}]}, R}
    ]},
    {inline, Expr, Idx+3};

translate_native(to_r, StackExpr, L, _WM, Idx) ->
    translate_fallback(to_r, StackExpr, L, Idx);

translate_native(from_r, StackExpr, L, _WM, Idx) ->
    translate_fallback(from_r, StackExpr, L, Idx);

translate_native(str_concat, StackExpr, L, _WM, Idx) ->
    AV = var(L, Idx), BV = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{atom, L, 'String'}, AV]},
                    {cons, L, {tuple, L, [{atom, L, 'String'}, BV]}, R}}, StackExpr},
        {cons, L,
            {tuple, L, [{atom, L, 'String'},
                {bin, L, [{bin_element, L, BV, default, [binary]},
                          {bin_element, L, AV, default, [binary]}]}]},
            R}
    ]},
    {inline, Expr, Idx+3};

translate_native(generic_len, StackExpr, L, _WM, Idx) ->
    V = var(L, Idx), R = var(L, Idx+1), Result = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, V, R}, StackExpr},
        {match, L, Result,
            {'case', L, V, [
                {clause, L, [{tuple, L, [{atom, L, 'String'}, var(L, Idx+3)]}], [],
                    [{tuple, L, [{atom, L, 'Int'},
                        rcall(L, erlang, byte_size, [var(L, Idx+3)])]}]},
                {clause, L, [{tuple, L, [{atom, L, 'List'}, var(L, Idx+4)]}], [],
                    [{tuple, L, [{atom, L, 'Int'},
                        rcall(L, erlang, length, [var(L, Idx+4)])]}]}
            ]}},
        {cons, L, Result, R}
    ]},
    {inline, Expr, Idx+5};

translate_native(and_op, StackExpr, L, _WM, Idx) ->
    A = var(L, Idx), B = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{atom, L, 'Bool'}, A]},
                    {cons, L, {tuple, L, [{atom, L, 'Bool'}, B]}, R}}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'Bool'}, {op, L, 'andalso', A, B}]}, R}
    ]},
    {inline, Expr, Idx+3};

translate_native(or_op, StackExpr, L, _WM, Idx) ->
    A = var(L, Idx), B = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{atom, L, 'Bool'}, A]},
                    {cons, L, {tuple, L, [{atom, L, 'Bool'}, B]}, R}}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'Bool'}, {op, L, 'orelse', A, B}]}, R}
    ]},
    {inline, Expr, Idx+3};

translate_native(nil, StackExpr, L, _WM, Idx) ->
    {inline, {cons, L, {tuple, L, [{atom, L, 'List'}, {nil, L}]}, StackExpr}, Idx};

translate_native(cons, StackExpr, L, _WM, Idx) ->
    Elem = var(L, Idx), ListVal = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, Elem,
                    {cons, L, {tuple, L, [{atom, L, 'List'}, ListVal]}, R}}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'List'}, {cons, L, Elem, ListVal}]}, R}
    ]},
    {inline, Expr, Idx+3};

translate_native(head, StackExpr, L, _WM, Idx) ->
    H = var(L, Idx), T = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{atom, L, 'List'}, {cons, L, H, T}]}, R}, StackExpr},
        {cons, L, H, R}
    ]},
    {inline, Expr, Idx+3};

translate_native(tail, StackExpr, L, _WM, Idx) ->
    T = var(L, Idx), R = var(L, Idx+1),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{atom, L, 'List'}, {cons, L, {var, L, '_'}, T}]}, R}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'List'}, T]}, R}
    ]},
    {inline, Expr, Idx+2};

%% Product type operations
translate_native({product_new, TypeName, Fields}, StackExpr, L, _WM, Idx) ->
    %% Pop N values from stack, build #{field => value} map
    N = length(Fields),
    {VarList, Idx1} = make_var_list(N, L, Idx),
    R = var(L, Idx1),
    %% Build pattern to match N items from stack
    MatchPat = lists:foldr(fun(V, Acc) -> {cons, L, V, Acc} end, R, VarList),
    %% Build map from fields and vars (fields are in order, stack has them reversed)
    RevVars = lists:reverse(VarList),
    MapPairs = lists:zipwith(fun(Field, V) ->
        {map_field_assoc, L, {atom, L, Field}, V}
    end, Fields, RevVars),
    MapExpr = {map, L, MapPairs},
    Expr = {block, L, [
        {match, L, MatchPat, StackExpr},
        {cons, L, {tuple, L, [{atom, L, TypeName}, MapExpr]}, R}
    ]},
    {inline, Expr, Idx1 + 1};

translate_native({product_get, Field}, StackExpr, L, _WM, Idx) ->
    %% Non-destructive: peek at TOS product, push field value on top
    MapVar = var(L, Idx), Val = var(L, Idx+1),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{var, L, '_'}, MapVar]}, {var, L, '_'}}, StackExpr},
        {match, L, Val, rcall(L, maps, get, [{atom, L, Field}, MapVar])},
        {cons, L, Val, StackExpr}
    ]},
    {inline, Expr, Idx+2};

translate_native({product_set, Field}, StackExpr, L, _WM, Idx) ->
    %% Pop value and product instance, update field, push updated instance
    NewVal = var(L, Idx), TypeVar = var(L, Idx+1), MapVar = var(L, Idx+2), R = var(L, Idx+3),
    Expr = {block, L, [
        {match, L, {cons, L, NewVal,
                    {cons, L, {tuple, L, [TypeVar, MapVar]}, R}}, StackExpr},
        {cons, L, {tuple, L, [TypeVar,
            rcall(L, maps, put, [{atom, L, Field}, NewVal, MapVar])]}, R}
    ]},
    {inline, Expr, Idx+4};

%% Map operations
translate_native(map_new, StackExpr, L, _WM, Idx) ->
    {inline, {cons, L, {tuple, L, [{atom, L, 'Map'}, {map, L, []}]}, StackExpr}, Idx};

translate_native(map_put, StackExpr, L, _WM, Idx) ->
    V = var(L, Idx), K = var(L, Idx+1), M = var(L, Idx+2), R = var(L, Idx+3),
    Expr = {block, L, [
        {match, L, {cons, L, V,
                    {cons, L, K,
                    {cons, L, {tuple, L, [{atom, L, 'Map'}, M]}, R}}}, StackExpr},
        {cons, L, {tuple, L, [{atom, L, 'Map'},
            rcall(L, maps, put, [K, V, M])]}, R}
    ]},
    {inline, Expr, Idx+4};

translate_native(map_get, StackExpr, L, _WM, Idx) ->
    K = var(L, Idx), M = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, K,
                    {cons, L, {tuple, L, [{atom, L, 'Map'}, M]}, R}}, StackExpr},
        {cons, L, rcall(L, maps, get, [K, M]), R}
    ]},
    {inline, Expr, Idx+3};

translate_native({apply_impl, OpName}, StackExpr, L, _WM, Idx) ->
    %% Push unknown operations as atoms — no ETS dependency
    AtomName = if is_binary(OpName) -> binary_to_atom(OpName, utf8);
                  is_list(OpName) -> list_to_atom(OpName);
                  is_atom(OpName) -> OpName;
                  true -> list_to_atom(lists:flatten(io_lib:format("~p", [OpName])))
               end,
    {inline, {cons, L, {tuple, L, [{atom, L, 'Atom'}, {atom, L, AtomName}]}, StackExpr}, Idx};

translate_native(print_tos, StackExpr, L, _WM, Idx) ->
    V = var(L, Idx), R = var(L, Idx+1),
    Expr = {block, L, [
        {match, L, {cons, L, V, R}, StackExpr},
        rcall(L, io, format, [{string, L, "~p~n"}, {cons, L, V, {nil, L}}]),
        R
    ]},
    {inline, Expr, Idx+2};

translate_native(print_stack, StackExpr, L, _WM, Idx) ->
    SV = var(L, Idx),
    Expr = {block, L, [
        {match, L, SV, StackExpr},
        rcall(L, io, format, [{string, L, "Stack: ~p~n"}, {cons, L, SV, {nil, L}}]),
        SV
    ]},
    {inline, Expr, Idx+1};

translate_native(assert_true, StackExpr, L, _WM, Idx) ->
    V = var(L, Idx), R = var(L, Idx+1),
    Expr = {block, L, [
        {match, L, {cons, L, {tuple, L, [{atom, L, 'Bool'}, V]}, R}, StackExpr},
        {'case', L, V, [
            {clause, L, [{atom, L, true}], [], [R]},
            {clause, L, [{var, L, '_'}], [],
                [{call, L, {remote, L, {atom, L, erlang}, {atom, L, error}},
                    [{atom, L, assertion_failed}]}]}
        ]}
    ]},
    {inline, Expr, Idx+2};

translate_native(assert_eq, StackExpr, L, _WM, Idx) ->
    A = var(L, Idx), B = var(L, Idx+1), R = var(L, Idx+2),
    Expr = {block, L, [
        {match, L, {cons, L, A, {cons, L, B, R}}, StackExpr},
        {'case', L, {op, L, '=:=', A, B}, [
            {clause, L, [{atom, L, true}], [], [R]},
            {clause, L, [{var, L, '_'}], [],
                [{call, L, {remote, L, {atom, L, erlang}, {atom, L, error}},
                    [{tuple, L, [{atom, L, assert_eq_failed},
                                 {tuple, L, [{atom, L, expected}, A]},
                                 {tuple, L, [{atom, L, got}, B]}]}]}]}
        ]}
    ]},
    {inline, Expr, Idx+3};

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

%%% === Native select_clause compilation ===
%%% Compiles pattern-matching sub-clauses to native BEAM case expressions.

translate_select_clause(Clauses, StackExpr, L, WordsMap, VarIdx) ->
    StackVar = var(L, VarIdx),
    Idx0 = VarIdx + 1,
    {CaseClauses, MaxIdx} = build_case_clauses(Clauses, L, WordsMap, Idx0),
    CaseExpr = {block, L, [
        {match, L, StackVar, StackExpr},
        {'case', L, StackVar, CaseClauses}
    ]},
    {CaseExpr, MaxIdx}.

build_case_clauses([], _L, _WM, Idx) -> {[], Idx};
build_case_clauses([{SigIn, Body} | Rest], L, WM, Idx) ->
    {Pattern, _RestVar, Idx1} = build_clause_pattern(SigIn, L, Idx),
    %% Compile the clause body against the RestVar (stack without matched items)
    %% But select_clause doesn't consume — it keeps matched items on stack.
    %% Wait, looking at Ring 0: select_clause pops clauses list, then runs body
    %% with `Rest` (stack sans clauses list). The matched items ARE on the stack.
    %% So body runs against the full stack (sans clauses list, which was already popped).
    %% For native compilation, the case matches against the stack, and the body
    %% runs with the full stack (including matched items).
    {BodyExpr, Idx2} = translate_body(Body, Pattern, L, WM, Idx1),
    Clause = {clause, L, [Pattern], [], [BodyExpr]},
    {RestClauses, Idx3} = build_case_clauses(Rest, L, WM, Idx2),
    {[Clause | RestClauses], Idx3}.

%% Build a pattern that matches the stack against a signature.
%% SigIn = [Entry] where Entry is Type atom or {Type, Value} constraint.
build_clause_pattern([], L, Idx) ->
    RestVar = var(L, Idx),
    {RestVar, RestVar, Idx + 1};
build_clause_pattern([Entry | SigRest], L, Idx) ->
    {ItemPat, Idx1} = build_sig_item_pattern(Entry, L, Idx),
    {RestPat, RestVar, Idx2} = build_clause_pattern(SigRest, L, Idx1),
    {{cons, L, ItemPat, RestPat}, RestVar, Idx2}.

%% Build a pattern for a single signature entry.
build_sig_item_pattern({Type, Value}, L, Idx) when is_atom(Type) ->
    %% Value constraint: match exact {Type, Value}
    ValAbstract = erl_parse:abstract(Value),
    {{tuple, L, [{atom, L, Type}, ValAbstract]}, Idx};
build_sig_item_pattern('Any', L, Idx) ->
    %% Any type: match anything
    {var(L, Idx), Idx + 1};
build_sig_item_pattern(Type, L, Idx) when is_atom(Type) ->
    %% Type match: match {Type, _}
    {{tuple, L, [{atom, L, Type}, var(L, Idx)]}, Idx + 1}.

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

make_var_list(0, _L, Idx) -> {[], Idx};
make_var_list(N, L, Idx) ->
    V = var(L, Idx),
    {Rest, Idx1} = make_var_list(N - 1, L, Idx + 1),
    {[V | Rest], Idx1}.

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
