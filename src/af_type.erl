-module(af_type).

-include("token.hrl").
-include("operation.hrl").
-include("af_type.hrl").
-include("continuation.hrl").

-export([init/0, reset/0]).
-export([register_type/1, add_op/2, replace_ops/3, any_op_with_name/1]).
-export([find_op/2, find_op_in_tos/2, find_op_in_any/2, find_op_by_name/2, match_sig/2, match_guard/2]).
-export([get_type/1, all_types/0]).
-export([snapshot/0]).
-export([forget/1, current_def_counter/0]).
-export([dict_find_op_in_tos/3, dict_find_op_in_any/3, dict_get_type/2,
         dict_all_types/1, dict_add_op/3, dict_register_type/2, dict_replace_ops/4,
         dict_op_clause_count/3]).

-export_type([type_constraint/0, stack_item/0]).

-type type_constraint() :: 'Any' | atom() | {atom(), term()}.
-type stack_item() :: {atom(), term()}.

%% The type registry uses ETS for mutable read/write. Each op is stamped with
%% a monotonic `defined_at` counter tracked in a second ETS table so that the
%% Forth-style `forget` word can rewind the dictionary back to a known point.
%%
%% A previous attempt stored the registry as a single `persistent_term` map.
%% That works semantically but exhausts the BEAM "literal heap" when the
%% compiler rewrites the map thousands of times during test runs: persistent_term
%% copies the whole value into a GC-collected literal area on every put. ETS
%% remains the right tool for this write-heavy path.
-define(TABLE,   af_type_registry).
-define(CTRTAB,  af_type_counter).

%%% API

init() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, set, public, {keypos, #af_type.name}]);
        _ -> ok
    end,
    case ets:info(?CTRTAB) of
        undefined ->
            ets:new(?CTRTAB, [named_table, set, public]),
            ets:insert(?CTRTAB, {counter, 0});
        _ -> ok
    end,
    ensure_type('Any'),
    ensure_type('Atom'),
    ensure_type('Ref'),
    ok.

reset() ->
    catch ets:delete(?TABLE),
    catch ets:delete(?CTRTAB),
    %% Reset the auto-compile flag too — tests that leave it enabled would
    %% otherwise cause subsequent tests to native-compile words unexpectedly.
    catch persistent_term:erase(af_auto_compile),
    af_repl:init_types().

register_type(#af_type{} = Type) ->
    ets:insert(?TABLE, Type),
    ok.

add_op(TypeName, #operation{} = Op) ->
    case ets:lookup(?TABLE, TypeName) of
        [#af_type{ops = Ops} = Type] ->
            StampedOp = stamp_op(Op),
            OpName = StampedOp#operation.name,
            Existing = maps:get(OpName, Ops, []),
            NewOps = maps:put(OpName, Existing ++ [StampedOp], Ops),
            ets:insert(?TABLE, Type#af_type{ops = NewOps}),
            ok;
        [] ->
            {error, {unknown_type, TypeName}}
    end.

%% Replace all operations with a given name in a type's dictionary.
%% Preserves `defined_at` from the existing ops so that native-wrapper
%% replacement does not disturb the monotonic ordering relied on by `forget`.
replace_ops(TypeName, OpName, NewOps) when is_list(NewOps) ->
    case ets:lookup(?TABLE, TypeName) of
        [#af_type{ops = Ops} = Type] ->
            ExistingList = maps:get(OpName, Ops, []),
            Preserved = preserve_defined_at(NewOps, ExistingList),
            UpdatedOps = maps:put(OpName, Preserved, Ops),
            ets:insert(?TABLE, Type#af_type{ops = UpdatedOps}),
            ok;
        [] ->
            {error, {unknown_type, TypeName}}
    end.

%% Core dispatch: find operation by token name against current stack.
%% Searches TOS type dictionary first, then falls back to Any.
find_op(TokenName, []) ->
    find_op_in_type('Any', TokenName, []);
find_op(TokenName, [TosItem | _] = Stack) ->
    case tos_type_of(TosItem) of
        {ok, TosType} ->
            case find_op_in_type(TosType, TokenName, Stack) of
                {ok, _Op} = Found -> Found;
                not_found -> find_op_in_type('Any', TokenName, Stack)
            end;
        none ->
            find_op_in_type('Any', TokenName, Stack)
    end.

%% Search ONLY in the TOS type's dictionary (no Any fallback).
find_op_in_tos(_TokenName, []) ->
    not_found;
find_op_in_tos(TokenName, [TosItem | _] = Stack) ->
    case tos_type_of(TosItem) of
        {ok, TosType} -> find_op_in_type(TosType, TokenName, Stack);
        none -> not_found
    end.

%% Extract the type atom from a stack item. Handles both plain tagged values
%% {Type, Val} and product instances {TypeName, V1, V2, ..., Vn}.
tos_type_of(Item) when is_tuple(Item), tuple_size(Item) >= 2 ->
    First = element(1, Item),
    case is_atom(First) of
        true -> {ok, First};
        false -> none
    end;
tos_type_of(_) -> none.

%% Search ONLY in the Any dictionary.
find_op_in_any(TokenName, Stack) ->
    find_op_in_type('Any', TokenName, Stack).

%% Find an operation by name in a type's dictionary (no signature matching).
%% Used during compilation where we just need to resolve names.
find_op_by_name(TokenName, TypeName) ->
    case ets:lookup(?TABLE, TypeName) of
        [#af_type{ops = Ops}] ->
            case maps:get(TokenName, Ops, []) of
                [Op | _] -> {ok, Op};
                [] -> not_found
            end;
        [] -> not_found
    end.

%% True if any type's dictionary has a registered op with this name.
%% Used by apply_impl to decide whether a "push as atom" fallback is
%% safe to cache (a later registration of the same name would make
%% the atom cache incorrect).
any_op_with_name(Name) ->
    Types = ets:tab2list(?TABLE),
    lists:any(fun(#af_type{ops = Ops}) ->
        maps:is_key(Name, Ops)
    end, Types).

%% Match a signature against the top of the stack.
%% Type variables ('_', '_a', '_b', etc.) and 'Any' match any stack item.
match_sig([], _Stack) -> true;
match_sig([_ | _], []) -> false;
match_sig(['Any' | SigRest], [_ | StackRest]) ->
    match_sig(SigRest, StackRest);
match_sig(['_' | SigRest], [_ | StackRest]) ->
    match_sig(SigRest, StackRest);
match_sig([{Type, Value} | SigRest], [StackItem | StackRest]) ->
    %% Value constraint: stack item must be {Type, Value} exactly. Works only
    %% on 2-tuple tagged values, not on product-type tuples (product types
    %% don't support value constraints).
    case StackItem of
        {Type, Value} -> match_sig(SigRest, StackRest);
        %% Checker-only: a stack item whose value is `undefined` means
        %% the type is known but the concrete value isn't (e.g. a fresh
        %% Bool produced by `==`). Accept it against any value
        %% constraint of the matching type — the runtime never produces
        %% undefined values so this branch is effectively checker-only.
        {Type, undefined} -> match_sig(SigRest, StackRest);
        _ -> false
    end;
match_sig([Type | SigRest], [StackItem | StackRest]) when is_atom(Type) ->
    StackType = case StackItem of
        Tuple when is_tuple(Tuple), tuple_size(Tuple) >= 2 -> element(1, Tuple);
        _ -> undefined
    end,
    case StackType of
        Type -> match_sig(SigRest, StackRest);
        %% 'Any' on the stack matches any declared type. The type checker
        %% produces Any as the result of operations whose static type
        %% depends on runtime data (e.g. `head` on a heterogeneous list),
        %% and the runtime never puts a bare 'Any' on the stack — so this
        %% branch is effectively checker-only, and it keeps static
        %% inference tracking through `over head` / `nth` / reduce-style
        %% combinators without false-negatives.
        'Any' -> match_sig(SigRest, StackRest);
        _ ->
            case af_type_check:is_type_variable(Type) of
                true -> match_sig(SigRest, StackRest);
                false -> false
            end
    end;
match_sig(_, _) -> false.

get_type(Name) ->
    case ets:lookup(?TABLE, Name) of
        [Type] -> {ok, Type};
        [] -> not_found
    end.

all_types() ->
    ets:tab2list(?TABLE).

%% Current monotonic counter value. Exposed so tests and tooling can snapshot
%% the dictionary state to roll back to.
current_def_counter() ->
    case ets:lookup(?CTRTAB, counter) of
        [{counter, N}] -> N;
        [] -> 0
    end.

%% Forth-style `forget`: remove every operation named OpName from every type
%% dictionary, together with every op defined at or after OpName's first
%% defined_at counter. Types themselves are not forgotten.
%%
%% Returns `ok` when at least one op was removed, `{error, not_found}` when
%% no op with that name exists.
forget(OpName) when is_list(OpName) ->
    case find_first_def_counter(OpName) of
        not_found -> {error, not_found};
        {ok, C} ->
            lists:foreach(fun(#af_type{ops = Ops} = Type) ->
                FilteredOps = maps:map(fun(_K, OpList) ->
                    [Op || Op <- OpList, keep_op(Op, C)]
                end, Ops),
                CleanedOps = maps:filter(fun(_K, V) -> V =/= [] end,
                                         FilteredOps),
                ets:insert(?TABLE, Type#af_type{ops = CleanedOps})
            end, ets:tab2list(?TABLE)),
            %% Roll the counter back so subsequent definitions reuse slots.
            case ets:lookup(?CTRTAB, counter) of
                [{counter, _}] -> ets:insert(?CTRTAB, {counter, C - 1});
                [] -> ok
            end,
            ok
    end;
forget(OpName) when is_atom(OpName) ->
    forget(atom_to_list(OpName));
forget(OpName) when is_binary(OpName) ->
    forget(binary_to_list(OpName)).

keep_op(#operation{defined_at = undefined}, _Cutoff) -> true;
keep_op(#operation{defined_at = At}, Cutoff) -> At < Cutoff.

%%% Internal

ensure_type(Name) ->
    case ets:lookup(?TABLE, Name) of
        [] -> ets:insert(?TABLE, #af_type{name = Name});
        _ -> ok
    end.

find_op_in_type(TypeName, OpName, Stack) ->
    case ets:lookup(?TABLE, TypeName) of
        [#af_type{ops = Ops}] ->
            case maps:get(OpName, Ops, []) of
                [] -> not_found;
                OpList -> match_first_op(OpList, Stack)
            end;
        [] -> not_found
    end.

match_first_op([], _Stack) -> not_found;
match_first_op([Op | Rest], Stack) ->
    case match_sig(Op#operation.sig_in, Stack) of
        true ->
            case match_guard(Op, Stack) of
                true -> {ok, Op};
                false -> match_first_op(Rest, Stack)
            end;
        false -> match_first_op(Rest, Stack)
    end.

%% Evaluate an operation's guard (if any) on a snapshot of the stack.
%% The guard is a list of #token{} records run as a small A4 program against
%% the current data stack. It must leave {Bool, true} on top to match.
%% An operation with no guard always matches (returns true).
match_guard(#operation{guard = undefined}, _Stack) -> true;
match_guard(#operation{guard = []}, _Stack) -> true;
match_guard(#operation{guard = GuardTokens}, Stack) ->
    TempCont = #continuation{data_stack = Stack},
    try af_interpreter:interpret_tokens(GuardTokens, TempCont) of
        #continuation{data_stack = [{'Bool', true} | _]} -> true;
        _ -> false
    catch _:_ -> false
    end.

%% Stamp an op with the next monotonic counter. An op that already carries a
%% counter (e.g. one carried across replace_ops) keeps it unchanged.
stamp_op(#operation{defined_at = undefined} = Op) ->
    Op#operation{defined_at = next_def_counter()};
stamp_op(Op) -> Op.

%% Allocate the next monotonic counter. ets:update_counter is atomic.
next_def_counter() ->
    ets:update_counter(?CTRTAB, counter, 1).

%% Carry defined_at from existing ops onto new ones that lack it.
preserve_defined_at(NewOps, ExistingOps) ->
    ExistingCounters = [O#operation.defined_at || O <- ExistingOps,
                        O#operation.defined_at =/= undefined],
    {Out, _} = lists:mapfoldl(fun
        (Op, Acc) when Op#operation.defined_at =/= undefined ->
            {Op, Acc};
        (Op, [H | T]) ->
            {Op#operation{defined_at = H}, T};
        (Op, []) ->
            {Op#operation{defined_at = next_def_counter()}, []}
    end, ExistingCounters, NewOps),
    Out.

%% Locate the smallest defined_at counter associated with any op named OpName.
find_first_def_counter(OpName) ->
    Candidates = lists:foldl(fun(#af_type{ops = Ops}, Acc) ->
        case maps:get(OpName, Ops, []) of
            [] -> Acc;
            OpList ->
                [Op#operation.defined_at
                 || Op <- OpList, Op#operation.defined_at =/= undefined]
                ++ Acc
        end
    end, [], ets:tab2list(?TABLE)),
    case Candidates of
        [] -> not_found;
        _ -> {ok, lists:min(Candidates)}
    end.

%%% ============================================================
%%% Dictionary-local functions (operate on a map, not ETS)
%%% ============================================================

%% Snapshot the entire ETS registry into a map: #{TypeName => #af_type{}}
-spec snapshot() -> map().
snapshot() ->
    lists:foldl(fun(#af_type{name = Name} = Type, Acc) ->
        maps:put(Name, Type, Acc)
    end, #{}, ets:tab2list(?TABLE)).

%% Look up op in TOS type's dictionary (local map, no ETS).
%% Supports both plain tagged values and product-type tuple instances.
dict_find_op_in_tos(_TokenName, [], _Dict) ->
    not_found;
dict_find_op_in_tos(TokenName, [TosItem | _] = Stack, Dict) ->
    case tos_type_of(TosItem) of
        {ok, TosType} -> dict_find_op_in_type(TosType, TokenName, Stack, Dict);
        none -> not_found
    end.

%% Look up op in Any dictionary (local map, no ETS)
dict_find_op_in_any(TokenName, Stack, Dict) ->
    dict_find_op_in_type('Any', TokenName, Stack, Dict).

%% Get a type record from the local dictionary
dict_get_type(Name, Dict) ->
    case maps:find(Name, Dict) of
        {ok, Type} -> {ok, Type};
        error -> not_found
    end.

%% Get all types from the local dictionary
dict_all_types(Dict) ->
    maps:values(Dict).

%% Register a type in the local dictionary (returns updated Dict)
dict_register_type(#af_type{name = Name} = Type, Dict) ->
    maps:put(Name, Type, Dict).

%% Add an operation to a type in the local dictionary (returns updated Dict)
dict_add_op(TypeName, #operation{} = Op, Dict) ->
    Type = case maps:find(TypeName, Dict) of
        {ok, T} -> T;
        error -> #af_type{name = TypeName}
    end,
    #af_type{ops = Ops} = Type,
    OpName = Op#operation.name,
    Existing = maps:get(OpName, Ops, []),
    NewOps = maps:put(OpName, Existing ++ [Op], Ops),
    maps:put(TypeName, Type#af_type{ops = NewOps}, Dict).

%% Replace all operations with a given name in a type (returns updated Dict)
dict_replace_ops(TypeName, OpName, NewOps, Dict) when is_list(NewOps) ->
    case maps:find(TypeName, Dict) of
        {ok, #af_type{ops = Ops} = Type} ->
            UpdatedOps = maps:put(OpName, NewOps, Ops),
            maps:put(TypeName, Type#af_type{ops = UpdatedOps}, Dict);
        error ->
            Dict
    end.

%% Count how many clauses exist under a given op name in a type (or Any).
%% Used by the interpreter to decide whether an op is safe to cache: a
%% single-clause op with plain types is cacheable; overloaded or value-
%% constrained ops must re-dispatch.
dict_op_clause_count(TypeName, OpName, Dict) ->
    case maps:find(TypeName, Dict) of
        {ok, #af_type{ops = Ops}} ->
            length(maps:get(OpName, Ops, []));
        error -> 0
    end.

%%% Internal (dictionary-local)

dict_find_op_in_type(TypeName, OpName, Stack, Dict) ->
    case maps:find(TypeName, Dict) of
        {ok, #af_type{ops = Ops}} ->
            case maps:get(OpName, Ops, []) of
                [] -> not_found;
                OpList -> match_first_op(OpList, Stack)
            end;
        error -> not_found
    end.
