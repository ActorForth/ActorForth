-module(af_type).

-include("operation.hrl").
-include("af_type.hrl").

-export([init/0, reset/0]).
-export([register_type/1, add_op/2, replace_ops/3]).
-export([find_op/2, find_op_in_tos/2, find_op_in_any/2, find_op_by_name/2, match_sig/2]).
-export([get_type/1, all_types/0]).
-export([snapshot/0]).
-export([dict_find_op_in_tos/3, dict_find_op_in_any/3, dict_get_type/2,
         dict_all_types/1, dict_add_op/3, dict_register_type/2, dict_replace_ops/4]).

-export_type([type_constraint/0, stack_item/0]).

-type type_constraint() :: 'Any' | atom() | {atom(), term()}.
-type stack_item() :: {atom(), term()}.

-define(TABLE, af_type_registry).

%%% API

init() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, set, public, {keypos, #af_type.name}]);
        _ -> ok
    end,
    ensure_type('Any'),
    ensure_type('Atom'),
    ok.

reset() ->
    catch ets:delete(?TABLE),
    af_repl:init_types().

register_type(#af_type{} = Type) ->
    ets:insert(?TABLE, Type),
    ok.

add_op(TypeName, #operation{} = Op) ->
    case ets:lookup(?TABLE, TypeName) of
        [#af_type{ops = Ops} = Type] ->
            OpName = Op#operation.name,
            Existing = maps:get(OpName, Ops, []),
            NewOps = maps:put(OpName, Existing ++ [Op], Ops),
            ets:insert(?TABLE, Type#af_type{ops = NewOps}),
            ok;
        [] ->
            {error, {unknown_type, TypeName}}
    end.

%% Replace all operations with a given name in a type's dictionary.
replace_ops(TypeName, OpName, NewOps) when is_list(NewOps) ->
    case ets:lookup(?TABLE, TypeName) of
        [#af_type{ops = Ops} = Type] ->
            UpdatedOps = maps:put(OpName, NewOps, Ops),
            ets:insert(?TABLE, Type#af_type{ops = UpdatedOps}),
            ok;
        [] ->
            {error, {unknown_type, TypeName}}
    end.

%% Core dispatch: find operation by token name against current stack.
%% Searches TOS type dictionary first, then falls back to Any.
find_op(TokenName, [{TosType, _} | _] = Stack) ->
    case find_op_in_type(TosType, TokenName, Stack) of
        {ok, _Op} = Found -> Found;
        not_found -> find_op_in_type('Any', TokenName, Stack)
    end;
find_op(TokenName, []) ->
    find_op_in_type('Any', TokenName, []).

%% Search ONLY in the TOS type's dictionary (no Any fallback).
find_op_in_tos(TokenName, [{TosType, _} | _] = Stack) ->
    find_op_in_type(TosType, TokenName, Stack);
find_op_in_tos(_TokenName, []) ->
    not_found.

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

%% Match a signature against the top of the stack.
%% Type variables ('_', '_a', '_b', etc.) and 'Any' match any stack item.
match_sig([], _Stack) -> true;
match_sig([_ | _], []) -> false;
match_sig(['Any' | SigRest], [_ | StackRest]) ->
    match_sig(SigRest, StackRest);
match_sig(['_' | SigRest], [_ | StackRest]) ->
    match_sig(SigRest, StackRest);
match_sig([{Type, Value} | SigRest], [{Type, Value} | StackRest]) ->
    match_sig(SigRest, StackRest);
match_sig([Type | SigRest], [{Type, _} | StackRest]) when is_atom(Type) ->
    case af_type_check:is_type_variable(Type) of
        true ->
            %% Type variable — already matched by name coincidence, but
            %% should match ANY type. Re-do: this clause matched because
            %% the stack item's type atom happened to equal the variable name.
            %% That's fine — it matches. Continue.
            match_sig(SigRest, StackRest);
        false ->
            match_sig(SigRest, StackRest)
    end;
match_sig([Type | SigRest], [_ | StackRest]) when is_atom(Type) ->
    %% Type didn't match stack item's type — check if it's a type variable
    case af_type_check:is_type_variable(Type) of
        true -> match_sig(SigRest, StackRest);
        false -> false
    end;
match_sig(_, _) -> false.

get_type(Name) ->
    case ets:lookup(?TABLE, Name) of
        [Type] -> {ok, Type};
        [] -> not_found
    end.

all_types() ->
    ets:tab2list(?TABLE).

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
        true -> {ok, Op};
        false -> match_first_op(Rest, Stack)
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

%% Look up op in TOS type's dictionary (local map, no ETS)
dict_find_op_in_tos(TokenName, [{TosType, _} | _] = Stack, Dict) ->
    dict_find_op_in_type(TosType, TokenName, Stack, Dict);
dict_find_op_in_tos(_TokenName, [], _Dict) ->
    not_found.

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
