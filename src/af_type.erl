-module(af_type).

-include("operation.hrl").
-include("af_type.hrl").

-export([init/0, reset/0]).
-export([register_type/1, add_op/2, replace_ops/3]).
-export([find_op/2, find_op_in_tos/2, find_op_in_any/2, find_op_by_name/2, match_sig/2]).
-export([get_type/1, all_types/0]).

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
    init().

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
