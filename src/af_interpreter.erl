-module(af_interpreter).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([interpret_tokens/2, interpret_token/2, new_continuation/0]).

-spec new_continuation() -> #continuation{}.
new_continuation() ->
    #continuation{dictionary = af_type:snapshot()}.

-spec interpret_tokens([#token{}], #continuation{}) -> #continuation{}.
interpret_tokens([], Cont) ->
    Cont;
interpret_tokens([Token | Rest], Cont) ->
    NewCont = interpret_token(Token, Cont),
    interpret_tokens(Rest, NewCont).

%% The outer interpreter. For each token:
%% 1. Search TOS type's dictionary
%% 2. If not found, check TOS type's handler (compiler states intercept here)
%% 3. If no handler, search Any dictionary
%% 4. If still not found, try literal handlers (Int, Bool, etc.)
%% 5. If still not found, push as Atom
-spec interpret_token(#token{}, #continuation{}) -> #continuation{}.
interpret_token(#token{value = Value} = Token, Cont) ->
    Stack = Cont#continuation.data_stack,
    Debug = Cont#continuation.debug,
    Cont1 = Cont#continuation{current_token = Token},
    case Cont#continuation.dictionary of
        undefined ->
            %% No local dictionary — fall back to ETS (legacy path)
            interpret_token_ets(Value, Token, Stack, Debug, Cont1);
        Dict ->
            %% Fast path: use continuation-local dictionary
            interpret_token_dict(Value, Token, Stack, Dict, Debug, Cont1)
    end.

%% Fast path: local dictionary lookup (no ETS)
interpret_token_dict(Value, Token, Stack, Dict, Debug, Cont1) ->
    case Token#token.quoted of
        true ->
            %% Quoted strings skip dictionary lookups (steps 1 & 3)
            %% but still go through handlers (step 2) so compiler can
            %% compile them into word bodies as string literal pushes.
            case get_tos_handler(Stack, Dict) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Handler(Value, Cont1);
                none ->
                    StringVal = {'String', list_to_binary(Value)},
                    debug_trace(Debug, Value, Stack, {literal, StringVal}),
                    Cont1#continuation{data_stack = [StringVal | Stack]}
            end;
        false ->
            dispatch_unquoted(Value, Stack, Dict, Debug, Cont1)
    end.

%% Unquoted dispatch with a monomorphic inline cache keyed on the top-3 stack
%% types plus the token value. The 3-deep key is enough to make dispatch
%% deterministic for every op in the codebase (the deepest sig_in is 4, and
%% we skip caching entries that would resolve against that one op).
%%
%% Cache is invalidated by clearing `dispatch_cache` whenever the continuation
%% dictionary is updated (word definition, compile, type definition).
dispatch_unquoted(Value, Stack, Dict, Debug, Cont1) ->
    Key = stack_key(Value, Stack),
    Cache = Cont1#continuation.dispatch_cache,
    case maps:find(Key, Cache) of
        {ok, {op, Impl}} ->
            debug_trace(Debug, Value, Stack, cache_op),
            Impl(Cont1);
        {ok, {literal, TypedValue}} ->
            debug_trace(Debug, Value, Stack, {literal, TypedValue}),
            Cont1#continuation{data_stack = [TypedValue | Stack]};
        {ok, atom} ->
            debug_trace(Debug, Value, Stack, atom),
            Cont1#continuation{data_stack = [{'Atom', Value} | Stack]};
        error ->
            full_dispatch_and_cache(Value, Stack, Dict, Debug, Cont1, Key)
    end.

full_dispatch_and_cache(Value, Stack, Dict, Debug, Cont1, Key) ->
    TosType = case Key of {_, T0, _, _} -> T0 end,
    case af_type:dict_find_op_in_tos(Value, Stack, Dict) of
        {ok, #operation{impl = Impl, sig_in = SigIn, guard = Guard} = Op} ->
            debug_trace(Debug, Value, Stack, {tos, Op}),
            ClauseCount = af_type:dict_op_clause_count(TosType, Value, Dict),
            maybe_cache(Key, SigIn, Guard, ClauseCount, Cont1, {op, Impl}, Impl(Cont1));
        not_found ->
            case get_tos_handler(Stack, Dict) of
                {ok, Handler} ->
                    %% Handler dispatch is stateful — never cache.
                    debug_trace(Debug, Value, Stack, handler),
                    Handler(Value, Cont1);
                none ->
                    case af_type:dict_find_op_in_any(Value, Stack, Dict) of
                        {ok, #operation{impl = Impl, sig_in = SigIn, guard = Guard} = Op} ->
                            debug_trace(Debug, Value, Stack, {any, Op}),
                            AnyCount = af_type:dict_op_clause_count('Any', Value, Dict),
                            maybe_cache(Key, SigIn, Guard, AnyCount, Cont1, {op, Impl}, Impl(Cont1));
                        not_found ->
                            case try_literals(Value, Dict) of
                                {ok, TypedValue} ->
                                    debug_trace(Debug, Value, Stack, {literal, TypedValue}),
                                    Result = Cont1#continuation{data_stack = [TypedValue | Stack]},
                                    maybe_cache(Key, [], undefined, 1, Cont1, {literal, TypedValue}, Result);
                                not_found ->
                                    debug_trace(Debug, Value, Stack, atom),
                                    Result = Cont1#continuation{data_stack = [{'Atom', Value} | Stack]},
                                    maybe_cache_atom(Key, Value, Cont1, Result)
                            end
                    end
            end
    end.

%% Compute a cache key from Token value and top-3 stack types.
%% Positions below the stack depth use 'none'. Non-atom-tagged stack items
%% force the whole key to be skipped (we return a sentinel that never hits
%% the cache) so legacy raw stack items don't corrupt the cache.
stack_key(Value, [A, B, C | _]) ->
    {Value, type_tag(A), type_tag(B), type_tag(C)};
stack_key(Value, [A, B]) ->
    {Value, type_tag(A), type_tag(B), none};
stack_key(Value, [A]) ->
    {Value, type_tag(A), none, none};
stack_key(Value, []) ->
    {Value, none, none, none}.

type_tag(Item) when is_tuple(Item), tuple_size(Item) >= 2 ->
    First = element(1, Item),
    case is_atom(First) of
        true -> First;
        false -> '$uncacheable'
    end;
type_tag(_) -> '$uncacheable'.

%% Cache a dispatch result only when it's safe to replay:
%%  - TOS type has no handler (dispatch is state-dependent otherwise)
%%  - sig_in length <= 3 (the key-depth, so stack shape fully determines match)
%%  - no value constraints in sig_in (tuple entries trigger different clauses)
%%  - no guard expression (runtime-checked, not key-determined)
%%  - only one clause under this name (multi-clause dispatch picks by stack
%%    value, our key captures only types)
maybe_cache(Key, SigIn, Guard, ClauseCount, CallCont, CacheEntry, Result) ->
    case should_cache(Key, SigIn, Guard, ClauseCount, CallCont) of
        false -> Result;
        true ->
            OldCache = CallCont#continuation.dispatch_cache,
            ResCache = Result#continuation.dispatch_cache,
            %% If the callee swapped out the cache (e.g. word definition
            %% cleared it), respect that — otherwise merge our new entry in.
            MergedCache = case ResCache =:= OldCache orelse map_size(ResCache) >= map_size(OldCache) of
                true  -> maps:put(Key, CacheEntry, ResCache);
                false -> ResCache
            end,
            Result#continuation{dispatch_cache = MergedCache}
    end.

%% Atom cache: safe only if NO op with this name exists in the dictionary.
%% If any op exists, the atom fallback was the fallthrough for THIS specific
%% stack shape; a different stack with the same 3-deep key could match it.
maybe_cache_atom(Key, Value, CallCont, Result) ->
    Dict = CallCont#continuation.dictionary,
    case any_op_named(Value, Dict) of
        true  -> Result;
        false -> maybe_cache(Key, [], undefined, 1, CallCont, atom, Result)
    end.

should_cache({_, '$uncacheable', _, _}, _, _, _, _) -> false;
should_cache({_, _, '$uncacheable', _}, _, _, _, _) -> false;
should_cache({_, _, _, '$uncacheable'}, _, _, _, _) -> false;
should_cache(_Key, _SigIn, Guard, _ClauseCount, _Cont) when Guard =/= undefined ->
    false;
should_cache(_Key, _SigIn, _Guard, ClauseCount, _Cont) when ClauseCount > 1 ->
    false;
should_cache({_, T0, _, _}, SigIn, _Guard, _ClauseCount, Cont) ->
    length(SigIn) =< 3
        andalso not has_value_constraint(SigIn)
        andalso not tos_has_handler(T0, Cont#continuation.dictionary).

has_value_constraint([]) -> false;
has_value_constraint([{_, _} | _]) -> true;
has_value_constraint([_ | Rest]) -> has_value_constraint(Rest).

any_op_named(Name, Dict) when is_map(Dict) ->
    maps:fold(fun(_, #af_type{ops = Ops}, Acc) ->
        Acc orelse maps:is_key(Name, Ops)
    end, false, Dict);
any_op_named(_, _) -> false.

tos_has_handler(none, _Dict) -> false;
tos_has_handler(TosType, Dict) ->
    case af_type:dict_get_type(TosType, Dict) of
        {ok, #af_type{handler = Handler}} when Handler =/= undefined -> true;
        _ -> false
    end.

%% Legacy path: ETS-based lookup (for continuations without dictionary)
interpret_token_ets(Value, Token, Stack, Debug, Cont1) ->
    case Token#token.quoted of
        true ->
            %% Quoted strings skip dictionary lookups (steps 1 & 3)
            %% but still go through handlers (step 2) so compiler can
            %% compile them into word bodies as string literal pushes.
            case get_tos_handler(Stack) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Handler(Value, Cont1);
                none ->
                    StringVal = {'String', list_to_binary(Value)},
                    debug_trace(Debug, Value, Stack, {literal, StringVal}),
                    Cont1#continuation{data_stack = [StringVal | Stack]}
            end;
        false ->
            interpret_unquoted(Value, Stack, Debug, Cont1)
    end.

interpret_unquoted(Value, Stack, Debug, Cont1) ->
    case af_type:find_op_in_tos(Value, Stack) of
        {ok, #operation{impl = Impl} = Op} ->
            debug_trace(Debug, Value, Stack, {tos, Op}),
            Impl(Cont1);
        not_found ->
            case get_tos_handler(Stack) of
                {ok, Handler} ->
                    debug_trace(Debug, Value, Stack, handler),
                    Handler(Value, Cont1);
                none ->
                    case af_type:find_op_in_any(Value, Stack) of
                        {ok, #operation{impl = Impl} = Op} ->
                            debug_trace(Debug, Value, Stack, {any, Op}),
                            Impl(Cont1);
                        not_found ->
                            case try_literals_ets(Value) of
                                {ok, TypedValue} ->
                                    debug_trace(Debug, Value, Stack, {literal, TypedValue}),
                                    Cont1#continuation{data_stack = [TypedValue | Stack]};
                                not_found ->
                                    debug_trace(Debug, Value, Stack, atom),
                                    Cont1#continuation{data_stack = [{'Atom', Value} | Stack]}
                            end
                    end
            end
    end.

%%% Literal handlers

try_literals(TokenValue, Dict) ->
    AllTypes = af_type:dict_all_types(Dict),
    try_literal_types(TokenValue, AllTypes).

try_literals_ets(TokenValue) ->
    AllTypes = af_type:all_types(),
    try_literal_types(TokenValue, AllTypes).

try_literal_types(_TokenValue, []) -> not_found;
try_literal_types(TokenValue, [#af_type{ops = Ops} | Rest]) ->
    case maps:get("literal", Ops, []) of
        [] -> try_literal_types(TokenValue, Rest);
        [#operation{impl = Impl} | _] ->
            TempCont = #continuation{data_stack = [{'Atom', TokenValue}]},
            try
                ResultCont = Impl(TempCont),
                [TypedValue | _] = ResultCont#continuation.data_stack,
                {ok, TypedValue}
            catch _:_ ->
                try_literal_types(TokenValue, Rest)
            end
    end.

%%% Debug trace

debug_trace(false, _, _, _) -> ok;
debug_trace(true, Value, Stack, Dispatch) ->
    StackStr = format_stack(Stack),
    DispatchStr = format_dispatch(Dispatch),
    io:format("[~s] ~s -> ~s~n", [StackStr, Value, DispatchStr]).

format_stack([]) -> "";
format_stack(Stack) ->
    Items = lists:map(fun({Type, Val}) ->
        io_lib:format("~p(~p)", [Type, Val])
    end, Stack),
    lists:join(" ", Items).

format_dispatch({tos, #operation{name = Name}}) ->
    TosInfo = io_lib:format("TOS.~s", [Name]),
    lists:flatten(TosInfo);
format_dispatch(cache_op) -> "cache";
format_dispatch(handler) -> "handler";
format_dispatch({any, #operation{name = Name}}) ->
    AnyInfo = io_lib:format("Any.~s", [Name]),
    lists:flatten(AnyInfo);
format_dispatch({literal, {Type, _Val}}) ->
    LitInfo = io_lib:format("literal->~p", [Type]),
    lists:flatten(LitInfo);
format_dispatch(atom) -> "->Atom".

%%% Internal

get_tos_handler([]) -> none;
get_tos_handler([TosItem | _]) when is_tuple(TosItem), tuple_size(TosItem) >= 2 ->
    TosType = element(1, TosItem),
    case is_atom(TosType) of
        true ->
            case af_type:get_type(TosType) of
                {ok, #af_type{handler = Handler}} when Handler =/= undefined ->
                    {ok, Handler};
                _ -> none
            end;
        false -> none
    end;
get_tos_handler(_) -> none.

get_tos_handler([], _Dict) -> none;
get_tos_handler([TosItem | _], Dict) when is_tuple(TosItem), tuple_size(TosItem) >= 2 ->
    TosType = element(1, TosItem),
    case is_atom(TosType) of
        true ->
            case af_type:dict_get_type(TosType, Dict) of
                {ok, #af_type{handler = Handler}} when Handler =/= undefined ->
                    {ok, Handler};
                _ -> none
            end;
        false -> none
    end;
get_tos_handler(_, _) -> none.
