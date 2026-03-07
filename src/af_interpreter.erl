-module(af_interpreter).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([interpret_tokens/2, interpret_token/2, new_continuation/0]).

-spec new_continuation() -> #continuation{}.
new_continuation() ->
    #continuation{}.

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
                            case Token#token.quoted of
                                true ->
                                    StringVal = {'String', list_to_binary(Value)},
                                    debug_trace(Debug, Value, Stack, {literal, StringVal}),
                                    Cont1#continuation{data_stack = [StringVal | Stack]};
                                false ->
                                    case try_literals(Value) of
                                        {ok, TypedValue} ->
                                            debug_trace(Debug, Value, Stack, {literal, TypedValue}),
                                            Cont1#continuation{data_stack = [TypedValue | Stack]};
                                        not_found ->
                                            debug_trace(Debug, Value, Stack, atom),
                                            Cont1#continuation{data_stack = [{'Atom', Value} | Stack]}
                                    end
                            end
                    end
            end
    end.

%%% Literal handlers

try_literals(TokenValue) ->
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
get_tos_handler([{TosType, _} | _]) ->
    case af_type:get_type(TosType) of
        {ok, #af_type{handler = Handler}} when Handler =/= undefined ->
            {ok, Handler};
        _ -> none
    end.
