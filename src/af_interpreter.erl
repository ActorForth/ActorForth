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
%% 4. If still not found, push as Atom
-spec interpret_token(#token{}, #continuation{}) -> #continuation{}.
interpret_token(#token{value = Value} = Token, Cont) ->
    Stack = Cont#continuation.data_stack,
    Cont1 = Cont#continuation{current_token = Token},
    case af_type:find_op_in_tos(Value, Stack) of
        {ok, #operation{impl = Impl}} ->
            Impl(Cont1);
        not_found ->
            case get_tos_handler(Stack) of
                {ok, Handler} ->
                    Handler(Value, Cont1);
                none ->
                    case af_type:find_op_in_any(Value, Stack) of
                        {ok, #operation{impl = Impl}} ->
                            Impl(Cont1);
                        not_found ->
                            Cont1#continuation{data_stack = [{'Atom', Value} | Stack]}
                    end
            end
    end.

%%% Internal

get_tos_handler([]) -> none;
get_tos_handler([{TosType, _} | _]) ->
    case af_type:get_type(TosType) of
        {ok, #af_type{handler = Handler}} when Handler =/= undefined ->
            {ok, Handler};
        _ -> none
    end.
