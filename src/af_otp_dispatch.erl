-module(af_otp_dispatch).

-include("token.hrl").
-include("continuation.hrl").

-export([call_word/3, cast_word/3]).

%% Dispatch an ActorForth word as a gen_server call.
%% State is a tagged ActorForth stack item {TypeName, Value}.
%% Args are raw Erlang values to push before the word.
%% Returns {ReturnValues, NewState}.
-spec call_word(string(), [term()], term()) -> {[term()], term()}.
call_word(WordName, Args, State) ->
    ensure_types(),
    ArgItems = [af_term:to_stack_item(A) || A <- Args],
    %% Build stack: state on bottom, args on top (TOS-first)
    Stack = lists:reverse(ArgItems) ++ [State],
    Cont = #continuation{data_stack = Stack},
    Token = #token{value = WordName},
    ResultCont = af_interpreter:interpret_token(Token, Cont),
    ResultStack = ResultCont#continuation.data_stack,
    %% The state is the bottom item, everything above is return values
    case ResultStack of
        [] -> {[], State};
        [SingleItem] -> {[], SingleItem};
        _ ->
            {ReturnItems, [NewState]} = lists:split(length(ResultStack) - 1, ResultStack),
            ReturnValues = [af_term:from_stack_item(R) || R <- ReturnItems],
            {ReturnValues, NewState}
    end.

%% Dispatch an ActorForth word as a gen_server cast (state-only).
%% Returns the new state (as tagged item).
-spec cast_word(string(), [term()], term()) -> term().
cast_word(WordName, Args, State) ->
    {_Returns, NewState} = call_word(WordName, Args, State),
    NewState.

%% Ensure types are initialized (idempotent).
ensure_types() ->
    case ets:info(af_type_registry) of
        undefined -> af_repl:init_types();
        _ -> ok
    end.
