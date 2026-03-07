-module(af_actor_worker).
-behaviour(gen_server).

-include("token.hrl").
-include("continuation.hrl").

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    type_name :: atom(),
    instance  :: {atom(), term()}
}).

start_link(TypeName, StateInstance) ->
    gen_server:start_link(?MODULE, {TypeName, StateInstance}, []).

init({TypeName, StateInstance}) ->
    {ok, #state{type_name = TypeName, instance = StateInstance}}.

handle_call({call, WordName, Args}, _From, State) ->
    #state{type_name = TypeName, instance = Instance} = State,
    try
        {ReplyValues, NewInstance} = execute_call(WordName, Args, TypeName, Instance),
        NewState = State#state{instance = NewInstance},
        {reply, {ok, ReplyValues}, NewState}
    catch
        _:Reason ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({cast, "stop", _Args}, State) ->
    {stop, normal, State};

handle_cast({cast, WordName, Args}, State) ->
    #state{type_name = TypeName, instance = Instance} = State,
    try
        NewInstance = execute_cast(WordName, Args, TypeName, Instance),
        {noreply, State#state{instance = NewInstance}}
    catch
        _:_Reason ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%% Internal

execute_cast(WordName, Args, _TypeName, StateInstance) ->
    Stack = Args ++ [StateInstance],
    Cont = #continuation{data_stack = Stack},
    Token = #token{value = WordName},
    ResultCont = af_interpreter:interpret_token(Token, Cont),
    ResultStack = ResultCont#continuation.data_stack,
    lists:last(ResultStack).

execute_call(WordName, Args, TypeName, StateInstance) ->
    Stack = Args ++ [StateInstance],
    Cont = #continuation{data_stack = Stack},
    Token = #token{value = WordName},
    ResultCont = af_interpreter:interpret_token(Token, Cont),
    ResultStack = ResultCont#continuation.data_stack,
    separate_reply(TypeName, ResultStack).

separate_reply(TypeName, Stack) ->
    separate_reply(TypeName, Stack, []).

separate_reply(TypeName, [{TypeName, _} = Instance | _Rest], ReplyAcc) ->
    {lists:reverse(ReplyAcc), Instance};
separate_reply(TypeName, [Item | Rest], ReplyAcc) ->
    separate_reply(TypeName, Rest, [Item | ReplyAcc]);
separate_reply(_TypeName, [], ReplyAcc) ->
    {lists:reverse(ReplyAcc), undefined}.
