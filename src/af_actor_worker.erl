-module(af_actor_worker).
-behaviour(gen_server).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([build_native_cache/1]).

-record(state, {
    type_name :: atom(),
    instance  :: {atom(), term()},
    %% Cache: #{WordName::string() => fun([Stack]) -> [NewStack]}
    native_cache :: map()
}).

start_link(TypeName, StateInstance) ->
    gen_server:start_link(?MODULE, {TypeName, StateInstance}, []).

init({TypeName, StateInstance}) ->
    Cache = build_native_cache(TypeName),
    {ok, #state{type_name = TypeName, instance = StateInstance,
                native_cache = Cache}}.

handle_cast({cast, "stop", _Args}, State) ->
    {stop, normal, State};

handle_cast({cast, WordName, Args}, #state{instance = Instance,
                                            native_cache = Cache} = State) ->
    try
        NewInstance = case maps:find(WordName, Cache) of
            {ok, {Mod, Fun}} ->
                case Args of
                    [] -> hd(Mod:Fun([Instance]));
                    _ -> lists:last(Mod:Fun(Args ++ [Instance]))
                end;
            error ->
                interpret_cast(WordName, Args, Instance)
        end,
        {noreply, State#state{instance = NewInstance}}
    catch _:_ ->
        {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({call, WordName, Args}, _From,
            #state{type_name = TypeName, instance = Instance,
                   native_cache = Cache} = State) ->
    try
        {ReplyValues, NewInstance} = case maps:find(WordName, Cache) of
            {ok, {Mod, Fun}} ->
                ResultStack = case Args of
                    [] -> Mod:Fun([Instance]);
                    _ -> Mod:Fun(Args ++ [Instance])
                end,
                separate_reply(TypeName, ResultStack);
            error ->
                interpret_call(WordName, Args, TypeName, Instance)
        end,
        {reply, {ok, ReplyValues}, State#state{instance = NewInstance}}
    catch
        _:Reason ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%% Internal

build_native_cache(TypeName) ->
    case catch af_type:get_type(TypeName) of
        {ok, #af_type{ops = Ops}} ->
            maps:fold(fun(Name, OpList, Acc) ->
                case find_native_fun(OpList) of
                    {ok, Fun} -> maps:put(Name, Fun, Acc);
                    not_found -> Acc
                end
            end, #{}, Ops);
        _ -> #{}
    end.

find_native_fun([]) -> not_found;
find_native_fun([#operation{source = {native, Mod}, name = Name} | _]) ->
    FunAtom = list_to_atom(Name),
    {ok, {Mod, FunAtom}};
find_native_fun([_ | Rest]) ->
    find_native_fun(Rest).

interpret_cast(WordName, Args, StateInstance) ->
    Stack = Args ++ [StateInstance],
    Cont = #continuation{data_stack = Stack},
    Token = #token{value = WordName},
    ResultCont = af_interpreter:interpret_token(Token, Cont),
    lists:last(ResultCont#continuation.data_stack).

interpret_call(WordName, Args, TypeName, StateInstance) ->
    Stack = Args ++ [StateInstance],
    Cont = #continuation{data_stack = Stack},
    Token = #token{value = WordName},
    ResultCont = af_interpreter:interpret_token(Token, Cont),
    separate_reply(TypeName, ResultCont#continuation.data_stack).

separate_reply(TypeName, Stack) ->
    separate_reply(TypeName, Stack, []).
separate_reply(TypeName, [{TypeName, _} = Instance | _Rest], ReplyAcc) ->
    {lists:reverse(ReplyAcc), Instance};
separate_reply(TypeName, [Item | Rest], ReplyAcc) ->
    separate_reply(TypeName, Rest, [Item | ReplyAcc]);
separate_reply(_TypeName, [], ReplyAcc) ->
    {lists:reverse(ReplyAcc), undefined}.
