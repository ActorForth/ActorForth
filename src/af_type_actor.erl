-module(af_type_actor).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

%% Actor model:
%%   Any type instance can become an actor via 'server'.
%%   server   — ( TypeInstance -- Actor )  spawn actor from type instance
%%   <<       — ( Actor -- ActorSend )    enter actor-send mode
%%   .        — ( ActorSend -- Actor )    finish send block
%%   stop     — sent via << stop . to terminate actor
%%
%% Inside << ... . :
%%   - Non-actor-vocab tokens execute locally on a temp stack
%%   - Actor-vocab tokens pop matching args from temp stack,
%%     package as message, dispatch to actor
%%   - Cast (no return beyond state type) = async
%%   - Call (returns beyond state type) = sync, reply pushed to temp stack
%%
%% Actor state is private. Auto-generated accessors (getters/setters)
%% are NOT callable through <<. Only user-defined words form the
%% remote interface.

init() ->
    af_type:register_type(#af_type{name = 'Actor'}),
    af_type:register_type(#af_type{name = 'ActorSend'}),

    %% server: pops type instance, spawns actor, pushes Actor ref
    af_type:add_op('Any', #operation{
        name = "server",
        sig_in = ['Any'],
        sig_out = ['Actor'],
        impl = fun op_server/1
    }),

    %% <<: enters actor-send mode (in Actor dict)
    af_type:add_op('Actor', #operation{
        name = "<<",
        sig_in = ['Actor'],
        sig_out = ['ActorSend'],
        impl = fun op_send_begin/1
    }),

    %% . in ActorSend: finishes send block
    af_type:add_op('ActorSend', #operation{
        name = ".",
        sig_in = ['ActorSend'],
        sig_out = [],
        impl = fun op_send_end/1
    }),

    %% self: push current process pid as Actor (no vocab — raw Actor)
    af_type:add_op('Any', #operation{
        name = "self",
        sig_in = [],
        sig_out = ['Actor'],
        impl = fun op_self/1
    }),

    %% Set handler for ActorSend
    af_type:register_type(#af_type{
        name = 'ActorSend',
        ops = get_ops('ActorSend'),
        handler = fun handle_actor_send/2
    }),

    ok.

get_ops(TypeName) ->
    {ok, #af_type{ops = Ops}} = af_type:get_type(TypeName),
    Ops.

%%% --- server: spawn actor from type instance ---

op_server(Cont) ->
    [{TypeName, _Value} = Instance | Rest] = Cont#continuation.data_stack,
    Vocab0 = build_vocab(TypeName),
    Vocab = Vocab0#{"stop" => [#{args => [], returns => []}]},
    Pid = erlang:spawn_link(fun() -> actor_loop(TypeName, Instance) end),
    ActorVal = #{pid => Pid, type_name => TypeName, vocab => Vocab},
    Cont#continuation{data_stack = [{'Actor', ActorVal} | Rest]}.

%% Build the remotely-callable vocabulary for a type.
%% Only includes user-defined words (source =/= auto).
%% Computes args_in (sig_in minus state type) and returns (sig_out minus state type).
build_vocab(TypeName) ->
    case af_type:get_type(TypeName) of
        {ok, #af_type{ops = Ops}} ->
            maps:fold(fun(WordName, OpList, Acc) ->
                Entries = lists:filtermap(fun(#operation{source = Source} = Op) ->
                    case Source of
                        auto -> false;
                        _ ->
                            ArgsIn = remove_first(TypeName, Op#operation.sig_in),
                            Returns = remove_first(TypeName, Op#operation.sig_out),
                            {true, #{args => ArgsIn, returns => Returns}}
                    end
                end, OpList),
                case Entries of
                    [] -> Acc;
                    _ -> maps:put(WordName, Entries, Acc)
                end
            end, #{}, Ops);
        not_found ->
            #{}
    end.

%% Remove the first occurrence of an element from a list.
remove_first(_Elem, []) -> [];
remove_first(Elem, [Elem | Rest]) -> Rest;
remove_first(Elem, [H | Rest]) -> [H | remove_first(Elem, Rest)].

%%% --- << : begin actor send ---

op_send_begin(Cont) ->
    [{'Actor', ActorInfo} | Rest] = Cont#continuation.data_stack,
    SendState = #{
        actor => {'Actor', ActorInfo},
        local_stack => [],
        vocab => maps:get(vocab, ActorInfo)
    },
    Cont#continuation{data_stack = [{'ActorSend', SendState} | Rest]}.

%%% --- . : end actor send ---

op_send_end(Cont) ->
    [{'ActorSend', State} | Rest] = Cont#continuation.data_stack,
    #{actor := ActorVal, local_stack := LocalStack} = State,
    %% Push Actor back, then any remaining local stack values on top
    Cont#continuation{data_stack = LocalStack ++ [ActorVal | Rest]}.

%%% --- ActorSend handler: intercepts all tokens ---

handle_actor_send(TokenValue, Cont) ->
    [{'ActorSend', State} | Rest] = Cont#continuation.data_stack,
    #{actor := {'Actor', ActorInfo}, local_stack := LocalStack, vocab := Vocab} = State,
    case maps:find(TokenValue, Vocab) of
        {ok, Entries} ->
            %% Actor vocabulary word — find matching entry and dispatch
            dispatch_actor_word(TokenValue, Entries, ActorInfo, LocalStack, State, Rest, Cont);
        error ->
            %% Not an actor word — execute locally on temp stack
            TempCont = #continuation{data_stack = LocalStack},
            Token = #token{value = TokenValue},
            NewTempCont = af_interpreter:interpret_token(Token, TempCont),
            NewLocalStack = NewTempCont#continuation.data_stack,
            NewState = State#{local_stack => NewLocalStack},
            Cont#continuation{data_stack = [{'ActorSend', NewState} | Rest]}
    end.

dispatch_actor_word(WordName, Entries, ActorInfo, LocalStack, State, Rest, Cont) ->
    %% Find the first entry whose args match the local stack
    #{pid := Pid} = ActorInfo,
    Entry = find_matching_entry(Entries, LocalStack),
    #{args := ArgsIn, returns := Returns} = Entry,
    %% Pop args from local stack
    NumArgs = length(ArgsIn),
    {Args, RemainingStack} = lists:split(NumArgs, LocalStack),
    case Returns of
        [] ->
            %% Cast: async, no reply
            Pid ! {cast, WordName, Args},
            NewState = State#{local_stack => RemainingStack},
            Cont#continuation{data_stack = [{'ActorSend', NewState} | Rest]};
        _ ->
            %% Call: sync, wait for reply
            Ref = make_ref(),
            Pid ! {call, WordName, Args, self(), Ref},
            receive
                {reply, Ref, ReplyValues} ->
                    NewLocalStack = ReplyValues ++ RemainingStack,
                    NewState = State#{local_stack => NewLocalStack},
                    Cont#continuation{data_stack = [{'ActorSend', NewState} | Rest]}
            after 5000 ->
                error({actor_call_timeout, WordName})
            end
    end.

find_matching_entry([Entry], _Stack) ->
    %% Single entry — just use it (validation could be added later)
    Entry;
find_matching_entry([Entry | _Rest], _Stack) ->
    %% TODO: signature matching against local stack for overloaded words
    Entry.

%%% --- self ---

op_self(Cont) ->
    Pid = self(),
    ActorVal = #{pid => Pid, type_name => undefined, vocab => #{}},
    Cont#continuation{
        data_stack = [{'Actor', ActorVal} | Cont#continuation.data_stack]
    }.

%%% --- Actor process loop ---

actor_loop(TypeName, StateInstance) ->
    receive
        {cast, "stop", _Args} ->
            ok;
        {cast, WordName, Args} ->
            NewState = execute_actor_word(WordName, Args, TypeName, StateInstance),
            actor_loop(TypeName, NewState);
        {call, WordName, Args, From, Ref} ->
            {ReplyValues, NewState} = execute_actor_call(WordName, Args, TypeName, StateInstance),
            From ! {reply, Ref, ReplyValues},
            actor_loop(TypeName, NewState)
    end.

%% Execute a word on the actor's stack (state instance + pushed args).
%% Returns the updated state instance.
execute_actor_word(WordName, Args, _TypeName, StateInstance) ->
    %% Build stack: state on TOS (for TOS-driven dispatch), args below
    Stack = [StateInstance | Args],
    Cont = #continuation{data_stack = Stack},
    Token = #token{value = WordName},
    ResultCont = af_interpreter:interpret_token(Token, Cont),
    %% The state instance should be on the stack (possibly with other values)
    %% Take the bottom-most item as the state
    ResultStack = ResultCont#continuation.data_stack,
    lists:last(ResultStack).

%% Execute a word that returns values (call semantics).
%% Returns {ReplyValues, NewStateInstance}.
execute_actor_call(WordName, Args, TypeName, StateInstance) ->
    %% Build stack: state on TOS (for TOS-driven dispatch), args below
    Stack = [StateInstance | Args],
    Cont = #continuation{data_stack = Stack},
    Token = #token{value = WordName},
    ResultCont = af_interpreter:interpret_token(Token, Cont),
    ResultStack = ResultCont#continuation.data_stack,
    %% Separate reply values from state.
    %% After execution, state type should be on the stack.
    %% Everything above it is reply data.
    separate_reply(TypeName, ResultStack).

%% Split the stack into reply values (above state) and state instance.
separate_reply(TypeName, Stack) ->
    separate_reply(TypeName, Stack, []).

separate_reply(TypeName, [{TypeName, _} = State | _Rest], ReplyAcc) ->
    {lists:reverse(ReplyAcc), State};
separate_reply(TypeName, [Item | Rest], ReplyAcc) ->
    separate_reply(TypeName, Rest, [Item | ReplyAcc]);
separate_reply(_TypeName, [], ReplyAcc) ->
    %% No state found — shouldn't happen with well-behaved words
    {lists:reverse(ReplyAcc), undefined}.
