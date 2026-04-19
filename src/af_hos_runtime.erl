-module(af_hos_runtime).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0, spawn_system/1, send_event/3, send_event_call/4,
         stop_system/1]).

%% HOS runtime wiring.
%%
%% Given a SystemNode tree whose axioms have already been checked,
%% spawn one Erlang process per system. The topology mirrors the
%% tree exactly: a parent process holds the PIDs of its direct
%% children (by name) and its own parent's PID. Sibling PIDs are not
%% reachable from any process, so a sibling-to-sibling message is
%% structurally impossible, not just disallowed. That is the HOS
%% axiom-1 guarantee surfaced at the runtime level.
%%
%% Handler body execution is intentionally limited in this MVP:
%%
%%   * `ChildName EventName` pair: send {cast, EventName, []} to the
%%     named child's PID.
%%   * `ParentName EventName` pair: send {cast, EventName, []} to
%%     the parent's PID.
%%   * bare event name matching one of this system's own handlers:
%%     self-dispatch.
%%   * anything else (literals, a4 builtins, etc.): ignored at this
%%     stage -- M5's job is to demonstrate topology, not to give the
%%     elevator a full business-logic runtime.
%%
%% The instrumentation log (accessible via stop_system's return)
%% records every event each system process received in order, which
%% lets FAT tests assert "parent saw X, then child saw Y."

%% ---------------------------------------------------------------
%% Registration into the a4 runtime
%% ---------------------------------------------------------------

init() ->
    af_type:add_op('Any', #operation{
        name = "spawn-system",
        sig_in = ['SystemNode'],
        sig_out = ['Actor'],
        impl = fun op_spawn_system/1
    }),
    af_type:add_op('Any', #operation{
        name = "send-event",
        sig_in = ['Actor', 'Atom'],
        sig_out = [],
        impl = fun op_send_event/1
    }),
    af_type:add_op('Any', #operation{
        name = "stop-system",
        sig_in = ['Actor'],
        sig_out = [],
        impl = fun op_stop_system/1
    }).


op_spawn_system(Cont) ->
    [Instance | Rest] = Cont#continuation.data_stack,
    Pid = spawn_system(Instance),
    ActorInfo = #{pid => Pid, type => hos_system, vocab => #{}},
    Cont#continuation{data_stack = [{'Actor', ActorInfo} | Rest]}.

op_send_event(Cont) ->
    [{'Atom', EventName}, {'Actor', Info} | Rest] = Cont#continuation.data_stack,
    Pid = maps:get(pid, Info),
    send_event(Pid, EventName, []),
    Cont#continuation{data_stack = Rest}.

op_stop_system(Cont) ->
    [{'Actor', Info} | Rest] = Cont#continuation.data_stack,
    Pid = maps:get(pid, Info),
    stop_system(Pid),
    Cont#continuation{data_stack = Rest}.


%% ---------------------------------------------------------------
%% Tree spawning
%% ---------------------------------------------------------------

spawn_system(SystemNode) ->
    spawn_system(SystemNode, undefined, "").

spawn_system({'SystemNode', NameBin, ParentNameBin, _StateType,
              Children, _Handlers, _Transitions} = Node,
             ParentPid, _ParentNameInTree) ->
    Name = to_list(NameBin),
    Parent = to_list(ParentNameBin),
    Self = spawn(fun() -> preflight(Node, ParentPid) end),
    register_children(Self, Children, Parent, Name),
    Self.

%% Late-binding trick: spawn the process early (so we have a PID to
%% hand to children), then tell it what children it got.
preflight(Node, ParentPid) ->
    receive
        {init, ChildMap} ->
            {'SystemNode', NameBin, ParentNameBin, StateType,
             _Children, Handlers, Transitions} = Node,
            Name = to_list(NameBin),
            Parent = to_list(ParentNameBin),
            Initial = initial_state(Transitions),
            loop(#{
                name => Name,
                parent_name => Parent,
                parent_pid => ParentPid,
                child_map => ChildMap,
                handlers => Handlers,
                state_type => StateType,
                transitions => Transitions,
                current_state => Initial,
                log => []
            })
    end.

%% First transition's From is the initial state by convention. For a
%% stateless system with no transitions the value is unused; we set
%% 'None' as a placeholder.
initial_state([]) -> 'None';
initial_state([T | _]) -> element(2, T).

%% Child entries in a registered SystemNode are name-stubs (the DSL
%% records only the child's declared name at definition time). At
%% spawn time we look each one up in the registry so the spawned
%% child actor has the child's full handler and transition table,
%% not an empty shell. An unregistered name falls back to the stub,
%% which is still a valid actor (no handlers, no transitions): it
%% just does nothing with events it receives.
register_children(Self, ChildStubs, _Parent, _Name) ->
    ChildPids = lists:map(fun(ChildStub) ->
        ChildName = to_list(element(2, ChildStub)),
        FullChild = case af_hos_check:lookup_system(ChildName) of
                        not_found -> ChildStub;
                        Full      -> Full
                    end,
        {ChildName, spawn_system(FullChild, Self, _Name)}
    end, ChildStubs),
    Self ! {init, maps:from_list(ChildPids)},
    ok.


%% ---------------------------------------------------------------
%% Main receive loop
%% ---------------------------------------------------------------

loop(State) ->
    receive
        {cast, EventName, Args} ->
            NewState = dispatch(EventName, Args, State),
            loop(NewState);
        {call, EventName, Args, From, Ref} ->
            {Reply, NewState} = dispatch_call(EventName, Args, State),
            From ! {reply, Ref, Reply},
            loop(NewState);
        {get_log, From, Ref} ->
            From ! {reply, Ref, lists:reverse(maps:get(log, State))},
            loop(State);
        {introspect_child, ChildName, From, Ref} ->
            %% Test-only hook: look up a child's PID by name. Used in
            %% unit tests to prove the parent holds the PID and a
            %% sibling does not. Intentionally NOT an HOS-legal event:
            %% production code would never use this.
            Reply = case maps:find(ChildName, maps:get(child_map, State)) of
                {ok, Pid} -> Pid;
                error -> not_found
            end,
            From ! {reply, Ref, Reply},
            loop(State);
        stop ->
            stop_children(State),
            ok
    end.

stop_children(#{child_map := ChildMap}) ->
    maps:foreach(fun(_Name, Pid) -> Pid ! stop end, ChildMap).


%% ---------------------------------------------------------------
%% Event dispatch
%% ---------------------------------------------------------------

dispatch(EventName, Args, State) ->
    EventStr = atom_to_list(EventName),
    State1 = log_event(EventStr, State),
    case find_handler(EventName, maps:get(handlers, State)) of
        {ok, Handler} ->
            execute_handler_body(Handler, Args, State1);
        not_found ->
            State1
    end.

dispatch_call(EventName, Args, State) ->
    NewState = dispatch(EventName, Args, State),
    {ok, NewState}.

log_event(EventStr, State) ->
    Log = maps:get(log, State),
    State#{log => [EventStr | Log]}.

find_handler(EventName, []) ->
    find_handler(EventName, [], no_match);
find_handler(EventName, Handlers) ->
    find_handler(EventName, Handlers, no_match).

find_handler(_EventName, [], no_match) -> not_found;
find_handler(EventName, [{'HandlerSpec', Name, _, _, _} = H | _], _)
        when Name =:= EventName ->
    {ok, H};
find_handler(EventName, [_ | Rest], Acc) ->
    find_handler(EventName, Rest, Acc).


%% Handler body interpreter.
%%
%% Stateful systems (state_type /= 'None'): handler bodies contain
%% only `-> State` markers. Each marker fires a declared transition;
%% the transition's effect_target/effect_event (if present) is
%% dispatched to the named subsystem. The current_state tracked in
%% the actor is updated.
%%
%% Stateless systems: handler bodies route events via `Target Event`
%% pairs (pass-through). No state machine.
execute_handler_body({'HandlerSpec', EventName, _SigIn, _SigOut, Body},
                     _Args, State) ->
    Tokens = [token_text(T) || T <- Body],
    case maps:get(state_type, State) of
        'None' -> process_tokens_stateless(Tokens, State);
        _      -> process_tokens_stateful(Tokens, EventName, State)
    end.

token_text({'String', B}) -> binary_to_list(B);
token_text(S) when is_list(S) -> S;
token_text(B) when is_binary(B) -> binary_to_list(B);
token_text({'Atom', A}) -> atom_to_list(A);
token_text(A) when is_atom(A) -> atom_to_list(A).

%% Stateless: pass-through routing via Target/Event pairs.
process_tokens_stateless([], State) ->
    State;
process_tokens_stateless([Target, EventName | Rest], State) ->
    case resolve_target(Target, State) of
        {ok, Pid} ->
            Pid ! {cast, list_to_atom(EventName), []},
            process_tokens_stateless(Rest, State);
        not_a_target ->
            process_tokens_stateless([EventName | Rest], State)
    end;
process_tokens_stateless([_Single], State) ->
    State.

%% Stateful: walk `-> State` markers, fire transitions, dispatch
%% declared effects. Each `-> State` advances current_state and sends
%% the declared effect (if any) to its target subsystem.
%%
%% Input rejection (Hamilton axiom 4): if a body's first transition
%% is not declared from the current state, the handler becomes a
%% no-op. The compiler proved some entry state reaches the target
%% under this trigger; if the live state is not among them, the
%% event does not apply. We stop processing the body rather than
%% half-run it or crash. A Door already Closed that receives close
%% falls into this branch.
process_tokens_stateful([], _Event, State) ->
    State;
process_tokens_stateful(["->", NextStateStr | Rest], Event, State) ->
    CurState = maps:get(current_state, State),
    NextState = list_to_atom(NextStateStr),
    case fire_transition(CurState, NextState, Event, State) of
        {ok, NewState} ->
            process_tokens_stateful(Rest, Event, NewState);
        skip ->
            State
    end;
process_tokens_stateful([_Passive | Rest], Event, State) ->
    process_tokens_stateful(Rest, Event, State).

%% Look up the transition (From, To, Trigger) in the declared table.
%% Dispatch its effect if declared, then update current_state.
%% Returns {ok, NewState} on a declared transition or `skip` when
%% the transition is not declared from the live state.
fire_transition(From, To, Event, State) ->
    Transitions = maps:get(transitions, State),
    case find_transition(From, To, Event, Transitions) of
        not_found ->
            skip;
        {'TransitionSpec', _F, _T, _Trig, EffectTarget, EffectEvent} ->
            dispatch_effect(EffectTarget, EffectEvent, State),
            {ok, State#{current_state => To}};
        {'TransitionSpec', _F, _T, _Trig} ->
            {ok, State#{current_state => To}}
    end.

find_transition(_From, _To, _Event, []) -> not_found;
find_transition(From, To, Event, [T | Rest]) ->
    case element(2, T) =:= From andalso
         element(3, T) =:= To andalso
         element(4, T) =:= Event of
        true  -> T;
        false -> find_transition(From, To, Event, Rest)
    end.

dispatch_effect(<<>>, _Event, _State) -> ok;
dispatch_effect("", _Event, _State)   -> ok;
dispatch_effect(_Target, 'none', _State) -> ok;
dispatch_effect(Target, Event, State) ->
    TargetStr = case Target of
                    B when is_binary(B) -> binary_to_list(B);
                    L when is_list(L)   -> L
                end,
    case resolve_target(TargetStr, State) of
        {ok, Pid}     -> Pid ! {cast, Event, []}, ok;
        not_a_target  -> ok
    end.

resolve_target(Name, #{child_map := ChildMap, parent_name := ParentName,
                       parent_pid := ParentPid, name := _SelfName}) ->
    case maps:find(Name, ChildMap) of
        {ok, Pid} -> {ok, Pid};
        error ->
            case Name =:= ParentName andalso ParentPid =/= undefined of
                true -> {ok, ParentPid};
                false -> not_a_target
            end
    end.


%% ---------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------

send_event(Pid, EventName, Args) when is_atom(EventName) ->
    Pid ! {cast, EventName, Args},
    ok;
send_event(Pid, EventName, Args) when is_list(EventName) ->
    send_event(Pid, list_to_atom(EventName), Args).

send_event_call(Pid, EventName, Args, Timeout) ->
    Ref = make_ref(),
    Pid ! {call, EventName, Args, self(), Ref},
    receive
        {reply, Ref, Reply} -> {ok, Reply}
    after Timeout ->
        {error, timeout}
    end.

stop_system(Pid) ->
    Pid ! stop,
    ok.


%% ---------------------------------------------------------------
%% Utility
%% ---------------------------------------------------------------

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(S) when is_list(S) -> S;
to_list(A) when is_atom(A) -> atom_to_list(A).
