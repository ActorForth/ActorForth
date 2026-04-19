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
%%
%% Two modes, selected by the system's state_type:
%%
%%   Stateful (state_type /= 'None'): the transitions table is
%%   authoritative. On event E in current state S, look up the row
%%   (S, _, E). If found, dispatch its effect and advance
%%   current_state. If not found, the event is input-rejected
%%   (no state change, no effect). One event fires at most one
%%   transition; there is no cascade.
%%
%%   Stateless (state_type = 'None'): handler bodies drive routing.
%%   The body's `Target Event` pairs dispatch {cast, Event, []} to
%%   the named child or parent. Used for pass-through routers like
%%   BuildingSystem in an early prototype.
%%
%% In both modes, an event arriving with no matching handler or
%% transition is logged and ignored. Unexpected input is safe.

dispatch(EventName, Args, State) ->
    EventStr = atom_to_list(EventName),
    State1 = log_event(EventStr, State),
    case maps:get(state_type, State1) of
        'None' ->
            case find_handler(EventName, maps:get(handlers, State1)) of
                {ok, Handler} ->
                    execute_handler_body(Handler, Args, State1);
                not_found ->
                    State1
            end;
        _ ->
            CurState = maps:get(current_state, State1),
            dispatch_transition(CurState, EventName, State1)
    end.

%% Stateful dispatch: look up (current_state, _, EventName) in the
%% transitions table. Fire it if found, input-reject otherwise.
dispatch_transition(CurState, EventName, State) ->
    Transitions = maps:get(transitions, State),
    case find_transition_for_event(CurState, EventName, Transitions) of
        not_found ->
            State;
        {'TransitionSpec', _F, To, _Trig, Target, EvtEff, Delay} ->
            dispatch_effect(Target, EvtEff, Delay, State),
            State#{current_state => To};
        {'TransitionSpec', _F, To, _Trig, Target, EvtEff} ->
            dispatch_effect(Target, EvtEff, 0, State),
            State#{current_state => To};
        {'TransitionSpec', _F, To, _Trig} ->
            State#{current_state => To}
    end.

find_transition_for_event(_CurState, _Event, []) ->
    not_found;
find_transition_for_event(CurState, Event, [T | Rest]) ->
    case element(2, T) =:= CurState andalso
         element(4, T) =:= Event of
        true  -> T;
        false -> find_transition_for_event(CurState, Event, Rest)
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


%% Handler body interpreter. Stateless systems only.
%%
%% Stateful systems are driven by the transitions table; their
%% handler bodies are forbidden by the checker. This function is
%% only invoked in the stateless branch of `dispatch`.
execute_handler_body({'HandlerSpec', _EventName, _SigIn, _SigOut, Body},
                     _Args, State) ->
    Tokens = [token_text(T) || T <- Body],
    process_tokens_stateless(Tokens, State).

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

%% dispatch_effect/4 handles three cases:
%%   * no target: effect is absent (ok).
%%   * target "after" with delay > 0: schedule self-send via
%%     erlang:send_after; the mailbox stays open so other events
%%     (emergency, stop) can arrive and preempt via input rejection
%%     of the late scheduled event if it no longer applies.
%%   * target "after" with delay 0: immediate self-send.
%%   * any other target: resolve to a parent/child PID and cast.
dispatch_effect(<<>>, _Event, _Delay, _State) -> ok;
dispatch_effect("", _Event, _Delay, _State)   -> ok;
dispatch_effect(_Target, 'none', _Delay, _State) -> ok;
dispatch_effect(<<"after">>, Event, Delay, _State) when Delay > 0 ->
    erlang:send_after(Delay, self(), {cast, Event, []}),
    ok;
dispatch_effect(<<"after">>, Event, 0, _State) ->
    self() ! {cast, Event, []},
    ok;
dispatch_effect(Target, Event, 0, State) ->
    TargetStr = case Target of
                    B when is_binary(B) -> binary_to_list(B);
                    L when is_list(L)   -> L
                end,
    case resolve_target(TargetStr, State) of
        {ok, Pid}     -> Pid ! {cast, Event, []}, ok;
        not_a_target  -> ok
    end;
dispatch_effect(_Target, _Event, _Delay, _State) ->
    %% Non-"after" target with nonzero delay is not a supported
    %% combination; ignore rather than crash.
    ok.

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
