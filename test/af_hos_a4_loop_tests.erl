-module(af_hos_a4_loop_tests).
-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

%% Phase 1a tests for the a4-native HOS runtime skeleton.
%%
%% These tests exercise the a4 runtime directly, bypassing the
%% Erlang af_hos_runtime. They verify only the minimal mail-loop
%% structure (receive -> handle-message -> tail-call) and the two
%% message types HosCast and HosStop.
%%
%% Children, transitions, effects, call/log/introspect are Phase 1b+
%% and are NOT exercised here.

%% ---------------------------------------------------------------
%% Setup / teardown
%% ---------------------------------------------------------------

setup() ->
    af_repl:init_types(),
    load_a4_runtime(),
    ok.

teardown(_) ->
    af_type:reset(),
    ok.

load_a4_runtime() ->
    Path = "src/bootstrap/hos/runtime.a4",
    {ok, Content} = file:read_file(Path),
    Tokens = af_parser:parse(binary_to_list(Content), Path),
    _Cont = af_interpreter:interpret_tokens(Tokens,
        af_interpreter:new_continuation()),
    %% Surface any deferred type checks that never resolved during
    %% load — matches af_repl:run_file/1's behaviour for a4 files.
    af_type_compiler:finalize_pending_checks(),
    ok.

%% ---------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------

%% Build a fresh HosState. Field values are RAW per af_type_product
%% convention. Fields past the type tag: name, parent, child-map, log,
%% trans-index, current-state, state-type, handlers. Defaults for
%% direct-spawn tests.
make_state(Name) ->
    SentinelActor = #{pid => self(), type_name => undefined, vocab => #{}},
    {'HosState', list_to_binary(Name), SentinelActor, #{}, [], #{}, 'None',
     'None', []}.

%% Spawn a process that runs the a4 `hos-loop` word with InitialState
%% already on the data stack. The process dies when hos-loop exits.
spawn_loop(InitialState) ->
    erlang:spawn(fun() ->
        Cont = (af_interpreter:new_continuation())#continuation{
            data_stack = [InitialState]
        },
        Token = #token{value = "hos-loop"},
        af_interpreter:interpret_token(Token, Cont)
    end).

send_cast(Pid, EventName) ->
    Pid ! {af_msg, {'HosCast', EventName, []}}.

send_stop(Pid) ->
    Pid ! {af_msg, {'HosStop', normal}}.

%% Wait up to TimeoutMs for Pid to exit. Returns ok for normal or
%% noproc (already-dead, which the monitor reports immediately).
%% Anything else — abnormal exit reason or timeout — is surfaced.
wait_exit(Pid, TimeoutMs) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok;
        {'DOWN', Ref, process, Pid, noproc} -> ok;
        {'DOWN', Ref, process, Pid, Reason} -> {exit, Reason}
    after TimeoutMs ->
        erlang:demonitor(Ref, [flush]),
        timeout
    end.

%% ---------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------

loop_fixture_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [ fun(_) -> {"bool-subclause False",            fun bool_false_dispatch/0} end,
       fun(_) -> {"bool-subclause True",             fun bool_true_dispatch/0} end,
       fun(_) -> {"handle-message HosCast",          fun handle_cast_sync/0} end,
       fun(_) -> {"handle-message HosStop",          fun handle_stop_sync/0} end,
       fun(_) -> {"hos-loop-step False exits",       fun step_false_exits/0} end,
       fun(_) -> {"spawn-then-stop clean exit",      fun spawn_then_stop/0} end,
       fun(_) -> {"cast-then-stop clean exit",       fun cast_then_stop/0} end,
       fun(_) -> {"many casts then stop clean exit", fun many_casts_then_stop/0} end,
       fun(_) -> {"lookup-full-system resolves",     fun lookup_resolves/0} end,
       fun(_) -> {"a4 spawn-system on a leaf",       fun a4_spawn_leaf/0} end,
       fun(_) -> {"a4 spawn-system with children",   fun a4_spawn_parent_with_children/0} end,
       fun(_) -> {"stop propagates to children",     fun stop_propagates_to_children/0} end,
       fun(_) -> {"HosGetLog returns event log",     fun get_log_via_hosgetlog/0} end,
       fun(_) -> {"HosCall replies ok",              fun call_replies_ok/0} end,
       fun(_) -> {"HosIntrospectChild returns PID",  fun introspect_child_pid/0} end,
       fun(_) -> {"HosIntrospectChild unknown=not_found", fun introspect_child_unknown/0} end,
       fun(_) -> {"initial state = first transition's From",  fun initial_state_from_first/0} end,
       fun(_) -> {"stateful cast fires a transition",         fun stateful_transition_fires/0} end,
       fun(_) -> {"stateful cast rejects unmatched event",    fun stateful_unmatched_rejected/0} end,
       fun(_) -> {"toggle between two states across 4 flips", fun stateful_four_flips/0} end,
       fun(_) -> {"transition effect sends event to child",   fun effect_to_child/0} end,
       fun(_) -> {"after-delay 0 self-sends the event",       fun effect_after_zero/0} end,
       fun(_) -> {"stateless router dispatches body pairs",   fun stateless_router_dispatches/0} end
     ]}.

%% --- Phase 1b: a4 spawn-system --------------------------------------

lookup_resolves() ->
    af_hos_check:clear_registry(),
    Full = leaf_system("Probe", ""),
    af_hos_check:register_system(Full),
    %% Direct Erlang sanity-check.
    ?assertMatch({'SystemNode', <<"Probe">>, _, _, _, _, _},
                 af_hos_check:lookup_system(<<"Probe">>)),
    %% Then via the a4 word (FFI path).
    Cont0 = (af_interpreter:new_continuation())#continuation{
        data_stack = [{'String', <<"Probe">>}]
    },
    Cont1 = af_interpreter:interpret_token(#token{value = "lookup-full-system"}, Cont0),
    ?assertMatch([{'Tuple', {'SystemNode', <<"Probe">>, _, _, _, _, _}}],
                 Cont1#continuation.data_stack).

a4_spawn_system(SystemNode) ->
    Cont0 = (af_interpreter:new_continuation())#continuation{
        data_stack = [{'Tuple', SystemNode}]
    },
    Cont1 = af_interpreter:interpret_token(#token{value = "spawn-system"}, Cont0),
    [{'Actor', #{pid := Pid}}] = Cont1#continuation.data_stack,
    Pid.

leaf_system(Name, Parent) ->
    {'SystemNode', list_to_binary(Name), list_to_binary(Parent),
     'None', [], [], []}.

a4_spawn_leaf() ->
    af_hos_check:clear_registry(),
    Node = leaf_system("Solo", ""),
    af_hos_check:register_system(Node),
    Pid = a4_spawn_system(Node),
    timer:sleep(20),
    ?assert(is_process_alive(Pid)),
    send_stop(Pid),
    ?assertEqual(ok, wait_exit(Pid, 500)).

a4_spawn_parent_with_children() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    C1 = leaf_system("C1", "Root"),
    C2 = leaf_system("C2", "Root"),
    Root = {'SystemNode', <<"Root">>, <<"">>, 'None', [C1, C2], [], []},
    af_hos_check:register_system(C1),
    af_hos_check:register_system(C2),
    af_hos_check:register_system(Root),
    RootPid = a4_spawn_system(Root),
    timer:sleep(50),
    %% Children are spawned in the ROOT actor's process, not ours, so
    %% they're linked to RootPid rather than the test process. Query
    %% them via HosIntrospectChild.
    C1Pid = introspect_one(RootPid, <<"C1">>),
    C2Pid = introspect_one(RootPid, <<"C2">>),
    ?assert(is_pid(C1Pid)),
    ?assert(is_pid(C2Pid)),
    ?assertNotEqual(C1Pid, C2Pid),
    [?assert(is_process_alive(P)) || P <- [C1Pid, C2Pid]],
    send_stop(RootPid),
    wait_exit(RootPid, 500),
    [wait_exit(P, 500) || P <- [C1Pid, C2Pid]].

introspect_one(Pid, Name) ->
    Ref = introspect_ref(),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    Pid ! {af_msg, {'HosIntrospectChild', Name, From, Ref}},
    case await_reply(Ref, 500) of
        {ok, ChildPid} when is_pid(ChildPid) -> ChildPid;
        Other -> error({introspect_failed, Name, Other})
    end.

stop_propagates_to_children() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    C1 = leaf_system("C1b", "Rootb"),
    C2 = leaf_system("C2b", "Rootb"),
    Root = {'SystemNode', <<"Rootb">>, <<"">>, 'None', [C1, C2], [], []},
    af_hos_check:register_system(C1),
    af_hos_check:register_system(C2),
    af_hos_check:register_system(Root),
    RootPid = a4_spawn_system(Root),
    timer:sleep(50),
    C1Pid = introspect_one(RootPid, <<"C1b">>),
    C2Pid = introspect_one(RootPid, <<"C2b">>),
    send_stop(RootPid),
    ?assertEqual(ok, wait_exit(RootPid, 500)),
    [?assertEqual(ok, wait_exit(P, 500)) || P <- [C1Pid, C2Pid]].

%% --- Phase 1c tests -------------------------------------------------

%% Wait for a typed reply wrapped in af_msg. The runtime sends replies
%% via the a4 `send` primitive, which preserves the stack-item shape
%% {'Tuple', {Ref, Value}} — the outer tag is carried through.
await_reply(RefAtom, TimeoutMs) ->
    receive
        {af_msg, {'Tuple', {RefAtom, Value}}} -> {ok, Value}
    after TimeoutMs ->
        timeout
    end.

get_log_via_hosgetlog() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    Node = leaf_system("LogSys", ""),
    af_hos_check:register_system(Node),
    RootPid = a4_spawn_system(Node),
    timer:sleep(20),
    %% Send two casts so the log has entries.
    send_cast(RootPid, e1),
    send_cast(RootPid, e2),
    timer:sleep(20),
    Ref = list_to_atom("glog-" ++ integer_to_list(erlang:unique_integer([positive]))),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    RootPid ! {af_msg, {'HosGetLog', From, Ref}},
    Result = await_reply(Ref, 500),
    send_stop(RootPid),
    wait_exit(RootPid, 500),
    ?assertMatch({ok, _}, Result),
    {ok, Log} = Result,
    %% make-tuple strips stack-item tags via from_stack_item, so the
    %% list elements come back as raw atoms, most-recent-first.
    ?assertEqual([e2, e1], Log).

call_replies_ok() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    Node = leaf_system("CallSys", ""),
    af_hos_check:register_system(Node),
    RootPid = a4_spawn_system(Node),
    timer:sleep(20),
    Ref = list_to_atom("call-" ++ integer_to_list(erlang:unique_integer([positive]))),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    RootPid ! {af_msg, {'HosCall', ping, [], From, Ref}},
    Result = await_reply(Ref, 500),
    send_stop(RootPid),
    wait_exit(RootPid, 500),
    ?assertEqual({ok, ok}, Result).

introspect_child_pid() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    C1 = leaf_system("IC1", "IRoot"),
    Root = {'SystemNode', <<"IRoot">>, <<"">>, 'None', [C1], [], []},
    af_hos_check:register_system(C1),
    af_hos_check:register_system(Root),
    RootPid = a4_spawn_system(Root),
    timer:sleep(20),
    Ref = list_to_atom("ic-" ++ integer_to_list(erlang:unique_integer([positive]))),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    RootPid ! {af_msg, {'HosIntrospectChild', <<"IC1">>, From, Ref}},
    Result = await_reply(Ref, 500),
    send_stop(RootPid),
    wait_exit(RootPid, 500),
    %% make-tuple unwraps the Actor stack item down to the raw PID.
    ?assertMatch({ok, Pid} when is_pid(Pid), Result).

introspect_child_unknown() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    Node = leaf_system("NoKids", ""),
    af_hos_check:register_system(Node),
    RootPid = a4_spawn_system(Node),
    timer:sleep(20),
    Ref = list_to_atom("icn-" ++ integer_to_list(erlang:unique_integer([positive]))),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    RootPid ! {af_msg, {'HosIntrospectChild', <<"Ghost">>, From, Ref}},
    Result = await_reply(Ref, 500),
    send_stop(RootPid),
    wait_exit(RootPid, 500),
    ?assertEqual({ok, not_found}, Result).

%% --- Phase 1d: stateful transitions --------------------------------

%% Build a SystemNode with a list of transitions. Each transition is
%% {'TransitionSpec', From, To, Trigger, Target, Effect, Delay}. We use
%% no effects for the Phase 1d state-machine unit tests.
transition(From, To, Trigger) ->
    {'TransitionSpec', From, To, Trigger, <<>>, 'none', 0}.

stateful_system(Name, Transitions) ->
    {'SystemNode', list_to_binary(Name), <<"">>,
     'Mode', [], [], Transitions}.

%% Query the a4 runtime's current-state via HosGetState.
current_state(Pid) ->
    Ref = list_to_atom("cs-" ++ integer_to_list(erlang:unique_integer([positive]))),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    Pid ! {af_msg, {'HosGetState', From, Ref}},
    await_reply(Ref, 500).

initial_state_from_first() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    Node = stateful_system("Light",
        [transition('Off', 'On', flip),
         transition('On', 'Off', flip)]),
    af_hos_check:register_system(Node),
    Pid = a4_spawn_system(Node),
    timer:sleep(20),
    Result = current_state(Pid),
    send_stop(Pid),
    wait_exit(Pid, 500),
    ?assertEqual({ok, 'Off'}, Result).

stateful_transition_fires() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    Node = stateful_system("Light",
        [transition('Off', 'On', flip),
         transition('On', 'Off', flip)]),
    af_hos_check:register_system(Node),
    Pid = a4_spawn_system(Node),
    timer:sleep(20),
    send_cast(Pid, flip),
    timer:sleep(20),
    Result = current_state(Pid),
    send_stop(Pid),
    wait_exit(Pid, 500),
    ?assertEqual({ok, 'On'}, Result).

stateful_unmatched_rejected() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    Node = stateful_system("Light",
        [transition('Off', 'On', flip),
         transition('On', 'Off', flip)]),
    af_hos_check:register_system(Node),
    Pid = a4_spawn_system(Node),
    timer:sleep(20),
    send_cast(Pid, 'bogus-event'),
    timer:sleep(20),
    Result = current_state(Pid),
    send_stop(Pid),
    wait_exit(Pid, 500),
    ?assertEqual({ok, 'Off'}, Result).

stateful_four_flips() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    Node = stateful_system("Light",
        [transition('Off', 'On', flip),
         transition('On', 'Off', flip)]),
    af_hos_check:register_system(Node),
    Pid = a4_spawn_system(Node),
    timer:sleep(20),
    [send_cast(Pid, flip) || _ <- lists:seq(1, 4)],
    timer:sleep(30),
    Result = current_state(Pid),
    send_stop(Pid),
    wait_exit(Pid, 500),
    %% 4 flips: Off -> On -> Off -> On -> Off.
    ?assertEqual({ok, 'Off'}, Result).

%% TransitionSpec with an explicit effect clause.
transition_with_effect(From, To, Trigger, TargetBin, Event, Delay) ->
    {'TransitionSpec', From, To, Trigger, TargetBin, Event, Delay}.

%% Parent with one child. Parent's `trigger` transition fires an effect
%% to the child: the Child sees an event in its own log.
effect_to_child() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    %% Child accepts the `ring` event via a self-loop transition.
    Child = {'SystemNode', <<"ChildEC">>, <<"ParentEC">>, 'Mode',
             [], [],
             [transition('Wait', 'Wait', ring)]},
    ParentT = transition_with_effect('Idle', 'Idle', trigger,
                                     <<"ChildEC">>, ring, 0),
    Parent = {'SystemNode', <<"ParentEC">>, <<"">>, 'Mode',
              [Child], [], [ParentT]},
    af_hos_check:register_system(Child),
    af_hos_check:register_system(Parent),
    ParentPid = a4_spawn_system(Parent),
    timer:sleep(20),
    %% Ask the parent for its child PID via HosIntrospectChild. More
    %% reliable than the links-diff pattern in this test, since the
    %% parent's transitions list changes the link graph EUnit
    %% observes from the test process.
    R1 = introspect_ref(),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    ParentPid ! {af_msg, {'HosIntrospectChild', <<"ChildEC">>, From, R1}},
    {ok, ChildPid} = await_reply(R1, 500),
    ?assert(is_pid(ChildPid)),
    send_cast(ParentPid, trigger),
    timer:sleep(30),
    R2 = introspect_ref(),
    ChildPid ! {af_msg, {'HosGetLog', From, R2}},
    ChildLog = await_reply(R2, 500),
    send_stop(ParentPid),
    wait_exit(ParentPid, 500),
    wait_exit(ChildPid, 500),
    ?assertEqual({ok, [ring]}, ChildLog).

introspect_ref() ->
    list_to_atom("ref-" ++ integer_to_list(erlang:unique_integer([positive]))).

%% Router system: state-type = 'None', handler routes events to children
%% via (Target EventName) pairs in the body token list.
stateless_router_dispatches() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    %% Two children, each accepting a distinct event.
    A = {'SystemNode', <<"Alpha">>, <<"Router">>, 'Mode', [], [],
         [transition('Idle', 'Idle', ping)]},
    B = {'SystemNode', <<"Beta">>, <<"Router">>, 'Mode', [], [],
         [transition('Idle', 'Idle', pong)]},
    %% Router handler 'fanout' routes: Alpha ping Beta pong.
    BodyTokens = [{'String', <<"Alpha">>}, {'String', <<"ping">>},
                  {'String', <<"Beta">>},  {'String', <<"pong">>}],
    Handler = {'HandlerSpec', fanout, [], [], BodyTokens},
    Router = {'SystemNode', <<"Router">>, <<"">>, 'None',
              [A, B], [Handler], []},
    af_hos_check:register_system(A),
    af_hos_check:register_system(B),
    af_hos_check:register_system(Router),
    RouterPid = a4_spawn_system(Router),
    timer:sleep(20),
    %% Find child PIDs via HosIntrospectChild.
    R1 = introspect_ref(),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    RouterPid ! {af_msg, {'HosIntrospectChild', <<"Alpha">>, From, R1}},
    {ok, AlphaPid} = await_reply(R1, 500),
    R2 = introspect_ref(),
    RouterPid ! {af_msg, {'HosIntrospectChild', <<"Beta">>, From, R2}},
    {ok, BetaPid} = await_reply(R2, 500),
    %% Send the fanout event to the router.
    send_cast(RouterPid, fanout),
    timer:sleep(30),
    %% Both children should have logged their respective events.
    R3 = introspect_ref(),
    AlphaPid ! {af_msg, {'HosGetLog', From, R3}},
    ALog = await_reply(R3, 500),
    R4 = introspect_ref(),
    BetaPid ! {af_msg, {'HosGetLog', From, R4}},
    BLog = await_reply(R4, 500),
    send_stop(RouterPid),
    wait_exit(RouterPid, 500),
    wait_exit(AlphaPid, 500),
    wait_exit(BetaPid, 500),
    ?assertEqual({ok, [ping]}, ALog),
    ?assertEqual({ok, [pong]}, BLog).

%% after 0 effect: transition fires a self-event. Verify the actor's
%% own log contains BOTH the originally-cast event and the after-event.
effect_after_zero() ->
    af_hos_check:clear_registry(),
    process_flag(trap_exit, true),
    T = transition_with_effect('Idle', 'Idle', trigger,
                               <<"after">>, followup, 0),
    %% Add a self-loop on followup so it stays registered as a valid
    %% event in state Idle (without it the after-event is input-rejected
    %% after arriving self-scheduled).
    T2 = transition('Idle', 'Idle', followup),
    Node = {'SystemNode', <<"Timer">>, <<"">>, 'Mode',
            [], [], [T, T2]},
    af_hos_check:register_system(Node),
    Pid = a4_spawn_system(Node),
    timer:sleep(20),
    send_cast(Pid, trigger),
    timer:sleep(30),
    Ref = list_to_atom("afz-" ++ integer_to_list(erlang:unique_integer([positive]))),
    From = #{pid => self(), type_name => undefined, vocab => #{}},
    Pid ! {af_msg, {'HosGetLog', From, Ref}},
    Result = await_reply(Ref, 500),
    send_stop(Pid),
    wait_exit(Pid, 500),
    ?assertMatch({ok, [followup, trigger]}, Result).

%% Probe: does multi-clause Bool dispatch work in isolation?
bool_false_dispatch() ->
    Src = ": probe-bool Bool -> Atom ;\n"
          "    : True -> Atom ; drop true-picked\n"
          "    : False -> Atom ; drop false-picked .\n",
    Tokens = af_parser:parse(Src, "probe"),
    Cont0 = af_interpreter:new_continuation(),
    Cont1 = af_interpreter:interpret_tokens(Tokens, Cont0),
    Cont2 = Cont1#continuation{data_stack = [{'Bool', false}]},
    Cont3 = af_interpreter:interpret_token(#token{value = "probe-bool"}, Cont2),
    ?assertMatch([{'Atom', "false-picked"}], Cont3#continuation.data_stack).

bool_true_dispatch() ->
    Src = ": probe-bool2 Bool -> Atom ;\n"
          "    : True -> Atom ; drop true-picked2\n"
          "    : False -> Atom ; drop false-picked2 .\n",
    Tokens = af_parser:parse(Src, "probe"),
    Cont0 = af_interpreter:new_continuation(),
    Cont1 = af_interpreter:interpret_tokens(Tokens, Cont0),
    Cont2 = Cont1#continuation{data_stack = [{'Bool', true}]},
    Cont3 = af_interpreter:interpret_token(#token{value = "probe-bool2"}, Cont2),
    ?assertMatch([{'Atom', "true-picked2"}], Cont3#continuation.data_stack).

step_false_exits() ->
    State = make_state("root"),
    %% hos-loop-step expects (HosState Bool) on stack. Bool=false -> exit.
    Cont0 = (af_interpreter:new_continuation())#continuation{
        data_stack = [{'Bool', false}, State]
    },
    Cont1 = af_interpreter:interpret_token(#token{value = "hos-loop-step"}, Cont0),
    ?assertEqual([], Cont1#continuation.data_stack).

%% Synchronous sanity check: dispatch handle-message on a state+HosCast
%% stack and verify the count field incremented. No processes involved.
handle_cast_sync() ->
    State0 = make_state("root"),
    Cast = {'HosCast', my_event, []},
    Cont0 = (af_interpreter:new_continuation())#continuation{
        data_stack = [Cast, State0]
    },
    Token = #token{value = "handle-message"},
    Cont1 = af_interpreter:interpret_token(Token, Cont0),
    %% (State' True). State'.log (position 5) is a list of one
    %% stack item wrapping the event atom. Auto-field-binding
    %% normalises atom raw values to their string form (list),
    %% matching what af_term:to_stack_item produces.
    ?assertMatch([{'Bool', true},
                  {'HosState', _, _, _, [{'Atom', "my_event"}], _, _, _, _}],
                 Cont1#continuation.data_stack).

handle_stop_sync() ->
    State0 = make_state("root"),
    Stop = {'HosStop', normal},
    Cont0 = (af_interpreter:new_continuation())#continuation{
        data_stack = [Stop, State0]
    },
    Token = #token{value = "handle-message"},
    Cont1 = af_interpreter:interpret_token(Token, Cont0),
    %% Stop handler returns (State' False). Log unchanged (empty).
    ?assertMatch([{'Bool', false}, {'HosState', _, _, _, [], _, _, _, _}],
                 Cont1#continuation.data_stack).

spawn_then_stop() ->
    Pid = spawn_loop(make_state("root")),
    true = is_process_alive(Pid),
    send_stop(Pid),
    ?assertEqual(ok, wait_exit(Pid, 500)).

cast_then_stop() ->
    Pid = spawn_loop(make_state("root")),
    send_cast(Pid, my_event),
    send_stop(Pid),
    ?assertEqual(ok, wait_exit(Pid, 1000)).

many_casts_then_stop() ->
    Pid = spawn_loop(make_state("root")),
    [send_cast(Pid, ev) || _ <- lists:seq(1, 100)],
    send_stop(Pid),
    ?assertEqual(ok, wait_exit(Pid, 2000)).
