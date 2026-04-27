-module(af_hos_dsl_a4_native_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("operation.hrl").

%% End-to-end tests for the a4-native HOS DSL (lib/hos/dsl.a4).
%% Loads the DSL once per test, then runs values-first source
%% through the interpreter and verifies registry contents.

setup() ->
    af_type:reset(),
    af_type_compiler:clear_pending_checks(),
    af_hos_check:clear_registry(),
    Cont0 = af_interpreter:new_continuation(),
    {ok, Source} = file:read_file("lib/hos/dsl.a4"),
    Tokens = af_parser:parse(binary_to_list(Source), "lib/hos/dsl.a4"),
    af_interpreter:interpret_tokens(Tokens, Cont0).

run(Src, BootCont) ->
    Tokens = af_parser:parse(Src, "test"),
    try
        af_interpreter:interpret_tokens(Tokens, BootCont)
    catch C:R:S ->
        erlang:error({interp_failed, C, R, S})
    end.

%% Minimal: empty system finalises and registers.
minimal_system_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun(BootCont) ->
        fun() ->
            run("Door system .", BootCont),
            ?assertMatch({'HosBlueprint', <<"Door">>, _, _, _, _, _},
                         af_hos_check:lookup_system("Door"))
        end
     end}.

%% System with parent, fsm, and a couple of events.
system_with_fields_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun(BootCont) ->
        fun() ->
            run("Door system "
                "  Car parent "
                "  DoorState fsm "
                "  open event "
                "  close event "
                ".", BootCont),
            Sys = af_hos_check:lookup_system("Door"),
            ?assertMatch({'HosBlueprint', <<"Door">>, <<"Car">>,
                          'DoorState', _, _, _}, Sys),
            {'HosBlueprint', _, _, _, _, Handlers, _} = Sys,
            HandlerEvents =
                [E || {'HandlerSpec', E, _, _, _} <- Handlers],
            ?assertEqual([open, close], HandlerEvents)
        end
     end}.

%% Confirm TB has no handler (architecture: dict-dispatch only).
tb_clean_dispatch_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun(_BootCont) ->
        fun() ->
            {ok, TB} = af_type:get_type('TransitionsBlock'),
            ?assertEqual(undefined, TB#af_type.handler),
            {ok, AnyEts} = af_type:get_type('Any'),
            TransOps = maps:get("trans", AnyEts#af_type.ops, []),
            ?assertEqual(3, length(TransOps))
        end
     end}.

%% Single basic transition row.
single_basic_transition_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun(BootCont) ->
        fun() ->
            FinalCont =
                run("Door system "
                    "  DoorState fsm "
                    "  Closed state  Opening state "
                    "  open event "
                    "  transitions "
                    "    Closed Opening open trans "
                    "  ; "
                    ".", BootCont),
            Sys = af_hos_check:lookup_system("Door"),
            case Sys of
                not_found ->
                    erlang:error({not_registered,
                                  {final_stack,
                                   FinalCont#continuation.data_stack}});
                _ -> ok
            end,
            ?assertMatch({'HosBlueprint', <<"Door">>, _, 'DoorState', _, _,
                          [{'TransitionSpec', 'Closed', 'Opening', 'open',
                            <<>>, 'none', 0}]}, Sys)
        end
     end}.

%% Mixed: basic + subsystem effect + timer effect.
system_with_transitions_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     fun(BootCont) ->
        fun() ->
            run("Door system "
                "  Car parent "
                "  DoorState fsm "
                "  Closed state  Opening state "
                "  Open state    Closing state "
                "  open event    opened event "
                "  close event   closed event "
                "  transitions "
                "    Closed  Opening  open                       trans "
                "    Opening Open     opened  Car opened         trans "
                "    Open    Closing  close   100 after closed   trans "
                "    Closing Closed   closed  Car closed         trans "
                "  ; "
                ".", BootCont),
            Sys = af_hos_check:lookup_system("Door"),
            {'HosBlueprint', _, _, _, _, _, Trans} = Sys,
            ?assertEqual(4, length(Trans)),
            [T1, T2, T3, T4] = Trans,
            ?assertMatch({'TransitionSpec', 'Closed', 'Opening', 'open',
                          <<>>, 'none', 0}, T1),
            ?assertMatch({'TransitionSpec', 'Opening', 'Open', 'opened',
                          <<"Car">>, 'opened', 0}, T2),
            ?assertMatch({'TransitionSpec', 'Open', 'Closing', 'close',
                          <<"after">>, 'closed', 100}, T3),
            ?assertMatch({'TransitionSpec', 'Closing', 'Closed', 'closed',
                          <<"Car">>, 'closed', 0}, T4)
        end
     end}.
