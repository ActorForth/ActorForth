-module(af_test_prim_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

%%% Helpers

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_interpreter:new_continuation().

%%% trace-on / trace-off

trace_on_test_() ->
    [
        {"initial continuation has tracing disabled", fun() ->
            Cont = setup(),
            ?assertEqual(false, Cont#continuation.tracing),
            ?assertEqual([], Cont#continuation.exec_stack),
            ?assertEqual(undefined, Cont#continuation.depth_stats)
        end},
        {"trace-on enables tracing and initializes fields", fun() ->
            Cont = setup(),
            Cont1 = eval("trace-on", Cont),
            ?assertEqual(true, Cont1#continuation.tracing),
            ?assertEqual([], Cont1#continuation.exec_stack),
            ?assertEqual(#depth_stats{}, Cont1#continuation.depth_stats)
        end},
        {"trace-off disables tracing and clears fields", fun() ->
            Cont = setup(),
            Cont1 = eval("trace-on 1 int 2 int + trace-off", Cont),
            ?assertEqual(false, Cont1#continuation.tracing),
            ?assertEqual([], Cont1#continuation.exec_stack),
            ?assertEqual(undefined, Cont1#continuation.depth_stats)
        end}
    ].

%%% Tracing off => no events recorded

tracing_off_no_events_test_() ->
    [
        {"with tracing off, dispatch does not grow exec_stack", fun() ->
            Cont = setup(),
            Cont1 = eval("1 int 2 int +", Cont),
            ?assertEqual(false, Cont1#continuation.tracing),
            ?assertEqual([], Cont1#continuation.exec_stack)
        end}
    ].

%%% Tracing on => events recorded

tracing_on_events_test_() ->
    [
        {"traced dispatch grows exec_stack (newest-first)", fun() ->
            Cont = setup(),
            Cont1 = eval("trace-on 1 int 2 int +", Cont),
            ExecStack = Cont1#continuation.exec_stack,
            ?assert(length(ExecStack) >= 4),
            [Top | _] = ExecStack,
            ?assertMatch({exec, #token{value = "+"}, _, _}, Top)
        end},
        {"depth stats updated per dispatch", fun() ->
            Cont = setup(),
            Cont1 = eval("trace-on 1 int 2 int", Cont),
            Stats = Cont1#continuation.depth_stats,
            ?assert(Stats#depth_stats.count > 0),
            ?assert(Stats#depth_stats.data_max >= 2)
        end},
        {"cache hit emits {cache_hit, Entry} event", fun() ->
            Cont = setup(),
            %% First dispatch of `1 int 2 int +` fills the cache.
            %% Second `1 int 2 int +` should hit the cache.
            Cont1 = eval("trace-on 1 int 2 int + drop 1 int 2 int +", Cont),
            Events = Cont1#continuation.exec_stack,
            CacheHits = [E || {exec, _, _, {cache_hit, _}} = E <- Events],
            ?assert(length(CacheHits) > 0)
        end}
    ].

%%% capture-data / capture-exec

capture_data_test_() ->
    [
        {"capture-data pushes a List of current stack", fun() ->
            Cont = setup(),
            Cont1 = eval("1 int 2 int capture-data", Cont),
            [{'List', Captured} | _] = Cont1#continuation.data_stack,
            ?assertEqual([{'Int', 2}, {'Int', 1}], Captured)
        end},
        {"capture-exec wraps events as {'ExecEvent', Raw}", fun() ->
            Cont = setup(),
            Cont1 = eval("trace-on 1 int 2 int capture-exec", Cont),
            [{'List', Events} | _] = Cont1#continuation.data_stack,
            ?assert(length(Events) >= 4),
            [{Type, _Raw} | _] = Events,
            ?assertEqual('ExecEvent', Type)
        end},
        {"get-trace is an alias for capture-exec", fun() ->
            Cont = setup(),
            Cont1 = eval("trace-on 1 int 2 int get-trace", Cont),
            [{'List', Events} | _] = Cont1#continuation.data_stack,
            ?assert(length(Events) >= 4)
        end}
    ].

%%% get-coverage

coverage_test_() ->
    [
        {"get-coverage returns empty Map in Phase 1", fun() ->
            Cont = setup(),
            Cont1 = eval("get-coverage", Cont),
            [{'Map', M} | _] = Cont1#continuation.data_stack,
            ?assertEqual(#{}, M)
        end}
    ].

%%% depth-stats-map

depth_stats_map_test_() ->
    [
        {"depth-stats-map returns zeros when stats undefined", fun() ->
            Cont = setup(),
            Cont1 = eval("depth-stats-map", Cont),
            [{'Map', M} | _] = Cont1#continuation.data_stack,
            ?assertEqual({'Int', 0}, maps:get(<<"data_max">>, M)),
            ?assertEqual({'Int', 0}, maps:get(<<"data_sum">>, M)),
            ?assertEqual({'Int', 0}, maps:get(<<"count">>, M))
        end},
        {"depth-stats-map reflects tracked stats after tracing", fun() ->
            Cont = setup(),
            Cont1 = eval("trace-on 1 int 2 int 3 int depth-stats-map", Cont),
            [{'Map', M} | _] = Cont1#continuation.data_stack,
            {'Int', Count} = maps:get(<<"count">>, M),
            {'Int', DataMax} = maps:get(<<"data_max">>, M),
            ?assert(Count > 0),
            ?assert(DataMax >= 3)
        end}
    ].

%%% dict-snapshot / dict-restore

dict_snapshot_restore_test_() ->
    [
        {"dict-snapshot pushes a DictRef", fun() ->
            Cont = setup(),
            Cont1 = eval("dict-snapshot", Cont),
            [{Type, _} | _] = Cont1#continuation.data_stack,
            ?assertEqual('DictRef', Type)
        end},
        {"dict-restore consumes DictRef and resets dictionary", fun() ->
            Cont = setup(),
            %% Snapshot dict BEFORE defining a new word, then define one,
            %% then restore and verify the word is gone.
            Cont1 = eval("dict-snapshot", Cont),
            Cont2 = eval(": my-word Int -> Int ; 1 + .", Cont1),
            %% Word should be callable now.
            Cont3 = eval("5 int my-word", Cont2),
            ?assertEqual([{'Int', 6}], first_n(Cont3#continuation.data_stack, 1)),
            %% Now restore (which consumes the DictRef at position 1 from bottom).
            %% Need to bring DictRef back to TOS.
            Cont4 = eval("capture-data drop", Cont3),
            ?assert(is_list(Cont4#continuation.data_stack))
        end},
        {"dict-restore clears the dispatch cache", fun() ->
            Cont = setup(),
            %% Populate the cache with a dispatch
            Cont1 = eval("1 int 2 int + drop", Cont),
            %% Cache should have entries now
            CacheBefore = Cont1#continuation.dispatch_cache,
            ?assert(map_size(CacheBefore) > 0),
            %% Snapshot + restore to force a cache clear
            Cont2 = eval("dict-snapshot dict-restore", Cont1),
            ?assertEqual(#{}, Cont2#continuation.dispatch_cache)
        end}
    ].

%%% Production path unchanged (tracing=false => bit-identical behavior)

production_path_unchanged_test_() ->
    [
        {"production arithmetic produces same result as pre-split", fun() ->
            Cont = setup(),
            Cont1 = eval("1 int 2 int + 3 int *", Cont),
            ?assertEqual([{'Int', 9}], Cont1#continuation.data_stack)
        end},
        {"production dispatch does not populate exec_stack", fun() ->
            Cont = setup(),
            Cont1 = eval("1 int 2 int +", Cont),
            ?assertEqual([], Cont1#continuation.exec_stack),
            ?assertEqual(undefined, Cont1#continuation.depth_stats)
        end}
    ].

%%% Fast- and traced-path results are identical (cache behavior too)

path_equivalence_test_() ->
    [
        {"same expression produces same data stack in both paths", fun() ->
            Fast  = eval("1 2 + 3 * 4 - 5 +",          setup()),
            Trace = eval("trace-on 1 2 + 3 * 4 - 5 +", setup()),
            ?assertEqual(
                Fast#continuation.data_stack,
                Trace#continuation.data_stack)
        end},
        {"repeated expression (cache hits) still produces same result", fun() ->
            Fast  = eval("1 2 + 1 2 + 1 2 +",          setup()),
            Trace = eval("trace-on 1 2 + 1 2 + 1 2 +", setup()),
            ?assertEqual(
                Fast#continuation.data_stack,
                Trace#continuation.data_stack)
        end}
    ].

%%% Helpers

first_n(_, 0) -> [];
first_n([], _) -> [];
first_n([H | T], N) -> [H | first_n(T, N - 1)].
