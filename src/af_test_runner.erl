-module(af_test_runner).

-include("token.hrl").
-include("continuation.hrl").
-include("af_error.hrl").

-export([run/1, run/2, run_file/1, run_file/2, discover/1]).

%% Test runner — loads .test.a4 files, extracts test specs from the
%% test_registry field populated by the DSL layer, runs each spec in a
%% separate process (actor), and aggregates results.
%%
%% Parallelism model:
%%   - Each non-serial test spawns one actor; all actors run concurrently.
%%   - Tests inside a `group-serial` group share one actor that runs them
%%     sequentially (but concurrently with everything else).
%%   - Each actor inherits a copy of the post-load continuation (so
%%     helpers and libraries are available) with stack / registry / scope
%%     cleared.
%%
%% All output is channelled through the coordinator so parallel runs
%% produce well-ordered streams. Phase-3 MVP keeps plain-text streaming;
%% richer output formats land in follow-up patches.

%%% Public API

run(Paths) -> run(Paths, []).

run(Paths, Opts) ->
    Files = lists:flatmap(fun discover/1, Paths),
    Seed  = ensure_seed(Opts),
    Mode  = resolve_mode(Opts),
    %% Load all files up-front so the dashboard can know the total count.
    Loaded = [load_file(F) || F <- Files],
    Total = lists:sum([length(maps:get(registry, L, [])) || L <- Loaded,
                                                           is_map(L)]),
    Dashboard = af_test_dashboard:start(Mode, Total),
    AllResults = lists:flatmap(fun(L) -> execute_loaded(L, Dashboard) end,
                               Loaded),
    {Passed, Failed, Errors} = af_test_dashboard:stop(Dashboard),
    maybe_print_seed(Seed, Mode),
    _ = {Passed, Failed, Errors},
    tally(AllResults).

run_file(Path) -> run_file(Path, []).

run_file(Path, Opts) ->
    Seed = ensure_seed(Opts),
    Mode = resolve_mode(Opts),
    Loaded = load_file(Path),
    Total = case Loaded of
        #{registry := R} -> length(R);
        _ -> 0
    end,
    Dashboard = af_test_dashboard:start(Mode, Total),
    Results = execute_loaded(Loaded, Dashboard),
    _ = af_test_dashboard:stop(Dashboard),
    maybe_print_seed(Seed, Mode),
    Results.

%% Load a single file and return a loaded-record {path, cont, registry} or
%% an error marker to be surfaced as a load_error result.
load_file(Path) ->
    try
        af_type:reset(),
        C0 = af_interpreter:new_continuation(),
        C1 = load_testing_lib(C0),
        {ok, Content} = file:read_file(Path),
        Tokens = af_parser:parse(binary_to_list(Content), Path),
        CLoaded = af_interpreter:interpret_tokens(Tokens, C1),
        #{path => Path, cont => CLoaded,
          registry => CLoaded#continuation.test_registry}
    catch
        Class:Reason:Stack ->
            #{path => Path, error => {Class, Reason, Stack}}
    end.

execute_loaded(#{error := Err, path := Path}, Dashboard) ->
    Msg = #{path => Path, name => <<"(file load)">>,
            group => [], kind => positive,
            status => load_error, reason => Err,
            duration_us => 0, pid => self()},
    af_test_dashboard:done(Dashboard, Msg),
    [Msg];
execute_loaded(#{registry := Registry, cont := Loaded, path := Path},
               Dashboard) ->
    execute(Registry, Loaded, Path, Dashboard).

discover(Path) ->
    case filelib:is_dir(Path) of
        true  -> filelib:wildcard(filename:join(Path, "**/*.test.a4"));
        false ->
            case filelib:is_regular(Path) of
                true  -> [Path];
                false -> []
            end
    end.

%%% Parallel execution

execute(Registry, Loaded, Path, Dashboard) ->
    %% Split specs into parallel individuals and serial-group batches.
    {ParTasks, SerialGroups} = partition_specs(Registry),
    Parent = self(),
    PTags = lists:map(fun(Spec) ->
        spawn_one_test(Spec, Loaded, Path, Parent, Dashboard)
    end, ParTasks),
    STags = lists:map(fun({_GP, Specs}) ->
        spawn_serial_group(Specs, Loaded, Path, Parent, Dashboard)
    end, maps:to_list(SerialGroups)),
    ExpectedCount = length(ParTasks)
                  + lists:sum([length(S) || {_, S} <- maps:to_list(SerialGroups)]),
    collect(ExpectedCount, PTags ++ STags, [], Dashboard).

%% Returns {ParallelSpecs, #{GroupPath => [SerialSpecs]}}.
partition_specs(Registry) ->
    lists:foldl(fun(Spec, {Par, Ser}) ->
        case maps:get(serial, Spec, false) of
            false ->
                {[Spec | Par], Ser};
            true ->
                GroupKey = maps:get(group_path, Spec),
                Existing = maps:get(GroupKey, Ser, []),
                {Par, Ser#{GroupKey => Existing ++ [Spec]}}
        end
    end, {[], #{}}, Registry).

spawn_one_test(Spec, Loaded, Path, Parent, Dashboard) ->
    Tag = make_ref(),
    Pid = spawn_link(fun() ->
        af_test_dashboard:started(Dashboard, self(),
                                  maps:get(group_path, Spec),
                                  maps:get(name, Spec)),
        Result = safe_run(Spec, Loaded, Path),
        Parent ! {test_done, Tag, self(), Result}
    end),
    {Tag, Pid, Spec}.

spawn_serial_group(Specs, Loaded, Path, Parent, Dashboard) ->
    Tag = make_ref(),
    Pid = spawn_link(fun() ->
        lists:foreach(fun(Spec) ->
            af_test_dashboard:started(Dashboard, self(),
                                      maps:get(group_path, Spec),
                                      maps:get(name, Spec)),
            Result = safe_run(Spec, Loaded, Path),
            Parent ! {test_done, Tag, self(), Result}
        end, Specs)
    end),
    {Tag, Pid, Specs}.

safe_run(Spec, Loaded, Path) ->
    try
        run_single(Spec, Loaded, Path)
    catch
        Class:Reason:Stack ->
            Base = base_result(Spec, Path),
            Base#{status      => fail,
                  reason      => {crash, {Class, Reason, Stack}},
                  duration_us => 0}
    end.

run_single(Spec, Loaded, Path) ->
    Name  = maps:get(name, Spec),
    Kind  = maps:get(kind, Spec),
    Body  = maps:get(body, Spec),
    Setup = maps:get(setup, Spec),
    TDown = maps:get(teardown, Spec),
    Group = maps:get(group_path, Spec),
    CTest0 = Loaded#continuation{
        data_stack    = [],
        return_stack  = [],
        word_trace    = [],
        test_registry = []
    },
    T0 = erlang:monotonic_time(microsecond),
    Res = run_body(Kind, Setup, Body, TDown, CTest0),
    T1 = erlang:monotonic_time(microsecond),
    DurationUs = T1 - T0,
    Res#{
        path        => Path,
        name        => Name,
        group       => Group,
        kind        => Kind,
        duration_us => DurationUs,
        pid         => self()
    }.

base_result(Spec, Path) ->
    #{
        path  => Path,
        name  => maps:get(name, Spec),
        group => maps:get(group_path, Spec),
        kind  => maps:get(kind, Spec),
        pid   => self()
    }.

run_body(Kind, Setup, Body, Teardown, C0) ->
    try
        C1 = run_tokens(Setup, C0),
        C2 = run_tokens(Body, C1),
        C3 = run_tokens(Teardown, C2),
        case Kind of
            positive ->
                #{status => pass, final => C3};
            {raises, Expected} ->
                #{status => fail,
                  reason => {expected_error, Expected, got_none}}
        end
    catch
        throw:#af_error{type = Type} = Err ->
            classify_error(Type, Err, Kind);
        error:#af_error{type = Type} = Err ->
            classify_error(Type, Err, Kind);
        Class:Reason:Stack ->
            classify_generic(Class, Reason, Stack, Kind)
    end.

classify_error(Type, _Err, {raises, Type}) ->
    #{status => pass};
classify_error(Type, Err, {raises, Expected}) ->
    #{status => fail, reason => {wrong_error, Expected, Type, Err}};
classify_error(_Type, Err, positive) ->
    #{status => fail, reason => {error, Err}}.

classify_generic(_Class, Reason, _Stack, {raises, Expected}) ->
    case Reason of
        {Expected, _}                  -> #{status => pass};
        Expected                       -> #{status => pass};
        _ -> #{status => fail,
               reason => {unexpected_error, Expected, Reason}}
    end;
classify_generic(Class, Reason, Stack, positive) ->
    #{status => fail, reason => {Class, Reason, Stack}}.

run_tokens([], C) -> C;
run_tokens(Tokens, C) when is_list(Tokens) ->
    af_interpreter:interpret_tokens(Tokens, C).

%%% Collect results with streaming output

collect(0, _Tags, Acc, _Dashboard) -> lists:reverse(Acc);
collect(Remaining, Tags, Acc, Dashboard) ->
    receive
        {test_done, _Tag, _Pid, Result} ->
            af_test_dashboard:done(Dashboard, Result),
            collect(Remaining - 1, Tags, [Result | Acc], Dashboard)
    after 60000 ->
        io:format(standard_error, "timeout: ~p tests still pending~n", [Remaining]),
        lists:reverse(Acc)
    end.

%%% Library loading

load_testing_lib(C) ->
    LibRoot = filename:join(["lib", "testing"]),
    Files = case filelib:is_dir(LibRoot) of
        true  -> filelib:wildcard(filename:join(LibRoot, "*.a4"));
        false -> []
    end,
    Libs = [F || F <- Files, not is_test_file(F)],
    lists:foldl(fun load_one/2, C, Libs).

is_test_file(F) ->
    lists:suffix(".test.a4", F).

load_one(File, C) ->
    case file:read_file(File) of
        {ok, Content} ->
            Tokens = af_parser:parse(binary_to_list(Content), File),
            af_interpreter:interpret_tokens(Tokens, C);
        _ -> C
    end.

%%% Output

resolve_mode(Opts) ->
    case proplists:get_value(render, Opts) of
        undefined ->
            case proplists:get_bool(dashboard, Opts) of
                true -> dashboard;
                false ->
                    case proplists:get_bool(quiet, Opts) of
                        true -> quiet;
                        false -> stream
                    end
            end;
        Mode when Mode =:= dashboard; Mode =:= stream; Mode =:= quiet ->
            Mode
    end.

maybe_print_seed(_Seed, quiet) -> ok;
maybe_print_seed(Seed, _Mode) ->
    io:format("  seed=~p~n~n", [Seed]).

count(Results) ->
    lists:foldl(fun
        (#{status := pass},       {P, F, E}) -> {P+1, F, E};
        (#{status := fail},       {P, F, E}) -> {P, F+1, E};
        (#{status := load_error}, {P, F, E}) -> {P, F, E+1}
    end, {0, 0, 0}, Results).

tally(Results) ->
    {P, F, E} = count(Results),
    {P, F, E}.

%%% Seed for reproducible actor ordering

ensure_seed(Opts) ->
    Seed = case proplists:get_value(seed, Opts) of
        undefined ->
            %% Derive from monotonic time; report it so the run can be replayed.
            erlang:monotonic_time();
        S when is_integer(S) -> S
    end,
    rand:seed(exsplus, {Seed, Seed, Seed}),
    Seed.
