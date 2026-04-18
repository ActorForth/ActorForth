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
    Coverage = proplists:get_bool(coverage, Opts),
    Threshold = proplists:get_value(coverage_threshold, Opts, 95),
    LaxCoverage = proplists:get_bool(lax_coverage, Opts),
    %% Load all files up-front so the dashboard can know the total count.
    LoadedRaw = [load_file(F) || F <- Files],
    Loaded = [enable_tracing(L, Coverage) || L <- LoadedRaw],
    Total = lists:sum([length(maps:get(registry, L, [])) || L <- Loaded,
                                                           is_map(L)]),
    Dashboard = af_test_dashboard:start(Mode, Total),
    AllResults = lists:flatmap(fun(L) -> execute_loaded(L, Dashboard) end,
                               Loaded),
    {Passed, Failed, Errors} = af_test_dashboard:stop(Dashboard),
    maybe_print_seed(Seed, Mode),
    CovSummary = summarize_coverage(AllResults, Coverage, Mode),
    CovResult = gate_coverage(CovSummary, Threshold, LaxCoverage, Mode),
    {Passed, Failed + case CovResult of fail -> 1; ok -> 0 end, Errors}.

enable_tracing(#{cont := C} = Loaded, true) ->
    Loaded#{cont => C#continuation{tracing = true, coverage = #{}}};
enable_tracing(Loaded, _) -> Loaded.

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
        Tokens0 = af_parser:parse(binary_to_list(Content), Path),
        Tokens = maybe_wrap_outer_group(Path, Tokens0),
        CLoaded = af_interpreter:interpret_tokens(Tokens, C1),
        #{path => Path, cont => CLoaded,
          registry => CLoaded#continuation.test_registry}
    catch
        Class:Reason:Stack ->
            #{path => Path, error => {Class, Reason, Stack}}
    end.

%% `.test.a4` semantics: if the file doesn't already open with an
%% explicit group, synthesize one from the file's basename. Tests in
%% `samples/list_ops.test.a4` implicitly live under `list_ops`.
maybe_wrap_outer_group(Path, Tokens) ->
    case is_test_file(Path) andalso not has_explicit_group(Tokens) of
        true  -> wrap_with_group(Path, Tokens);
        false -> Tokens
    end.

has_explicit_group([#token{value = _Name, quoted = false},
                    #token{value = "group"} | _]) -> true;
has_explicit_group([#token{value = _Name, quoted = false},
                    #token{value = "group-serial"} | _]) -> true;
has_explicit_group(_) -> false.

wrap_with_group(Path, Tokens) ->
    Base = basename_noext(Path),
    Open = [#token{value = Base,    file = Path, line = 0, column = 0, quoted = false},
            #token{value = "group", file = Path, line = 0, column = 0, quoted = false}],
    Close = [#token{value = ".",    file = Path, line = 0, column = 0, quoted = false}],
    Open ++ Tokens ++ Close.

basename_noext(Path) ->
    Base = filename:basename(Path),
    case lists:suffix(".test.a4", Base) of
        true ->
            lists:sublist(Base, length(Base) - length(".test.a4"));
        false ->
            filename:rootname(Base)
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
        coverage      = #{},
        test_registry = []
    },
    T0 = erlang:monotonic_time(microsecond),
    Res = run_body(Kind, Setup, Body, TDown, CTest0),
    T1 = erlang:monotonic_time(microsecond),
    DurationUs = T1 - T0,
    %% Pull coverage out of the final continuation (only present on
    %% successful runs). Skipped tests / crashed tests contribute none.
    Cov = case Res of
        #{final := #continuation{coverage = C}} -> C;
        _ -> #{}
    end,
    BaseRes = maps:remove(final, Res),
    BaseRes#{
        path        => Path,
        name        => Name,
        group       => Group,
        kind        => Kind,
        duration_us => DurationUs,
        pid         => self(),
        coverage    => Cov
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

%%% Coverage aggregation + gate

summarize_coverage(_Results, false, _Mode) -> undefined;
summarize_coverage(Results, true, Mode) ->
    MergedAll = lists:foldl(fun(R, Acc) ->
        merge_coverage(maps:get(coverage, R, #{}), Acc)
    end, #{}, Results),
    %% Exclude .test.a4 and lib/testing/ wrappers from coverage — tests
    %% don't measure themselves, and the DSL itself is exercised by
    %% construction on every run.
    Merged = maps:filter(fun(FileBin, _) ->
        is_production_source(FileBin)
    end, MergedAll),
    Visited = total_visited(Merged),
    Total = estimate_total_positions(),
    Pct = case Total of 0 -> 100; _ -> (Visited * 100) div Total end,
    case Mode of
        quiet -> ok;
        _ ->
            io:format("  coverage: ~b / ~b positions visited (~b%)~n",
                      [Visited, Total, Pct]),
            lists:foreach(fun({File, Positions}) ->
                io:format("    ~s: ~b unique positions~n",
                          [File, maps:size(Positions)])
            end, lists:sort(maps:to_list(Merged)))
    end,
    #{visited => Visited, total => Total, pct => Pct, files => Merged}.

is_production_source(FileBin) ->
    F = case is_binary(FileBin) of
        true  -> binary_to_list(FileBin);
        false -> FileBin
    end,
    not lists:suffix(".test.a4", F)
        andalso string:str(F, "lib/testing/") =:= 0.

merge_coverage(PerTest, Acc) ->
    maps:fold(fun(File, Positions, A) ->
        Existing = maps:get(File, A, #{}),
        Combined = maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end,
                                    Existing, Positions),
        A#{File => Combined}
    end, Acc, PerTest).

total_visited(Merged) ->
    lists:sum([maps:size(P) || P <- maps:values(Merged)]).

%% Estimate total parseable positions by counting tokens in every .a4
%% file under lib/. Tests exclude their own .test.a4 files. O(files).
estimate_total_positions() ->
    Files = filelib:wildcard("lib/**/*.a4"),
    Prod = [F || F <- Files, is_production_source(F)],
    lists:sum([count_tokens_in_file(F) || F <- Prod]).

count_tokens_in_file(Path) ->
    try
        {ok, Bin} = file:read_file(Path),
        Tokens = af_parser:parse(binary_to_list(Bin), Path),
        length(Tokens)
    catch _:_ -> 0 end.

gate_coverage(undefined, _Threshold, _Lax, _Mode) -> ok;
gate_coverage(_, _T, true, _Mode) -> ok;
gate_coverage(#{pct := Pct}, Threshold, false, Mode) when Pct >= Threshold ->
    case Mode of
        quiet -> ok;
        _ -> io:format("  coverage ok: ~b% >= ~b%~n", [Pct, Threshold])
    end,
    ok;
gate_coverage(#{pct := Pct}, Threshold, false, Mode) ->
    case Mode of
        quiet -> ok;
        _ -> io:format(standard_error,
                       "  coverage FAIL: ~b% < ~b%~n", [Pct, Threshold])
    end,
    fail.


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
