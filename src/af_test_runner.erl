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
    MatchPattern = proplists:get_value(match, Opts),
    TagLabel = proplists:get_value(tag, Opts),
    %% Load all files up-front so the dashboard can know the total count.
    LoadedRaw = [load_file(F) || F <- Files],
    %% Apply --match and --tag filters to each file's registry.
    LoadedFiltered = [apply_tag_filter(
                         apply_match_filter(L, MatchPattern),
                         TagLabel)
                      || L <- LoadedRaw],
    Loaded = [enable_tracing(L, Coverage) || L <- LoadedFiltered],
    Total = lists:sum([length(maps:get(registry, L, [])) || L <- Loaded,
                                                           is_map(L)]),
    Dashboard = af_test_dashboard:start(Mode, Total),
    AllResults = lists:flatmap(fun(L) -> execute_loaded(L, Dashboard) end,
                               Loaded),
    {Passed, Failed, Errors} = af_test_dashboard:stop(Dashboard),
    maybe_print_seed(Seed, Mode),
    CovSummary = summarize_coverage(AllResults, Coverage, Mode),
    CovResult = gate_coverage(CovSummary, Threshold, LaxCoverage, Mode),
    maybe_emit_structured(AllResults, Opts),
    {Passed, Failed + case CovResult of fail -> 1; ok -> 0 end, Errors}.

enable_tracing(#{cont := C} = Loaded, true) ->
    Loaded#{cont => C#continuation{tracing = true, coverage = #{}}};
enable_tracing(Loaded, _) -> Loaded.

%% Filter registry by name-substring match (if pattern set). Applies
%% simple substring matching; regex is a follow-up.
apply_match_filter(#{registry := _} = Loaded, undefined) ->
    Loaded;
apply_match_filter(#{registry := Reg} = Loaded, Pattern) ->
    PatBin = case is_list(Pattern) of
        true -> list_to_binary(Pattern);
        false -> Pattern
    end,
    Filtered = [S || S <- Reg,
                     binary:match(maps:get(name, S, <<>>), PatBin) =/= nomatch],
    Loaded#{registry => Filtered};
apply_match_filter(Loaded, _) -> Loaded.

apply_tag_filter(#{registry := _} = Loaded, undefined) ->
    Loaded;
apply_tag_filter(#{registry := Reg} = Loaded, Label) ->
    LabelBin = case is_list(Label) of
        true -> list_to_binary(Label);
        false -> Label
    end,
    Filtered = [S || S <- Reg,
                     lists:any(fun(T) -> T =:= LabelBin end,
                               maps:get(tags, S, []))],
    Loaded#{registry => Filtered};
apply_tag_filter(Loaded, _) -> Loaded.

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
    Skip  = maps:get(skip, Spec, undefined),
    MaxDepth = maps:get(max_depth, Spec, undefined),
    MaxRetDepth = maps:get(max_return_depth, Spec, undefined),
    CTest0 = Loaded#continuation{
        data_stack    = [],
        return_stack  = [],
        word_trace    = [],
        coverage      = #{},
        test_registry = []
    },
    T0 = erlang:monotonic_time(microsecond),
    Res0 = case Skip of
        undefined ->
            run_body(Kind, Setup, Body, TDown, CTest0);
        Reason ->
            #{status => skip, reason => Reason}
    end,
    Res = check_depth(Res0, MaxDepth, MaxRetDepth),
    T1 = erlang:monotonic_time(microsecond),
    DurationUs = T1 - T0,
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

check_depth(#{status := pass, final := #continuation{depth_stats = Stats}} = R,
            MaxD, MaxRD)
    when Stats =/= undefined, (MaxD =/= undefined orelse MaxRD =/= undefined) ->
    DataMax = Stats#depth_stats.data_max,
    RetMax  = Stats#depth_stats.return_max,
    case over_limit(DataMax, MaxD) orelse over_limit(RetMax, MaxRD) of
        false -> R;
        true ->
            #{status => fail,
              reason => {depth_exceeded,
                         #{data_max => DataMax, max_depth => MaxD,
                           return_max => RetMax, max_return_depth => MaxRD}}}
    end;
check_depth(R, _, _) -> R.

over_limit(_, undefined) -> false;
over_limit(Actual, Limit) when Actual > Limit -> true;
over_limit(_, _) -> false.

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
        C1 = af_interpreter:interpret_tokens(Setup, C0),
        C2 = af_interpreter:interpret_tokens(Body, C1),
        C3 = af_interpreter:interpret_tokens(Teardown, C2),
        case Kind of
            positive ->
                #{status => pass, final => C3};
            {raises, Expected} ->
                with_diagnosis(
                    #{status => fail,
                      reason => {expected_error, Expected, got_none}},
                    C3#continuation.data_stack)
        end
    catch
        throw:#af_error{type = Type, stack = S} = Err ->
            with_diagnosis(classify_error(Type, Err, Kind), S);
        error:#af_error{type = Type, stack = S} = Err ->
            with_diagnosis(classify_error(Type, Err, Kind), S);
        Class:Reason:Trace ->
            with_diagnosis(classify_generic(Class, Reason, Trace, Kind), [])
    end.

with_diagnosis(#{status := pass} = R, _) -> R;
with_diagnosis(#{status := fail} = R, Stack) ->
    case diagnose(Stack) of
        no_match -> R;
        Cat      -> R#{diagnosis => Cat}
    end.

%% Six-category diagnostic engine mirroring lib/testing/diagnose.a4.
%% First match wins. Runs over the fail-time stack snapshot from the
%% af_error record (no depth stats needed here — suddenly-deep is
%% coverage-only and fires via run_single's depth check).
diagnose(Stack) ->
    case find_atom(Stack) of
        {ok, AV} -> {extra_atom, AV};
        none ->
            case silent_type_mismatch(Stack) of
                {ok, Mismatch} -> Mismatch;
                none ->
                    case Stack of
                        [] -> no_match;
                        L  -> {missing_consumer, length(L)}
                    end
            end
    end.

find_atom([]) -> none;
find_atom([{'Atom', V} | _]) -> {ok, V};
find_atom([_ | Rest]) -> find_atom(Rest).

silent_type_mismatch([{'String', S}, {'Int', _} | _]) ->
    {ok, {silent_type_mismatch, expected_int_got_string, S}};
silent_type_mismatch([{'Int', _}, {'String', S} | _]) ->
    {ok, {silent_type_mismatch, expected_int_got_string, S}};
silent_type_mismatch(_) -> none.

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

group_str([]) -> "(top)";
group_str(Path) ->
    string:join([case B of
        Bin when is_binary(Bin) -> binary_to_list(Bin);
        L -> L
    end || B <- Path], "/").

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

%%% Structured output

maybe_emit_structured(Results, Opts) ->
    case proplists:get_bool(tap, Opts) of
        true  -> emit_tap(Results, standard_io);
        false -> ok
    end,
    case proplists:get_value(junit, Opts) of
        undefined -> ok;
        JUnitPath -> emit_junit(Results, JUnitPath)
    end,
    case proplists:get_value(json, Opts) of
        undefined -> ok;
        JsonPath  -> emit_json(Results, JsonPath)
    end.

emit_tap(Results, Io) ->
    io:format(Io, "TAP version 13~n", []),
    io:format(Io, "1..~b~n", [length(Results)]),
    lists:foldl(fun(R, N) ->
        emit_tap_line(Io, N, R),
        N + 1
    end, 1, Results),
    ok.

emit_tap_line(Io, N, #{status := pass, group := G, name := Name}) ->
    io:format(Io, "ok ~b - ~s/~s~n", [N, group_str(G), Name]);
emit_tap_line(Io, N, #{status := skip, group := G, name := Name,
                       reason := Reason}) ->
    io:format(Io, "ok ~b - ~s/~s # SKIP ~s~n",
              [N, group_str(G), Name, Reason]);
emit_tap_line(Io, N, #{status := fail, group := G, name := Name,
                       reason := Reason}) ->
    io:format(Io, "not ok ~b - ~s/~s~n  ---~n  reason: ~p~n  ---~n",
              [N, group_str(G), Name, Reason]);
emit_tap_line(Io, N, #{status := load_error, name := Name,
                       reason := Reason}) ->
    io:format(Io, "not ok ~b - ~s (load error)~n  ---~n  reason: ~p~n  ---~n",
              [N, Name, Reason]).

emit_junit(Results, Path) ->
    ByGroup = group_results(Results),
    {ok, File} = file:open(Path, [write, binary]),
    ok = file:write(File, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>),
    ok = file:write(File, <<"<testsuites>\n">>),
    lists:foreach(fun({GName, Tests}) ->
        emit_junit_suite(File, GName, Tests)
    end, ByGroup),
    ok = file:write(File, <<"</testsuites>\n">>),
    file:close(File).

group_results(Results) ->
    Map = lists:foldl(fun(R, Acc) ->
        Key = group_str(maps:get(group, R, [])),
        maps:update_with(Key, fun(L) -> [R | L] end, [R], Acc)
    end, #{}, Results),
    [{K, lists:reverse(V)} || {K, V} <- maps:to_list(Map)].

emit_junit_suite(File, GName, Tests) ->
    Failures = length([1 || #{status := fail} <- Tests]),
    Errors = length([1 || #{status := load_error} <- Tests]),
    TotalUs = lists:sum([maps:get(duration_us, T, 0) || T <- Tests]),
    file:write(File,
        io_lib:format(
          "  <testsuite name=\"~s\" tests=\"~b\" failures=\"~b\" errors=\"~b\" time=\"~.3f\">~n",
          [xml_escape(GName), length(Tests), Failures, Errors, TotalUs / 1.0e6])),
    lists:foreach(fun(T) ->
        emit_junit_case(File, GName, T)
    end, Tests),
    file:write(File, <<"  </testsuite>\n">>).

emit_junit_case(File, GName, #{status := pass, name := Name, duration_us := Us}) ->
    file:write(File,
        io_lib:format("    <testcase classname=\"~s\" name=\"~s\" time=\"~.3f\"/>~n",
                      [xml_escape(GName), xml_escape(bin(Name)), Us / 1.0e6]));
emit_junit_case(File, GName, #{status := skip, name := Name, reason := Reason}) ->
    file:write(File,
        io_lib:format("    <testcase classname=\"~s\" name=\"~s\">~n"
                      "      <skipped message=\"~s\"/>~n"
                      "    </testcase>~n",
                      [xml_escape(GName), xml_escape(bin(Name)),
                       xml_escape(bin(Reason))]));
emit_junit_case(File, GName, #{status := fail, name := Name,
                               duration_us := Us, reason := Reason}) ->
    ReasonStr = io_lib:format("~p", [Reason]),
    file:write(File,
        io_lib:format("    <testcase classname=\"~s\" name=\"~s\" time=\"~.3f\">~n"
                      "      <failure message=\"test failed\">~s</failure>~n"
                      "    </testcase>~n",
                      [xml_escape(GName), xml_escape(bin(Name)), Us / 1.0e6,
                       xml_escape(lists:flatten(ReasonStr))]));
emit_junit_case(File, _GName, #{status := load_error, name := Name,
                                reason := Reason}) ->
    ReasonStr = io_lib:format("~p", [Reason]),
    file:write(File,
        io_lib:format("    <testcase classname=\"file\" name=\"~s\">~n"
                      "      <error message=\"load error\">~s</error>~n"
                      "    </testcase>~n",
                      [xml_escape(bin(Name)), xml_escape(lists:flatten(ReasonStr))])).

bin(B) when is_binary(B) -> binary_to_list(B);
bin(L) when is_list(L)   -> L;
bin(Other)               -> io_lib:format("~p", [Other]).

xml_escape(S) when is_list(S) ->
    lists:flatten([case C of
        $< -> "&lt;";
        $> -> "&gt;";
        $& -> "&amp;";
        $" -> "&quot;";
        Other -> [Other]
    end || C <- S]);
xml_escape(Other) -> xml_escape(bin(Other)).

emit_json(Results, Path) ->
    JSON = lists:flatten([
        "{\n",
        "  \"summary\": ", json_summary(Results), ",\n",
        "  \"tests\": [\n",
        string:join([json_test(R) || R <- Results], ",\n"),
        "\n  ]\n",
        "}\n"
    ]),
    file:write_file(Path, JSON).

json_summary(Results) ->
    Passed = length([1 || #{status := pass} <- Results]),
    Failed = length([1 || #{status := fail} <- Results]),
    Errors = length([1 || #{status := load_error} <- Results]),
    Skipped = length([1 || #{status := skip} <- Results]),
    io_lib:format("{\"passed\": ~b, \"failed\": ~b, \"errors\": ~b, \"skipped\": ~b}",
                  [Passed, Failed, Errors, Skipped]).

json_test(#{status := S, name := Name, group := G} = R) ->
    Us = maps:get(duration_us, R, 0),
    Name1 = json_escape(bin(Name)),
    Group1 = json_escape(group_str(G)),
    Base = io_lib:format(
        "    {\"status\": \"~s\", \"group\": \"~s\", \"name\": \"~s\", \"duration_us\": ~b",
        [S, Group1, Name1, Us]),
    Extra = case S of
        pass -> "";
        skip ->
            io_lib:format(", \"reason\": \"~s\"",
                          [json_escape(bin(maps:get(reason, R, "")))]);
        _ ->
            io_lib:format(", \"reason\": \"~s\"",
                          [json_escape(lists:flatten(io_lib:format("~p",
                                                                    [maps:get(reason, R, none)])))])
    end,
    [Base, Extra, "}"].

json_escape(S) when is_list(S) ->
    lists:flatten([case C of
        $"  -> "\\\"";
        $\\ -> "\\\\";
        $\n -> "\\n";
        $\r -> "\\r";
        $\t -> "\\t";
        Other when Other >= 32 -> [Other];
        _ -> ""
    end || C <- S]);
json_escape(Other) -> json_escape(bin(Other)).


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
