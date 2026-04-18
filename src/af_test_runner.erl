-module(af_test_runner).

-include("token.hrl").
-include("continuation.hrl").
-include("af_error.hrl").

-export([run/1, run/2, run_file/1, run_file/2, discover/1]).

%% Test runner — loads .test.a4 files, extracts tests from the test_registry
%% field populated by the DSL layer, and runs each test.
%%
%% Phase 3 MVP scope:
%%   - Sequential execution (parallelism deferred to a later phase).
%%   - Plain-text output (pass/fail counts + failure detail).
%%   - No dashboard / TAP / JUnit / JSON yet.
%%   - No coverage reporting yet.
%%
%% Each test runs in a fresh continuation copied from the post-load state
%% (so helpers + loaded libraries are available) but with empty stack +
%% empty test registry / scope fields so the test can't "see" other tests.

%%% Public API

%% run(Paths) — Paths :: [file | dir]. Returns {Passed, Failed, Skipped}.
run(Paths) ->
    run(Paths, []).

run(Paths, Opts) ->
    Files = lists:flatmap(fun discover/1, Paths),
    Results = lists:flatmap(fun(F) -> run_file(F, Opts) end, Files),
    report(Results, Opts),
    tally(Results).

%% run_file(Path) — returns a list of per-test result maps for one file.
run_file(Path) ->
    run_file(Path, []).

run_file(Path, Opts) ->
    Verbose = proplists:get_bool(verbose, Opts),
    case Verbose of
        true -> io:format("=> loading ~s~n", [Path]);
        false -> ok
    end,
    try
        %% Reset the interpreter world so each file starts clean. (Phase 3:
        %% with parallelism we'll swap to per-actor continuations instead.)
        af_type:reset(),
        C0 = af_interpreter:new_continuation(),
        %% Auto-load assertion library so `.test.a4` tests can use it
        %% without an explicit `load`.
        C1 = load_testing_lib(C0),
        %% Load the file; the DSL populates test_registry.
        {ok, Content} = file:read_file(Path),
        Tokens = af_parser:parse(binary_to_list(Content), Path),
        CLoaded = af_interpreter:interpret_tokens(Tokens, C1),
        Registry = CLoaded#continuation.test_registry,
        run_registry(Registry, CLoaded, Path, Opts)
    catch
        Class:Reason:Stack ->
            [#{
                path     => Path,
                name     => <<"(file load)">>,
                status   => load_error,
                reason   => {Class, Reason, Stack}
            }]
    end.

%% discover(Path) — expand one input path into a list of .test.a4 files.
discover(Path) ->
    case filelib:is_dir(Path) of
        true  -> filelib:wildcard(filename:join(Path, "**/*.test.a4"));
        false ->
            case filelib:is_regular(Path) of
                true  -> [Path];
                false -> []
            end
    end.

%%% Execution

run_registry([], _Loaded, _Path, _Opts) ->
    [];
run_registry([Spec | Rest], Loaded, Path, Opts) ->
    Result = run_spec(Spec, Loaded, Path, Opts),
    [Result | run_registry(Rest, Loaded, Path, Opts)].

run_spec(Spec, Loaded, Path, Opts) ->
    Name  = maps:get(name, Spec),
    Kind  = maps:get(kind, Spec),
    Body  = maps:get(body, Spec),
    Setup = maps:get(setup, Spec),
    TDown = maps:get(teardown, Spec),
    Group = maps:get(group_path, Spec),
    %% Build a fresh per-test continuation: keep dictionary + cache
    %% (words loaded by the file are visible), zero the stack + registry.
    CTest0 = Loaded#continuation{
        data_stack    = [],
        return_stack  = [],
        word_trace    = [],
        test_scope    = [],
        test_registry = []
    },
    T0 = erlang:monotonic_time(microsecond),
    Res = run_body(Kind, Setup, Body, TDown, CTest0),
    T1 = erlang:monotonic_time(microsecond),
    DurationUs = T1 - T0,
    case proplists:get_bool(verbose, Opts) of
        true -> emit_line(Res, Group, Name, DurationUs);
        false -> emit_line(Res, Group, Name, DurationUs)
    end,
    Res#{
        path        => Path,
        name        => Name,
        group       => Group,
        kind        => Kind,
        duration_us => DurationUs
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
                %% Expected an error, didn't get one → fail.
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
    #{status => fail,
      reason => {wrong_error, Expected, Type, Err}};
classify_error(_Type, Err, positive) ->
    #{status => fail, reason => {error, Err}}.

classify_generic(_Class, Reason, _Stack, {raises, Expected}) ->
    %% A non-af_error exception — accept only if Reason happens to match
    %% the expected atom (simple heuristic for Phase 2).
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

%%% Library loading

load_testing_lib(C) ->
    LibRoot = filename:join(["lib", "testing"]),
    Files = case filelib:is_dir(LibRoot) of
        true  -> filelib:wildcard(filename:join(LibRoot, "*.a4"));
        false -> []
    end,
    %% Exclude .test.a4 files — those are test SOURCES, not library code.
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

%%% Reporting

emit_line(#{status := pass}, Group, Name, Us) ->
    io:format("ok     ~s/~s  ~bus~n", [group_str(Group), Name, Us]);
emit_line(#{status := fail, reason := Reason}, Group, Name, Us) ->
    io:format("not ok ~s/~s  ~bus~n  ~p~n", [group_str(Group), Name, Us, Reason]);
emit_line(#{status := load_error, reason := Reason}, _Group, Name, Us) ->
    io:format("LOAD ERROR ~s  ~bus~n  ~p~n", [Name, Us, Reason]).

group_str([]) -> "(top)";
group_str(Path) -> string:join([binary_to_list(B) || B <- Path], "/").

report(Results, _Opts) ->
    {Passed, Failed, Errors} = count(Results),
    io:format("~n==========~n"),
    io:format("~b passed, ~b failed, ~b errors~n~n",
              [Passed, Failed, Errors]).

count(Results) ->
    lists:foldl(fun
        (#{status := pass},       {P, F, E}) -> {P+1, F, E};
        (#{status := fail},       {P, F, E}) -> {P, F+1, E};
        (#{status := load_error}, {P, F, E}) -> {P, F, E+1}
    end, {0, 0, 0}, Results).

tally(Results) ->
    {P, F, E} = count(Results),
    {P, F, E}.
