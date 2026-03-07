-module(af_integration_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

%%% ===================================================================
%%% CT Callbacks
%%% ===================================================================

all() ->
    [{group, actor_interactions}, {group, multi_file_programs}].

groups() ->
    [{actor_interactions, [sequence], [
        ping_pong,
        fan_out,
        pipeline,
        server_state
    ]},
    {multi_file_programs, [sequence], [
        load_library,
        word_composition,
        product_type_with_words,
        pattern_matching_fibonacci
    ]}].

init_per_testcase(_TestCase, Config) ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_float:init(),
    af_type_string:init(),
    af_type_atom:init(),
    af_type_list:init(),
    af_type_map:init(),
    af_type_tuple:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_ffi:init(),
    af_type_actor:init(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%% ===================================================================
%%% Helpers
%%% ===================================================================

eval(Input) ->
    eval(Input, af_interpreter:new_continuation()).

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

%%% ===================================================================
%%% Group 1: Actor Interaction Scenarios
%%% ===================================================================

%% ping_pong: Two actors exchange messages back and forth.
%% We use raw spawn/send/receive primitives to create two processes
%% that exchange values.
ping_pong(_Config) ->
    %% We test actors exchanging messages using the low-level spawn/send/receive.
    %% Process A sends a value to Process B, B increments it and sends back.
    Self = self(),

    %% Spawn "pong" actor: receives a value, increments, sends back to sender
    PongPid = spawn(fun() ->
        receive
            {af_msg, {'Message', #{tag := <<"ping">>, data := {'Int', N}}}} ->
                %% Wait for the reply-to actor
                receive
                    {af_msg, {'Actor', #{pid := ReplyPid}}} ->
                        ReplyPid ! {af_msg, {'Message', #{tag => <<"pong">>, data => {'Int', N + 1}}}}
                end
        end
    end),

    %% Spawn "ping" actor: sends value to pong, receives reply, forwards to test process
    PingPid = spawn(fun() ->
        receive
            {af_msg, {'Message', #{tag := <<"start">>, data := {'Int', N}}}} ->
                %% Send ping message to pong
                PongPid ! {af_msg, {'Message', #{tag => <<"ping">>, data => {'Int', N}}}},
                %% Send self as reply-to
                PongPid ! {af_msg, {'Actor', #{pid => self(), type_name => undefined, vocab => #{}}}},
                %% Wait for pong reply
                receive
                    {af_msg, {'Message', #{tag := <<"pong">>, data := Result}}} ->
                        Self ! {test_result, Result}
                end
        end
    end),

    %% Send start message with value 10 to ping
    PingPid ! {af_msg, {'Message', #{tag => <<"start">>, data => {'Int', 10}}}},

    %% Verify ping received pong's reply (10 + 1 = 11)
    receive
        {test_result, {'Int', 11}} -> ok
    after 2000 ->
        ct:fail("Timed out waiting for ping-pong result")
    end.

%% fan_out: One sender dispatches different values to multiple receivers.
fan_out(_Config) ->
    Self = self(),

    %% Spawn 3 receiver actors, each reports received value back to test
    Receivers = lists:map(fun(Id) ->
        spawn(fun() ->
            receive
                {af_msg, Value} ->
                    Self ! {received, Id, Value}
            end
        end)
    end, [1, 2, 3]),

    %% Build ActorForth stack with 3 actors and send different values to each
    [R1, R2, R3] = Receivers,

    %% Use interpreter to send values via the send operation
    C0 = af_interpreter:new_continuation(),

    %% Send 100 to R1
    Actor1 = {'Actor', #{pid => R1, type_name => undefined, vocab => #{}}},
    C1 = C0#continuation{data_stack = [Actor1, {'Int', 100}]},
    C2 = eval("send", C1),
    ?assertEqual([], C2#continuation.data_stack),

    %% Send 200 to R2
    Actor2 = {'Actor', #{pid => R2, type_name => undefined, vocab => #{}}},
    C3 = C2#continuation{data_stack = [Actor2, {'Int', 200}]},
    C4 = eval("send", C3),
    ?assertEqual([], C4#continuation.data_stack),

    %% Send 300 to R3
    Actor3 = {'Actor', #{pid => R3, type_name => undefined, vocab => #{}}},
    C5 = C4#continuation{data_stack = [Actor3, {'Int', 300}]},
    C6 = eval("send", C5),
    ?assertEqual([], C6#continuation.data_stack),

    %% Collect results
    Results = collect_results(3, []),
    ?assertEqual([{1, {'Int', 100}}, {2, {'Int', 200}}, {3, {'Int', 300}}],
                 lists:sort(Results)).

collect_results(0, Acc) -> Acc;
collect_results(N, Acc) ->
    receive
        {received, Id, Value} -> collect_results(N - 1, [{Id, Value} | Acc])
    after 2000 ->
        ct:fail("Timed out waiting for fan_out results")
    end.

%% pipeline: Chain of actors where each transforms and forwards data.
%% A -> B -> C: A doubles, B adds 10, C reports to test process.
pipeline(_Config) ->
    Self = self(),

    %% Actor C: receives final value and reports to test process
    PidC = spawn(fun() ->
        receive
            {af_msg, Value} -> Self ! {pipeline_result, Value}
        end
    end),

    %% Actor B: receives value, adds 10, forwards to C
    PidB = spawn(fun() ->
        receive
            {af_msg, {'Int', N}} ->
                PidC ! {af_msg, {'Int', N + 10}}
        end
    end),

    %% Actor A: receives value, doubles it, forwards to B
    PidA = spawn(fun() ->
        receive
            {af_msg, {'Int', N}} ->
                PidB ! {af_msg, {'Int', N * 2}}
        end
    end),

    %% Send 5 into the pipeline: 5 -> double(5)=10 -> add10(10)=20
    PidA ! {af_msg, {'Int', 5}},

    receive
        {pipeline_result, {'Int', 20}} -> ok
    after 2000 ->
        ct:fail("Timed out waiting for pipeline result")
    end.

%% server_state: Counter server accumulates state across multiple casts.
%% Uses the server/<</>>/call pattern from the actor model.
server_state(_Config) ->
    %% Define Counter product type with a value field
    C1 = eval("type Counter value Int ."),

    %% Define increment (cast) and count (call) words
    C2 = eval(": increment Counter -> Counter ; value 1 int + value! .", C1),
    C3 = eval(": count Counter -> Counter Int ; value .", C2),

    %% Create counter server with initial value 0
    C4 = eval("0 int counter server", C3),
    [{'Actor', #{pid := Pid}}] = C4#continuation.data_stack,
    ?assert(is_process_alive(Pid)),

    %% Cast increment 5 times
    C5 = eval("<< increment >>", C4),
    C6 = eval("<< increment >>", C5),
    C7 = eval("<< increment >>", C6),
    C8 = eval("<< increment >>", C7),
    C9 = eval("<< increment >>", C8),

    %% Call count to verify accumulation (synchronous, no sleep needed)
    C10 = eval("<< count >>", C9),
    [{'Int', 5}, {'Actor', _}] = C10#continuation.data_stack,

    %% Increment 3 more times
    C11 = eval("drop << increment >>", C10),
    C12 = eval("<< increment >>", C11),
    C13 = eval("<< increment >>", C12),

    %% Verify total is 8
    C14 = eval("<< count >>", C13),
    [{'Int', 8}, {'Actor', _}] = C14#continuation.data_stack,

    %% Clean up: stop the actor. Use raw message like existing tests do.
    Pid ! {cast, "stop", []}.

%%% ===================================================================
%%% Group 2: Multi-File Programs
%%% ===================================================================

%% load_library: Load a .a4 library file and use its definitions.
load_library(_Config) ->
    %% Get absolute path to lib_math.a4
    %% Find the samples directory path
    ActualPath = find_samples_path("lib_math.a4"),

    %% Load the library
    LoadCmd = io_lib:format("\"~s\" load", [ActualPath]),
    C1 = eval(lists:flatten(LoadCmd)),

    %% Test square: 5 squared = 25
    C2 = eval("5 int square", C1),
    [{'Int', 25}] = C2#continuation.data_stack,

    %% Test double: 7 doubled = 14
    C3 = eval("7 int double", C1),
    [{'Int', 14}] = C3#continuation.data_stack,

    %% Test cube: 3 cubed = 27
    C4 = eval("3 int cube", C1),
    [{'Int', 27}] = C4#continuation.data_stack.

%% word_composition: Define words that call other words.
word_composition(_Config) ->
    %% Define double
    C1 = eval(": double Int -> Int ; dup + ."),

    %% Verify double works
    C2 = eval("3 int double", C1),
    [{'Int', 6}] = C2#continuation.data_stack,

    %% Define quadruple using double double
    C3 = eval(": quadruple Int -> Int ; double double .", C1),

    %% Verify quadruple works: 5 -> 10 -> 20
    C4 = eval("5 int quadruple", C3),
    [{'Int', 20}] = C4#continuation.data_stack,

    %% Verify with different input: 3 -> 6 -> 12
    C5 = eval("3 int quadruple", C3),
    [{'Int', 12}] = C5#continuation.data_stack,

    %% Define octuple using quadruple
    C6 = eval(": octuple Int -> Int ; quadruple quadruple .", C3),
    C7 = eval("2 int octuple", C6),
    [{'Int', 32}] = C7#continuation.data_stack.

%% product_type_with_words: Define a product type and words that operate on it.
product_type_with_words(_Config) ->
    %% Define Point with x and y fields
    C1 = eval("type Point x Int y Int ."),

    %% Create a point: x=3, y=4
    C2 = eval("3 int 4 int point", C1),
    [{'Point', Fields}] = C2#continuation.data_stack,
    ?assertEqual({'Int', 3}, maps:get(x, Fields)),
    ?assertEqual({'Int', 4}, maps:get(y, Fields)),

    %% Define distance-squared word: x*x + y*y
    C3 = eval(": distance-squared Point -> Point Int ; x dup * swap y dup * rot + .", C1),

    %% Test: 3*3 + 4*4 = 9 + 16 = 25
    C4 = eval("3 int 4 int point distance-squared", C3),
    [{'Int', 25}, {'Point', _}] = C4#continuation.data_stack,

    %% Test with different values: 5*5 + 12*12 = 25 + 144 = 169
    C5 = eval("5 int 12 int point distance-squared", C3),
    [{'Int', 169}, {'Point', _}] = C5#continuation.data_stack.

%% pattern_matching_fibonacci: Multi-clause pattern matching for fibonacci.
pattern_matching_fibonacci(_Config) ->
    %% Define fibonacci with base cases and recursive case
    %% Using the same syntax as samples/fib.a4
    C1 = eval(": fib Int -> Int ; : 0 -> 0 ; : 1 -> 1 ; : Int -> Int ; dup 1 int - fib swap 2 int - fib + ."),

    %% Base cases
    C2 = eval("0 int fib", C1),
    [{'Int', 0}] = C2#continuation.data_stack,

    C3 = eval("1 int fib", C1),
    [{'Int', 1}] = C3#continuation.data_stack,

    %% Small values
    C4 = eval("2 int fib", C1),
    [{'Int', 1}] = C4#continuation.data_stack,

    C5 = eval("3 int fib", C1),
    [{'Int', 2}] = C5#continuation.data_stack,

    C6 = eval("5 int fib", C1),
    [{'Int', 5}] = C6#continuation.data_stack,

    C7 = eval("6 int fib", C1),
    [{'Int', 8}] = C7#continuation.data_stack,

    %% The main target: fib(10) = 55
    C8 = eval("10 int fib", C1),
    [{'Int', 55}] = C8#continuation.data_stack.

%%% ===================================================================
%%% Internal helpers
%%% ===================================================================

%% Find the samples directory by searching common locations.
find_samples_path(Filename) ->
    Candidates = [
        filename:join(["samples", Filename]),
        filename:join(["..", "samples", Filename]),
        filename:join(["..", "..", "samples", Filename])
    ],
    %% Also try relative to the project root via _build path
    ProjectRoot = find_project_root(),
    RootCandidate = filename:join([ProjectRoot, "samples", Filename]),
    AllCandidates = [RootCandidate | Candidates],
    case lists:dropwhile(fun(P) -> not filelib:is_regular(P) end, AllCandidates) of
        [Found | _] -> filename:absname(Found);
        [] -> ct:fail({file_not_found, Filename, AllCandidates})
    end.

find_project_root() ->
    %% Walk up from CWD looking for rebar.config
    find_project_root(filename:absname(".")).

find_project_root("/") -> ".";
find_project_root(Dir) ->
    case filelib:is_regular(filename:join(Dir, "rebar.config")) of
        true -> Dir;
        false -> find_project_root(filename:dirname(Dir))
    end.
