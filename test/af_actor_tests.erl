-module(af_actor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_string:init(),
    af_type_map:init(),
    af_type_list:init(),
    af_type_actor:init(),
    af_type_ffi:init().

%% --- server spawns actor from type instance ---

server_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"server spawns actor from product type", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval("0 int counter server", C1),
            [{'Actor', Info}] = C2#continuation.data_stack,
            ?assert(is_pid(maps:get(pid, Info))),
            ?assertEqual('Counter', maps:get(type_name, Info)),
            %% Clean up
            maps:get(pid, Info) ! {cast, "stop", []}
        end} end,
        fun(_) -> {"server builds vocab excluding auto-generated ops", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            %% With non-destructive getters, no need for dup before value
            C2 = eval(": increment Counter -> Counter ; value 1 int + value! .", C1),
            C3 = eval("0 int counter server", C2),
            [{'Actor', Info}] = C3#continuation.data_stack,
            Vocab = maps:get(vocab, Info),
            ?assert(maps:is_key("increment", Vocab)),
            ?assertNot(maps:is_key("value", Vocab)),
            maps:get(pid, Info) ! {cast, "stop", []}
        end} end,
        fun(_) -> {"vocab classifies cast vs call correctly", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": increment Counter -> Counter ; value 1 int + value! .", C1),
            C3 = eval(": count Counter -> Counter Int ; value .", C2),
            C4 = eval("0 int counter server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            Vocab = maps:get(vocab, Info),
            [IncrEntry] = maps:get("increment", Vocab),
            ?assertEqual([], maps:get(args, IncrEntry)),
            ?assertEqual([], maps:get(returns, IncrEntry)),
            [CountEntry] = maps:get("count", Vocab),
            ?assertEqual([], maps:get(args, CountEntry)),
            ?assertEqual(['Int'], maps:get(returns, CountEntry)),
            maps:get(pid, Info) ! {cast, "stop", []}
        end} end
    ]}.

%% --- << ... >> basic messaging ---

messaging_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"cast via << word >> sends async message", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": increment Counter -> Counter ; value 1 int + value! .", C1),
            C3 = eval(": count Counter -> Counter Int ; value .", C2),
            C4 = eval("0 int counter server", C3),
            C5 = eval("<< increment >>", C4),
            [{'Actor', _}] = C5#continuation.data_stack,
            C6 = eval("<< count >>", C5),
            [{'Int', 1}, {'Actor', _}] = C6#continuation.data_stack,
            eval("<< stop >>", C6),
            ok
        end} end,
        fun(_) -> {"call via << word >> returns values", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": count Counter -> Counter Int ; value .", C1),
            C3 = eval("0 int counter server", C2),
            C4 = eval("<< count >>", C3),
            [{'Int', 0}, {'Actor', _}] = C4#continuation.data_stack,
            eval("<< stop >>", C4),
            ok
        end} end,
        fun(_) -> {"multiple casts in sequence", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": increment Counter -> Counter ; value 1 int + value! .", C1),
            C3 = eval(": count Counter -> Counter Int ; value .", C2),
            C4 = eval("0 int counter server", C3),
            C5 = eval("<< increment >>", C4),
            C6 = eval("<< increment >>", C5),
            C7 = eval("<< increment >>", C6),
            C8 = eval("<< count >>", C7),
            [{'Int', 3}, {'Actor', _}] = C8#continuation.data_stack,
            eval("<< stop >>", C8),
            ok
        end} end,
        fun(_) -> {"cast with args via << value word >>", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": add Counter Int -> Counter ; swap value rot + value! .", C1),
            C3 = eval(": count Counter -> Counter Int ; value .", C2),
            C4 = eval("0 int counter server", C3),
            C5 = eval("<< 5 int add >>", C4),
            C6 = eval("<< count >>", C5),
            [{'Int', 5}, {'Actor', _}] = C6#continuation.data_stack,
            eval("<< stop >>", C6),
            ok
        end} end,
        fun(_) -> {"chained commands in single << block", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": add Counter Int -> Counter ; swap value rot + value! .", C1),
            C3 = eval(": count Counter -> Counter Int ; value .", C2),
            C4 = eval("0 int counter server", C3),
            C5 = eval("<< 5 int add 10 int add >>", C4),
            C6 = eval("<< count >>", C5),
            [{'Int', 15}, {'Actor', _}] = C6#continuation.data_stack,
            eval("<< stop >>", C6),
            ok
        end} end
    ]}.

%% --- stop protocol ---

stop_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"stop terminates actor process", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval("0 int counter server", C1),
            [{'Actor', #{pid := Pid}}] = C2#continuation.data_stack,
            ?assert(is_process_alive(Pid)),
            eval("<< stop >>", C2),
            timer:sleep(50),
            ?assertNot(is_process_alive(Pid))
        end} end
    ]}.

%% --- state privacy ---

privacy_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"auto-generated getter not in actor vocab", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval("0 int counter server", C1),
            [{'Actor', Info}] = C2#continuation.data_stack,
            Vocab = maps:get(vocab, Info),
            ?assertNot(maps:is_key("value", Vocab)),
            maps:get(pid, Info) ! {cast, "stop", []}
        end} end,
        fun(_) -> {"auto-generated setter not in actor vocab", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval("0 int counter server", C1),
            [{'Actor', Info}] = C2#continuation.data_stack,
            Vocab = maps:get(vocab, Info),
            ?assertNot(maps:is_key("value!", Vocab)),
            maps:get(pid, Info) ! {cast, "stop", []}
        end} end,
        fun(_) -> {"non-vocab word inside << executes locally, not on actor", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": count Counter -> Counter Int ; value .", C1),
            C3 = eval("0 int counter server", C2),
            C4 = eval("<< 5 int drop count >>", C3),
            [{'Int', 0}, {'Actor', _}] = C4#continuation.data_stack,
            eval("<< stop >>", C4),
            ok
        end} end
    ]}.

%% --- real-world example: simple key-value store ---

kv_store_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"key-value store actor", fun() ->
            C1 = eval("type Gauge level Int .", af_interpreter:new_continuation()),
            C2 = eval(": set Gauge Int -> Gauge ; level! .", C1),
            C3 = eval(": get Gauge -> Gauge Int ; level .", C2),
            C4 = eval(": reset Gauge -> Gauge ; 0 int level! .", C3),
            C5 = eval("0 int gauge server", C4),
            C6 = eval("<< 42 int set >>", C5),
            C7 = eval("<< get >>", C6),
            [{'Int', 42}, {'Actor', _}] = C7#continuation.data_stack,
            C8 = eval("drop << reset >>", C7),
            C9 = eval("<< get >>", C8),
            [{'Int', 0}, {'Actor', _}] = C9#continuation.data_stack,
            eval("<< stop >>", C9),
            ok
        end} end
    ]}.

%% --- real-world example: event counter (like EventBridge stats) ---

event_counter_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"event counter with multiple named counters", fun() ->
            C1 = eval("type Stats hits Int errors Int .", af_interpreter:new_continuation()),
            C2 = eval(": hit Stats -> Stats ; hits 1 int + hits! .", C1),
            C3 = eval(": err Stats -> Stats ; errors 1 int + errors! .", C2),
            C4 = eval(": report Stats -> Stats Int Int ; hits swap errors rot .", C3),
            C5 = eval("0 int 0 int stats server", C4),
            C6 = eval("<< hit >>", C5),
            C7 = eval("<< hit >>", C6),
            C8 = eval("<< hit >>", C7),
            C9 = eval("<< err >>", C8),
            C10 = eval("<< report >>", C9),
            [{'Int', 3}, {'Int', 1}, {'Actor', _}] = C10#continuation.data_stack,
            eval("<< stop >>", C10),
            ok
        end} end
    ]}.

%% --- self ---

self_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"self pushes Actor with current pid", fun() ->
            C1 = eval("self", af_interpreter:new_continuation()),
            [{'Actor', Info}] = C1#continuation.data_stack,
            ?assertEqual(self(), maps:get(pid, Info))
        end} end
    ]}.
