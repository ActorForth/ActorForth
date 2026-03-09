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

%% --- Milestone 7.3: spawn / send / receive ---

spawn_send_receive_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"spawn creates actor from word name", fun() ->
            %% Define a word with sig_in so name pushed as Atom (not executed)
            C1 = eval(": echo Int -> ; receive drop .", af_interpreter:new_continuation()),
            C2 = eval("echo spawn", C1),
            [{'Actor', Info}] = C2#continuation.data_stack,
            ?assert(is_pid(maps:get(pid, Info))),
            %% Cleanup: send a message so the spawned process terminates
            Pid = maps:get(pid, Info),
            Pid ! {af_msg, {'Int', 0}},
            timer:sleep(50)
        end} end,
        fun(_) -> {"send delivers value to spawned actor", fun() ->
            %% Spawned process sends back via raw Erlang msg
            Self = self(),
            SelfPid = Self,
            %% Use erlang process directly to test send/receive
            Pid = spawn(fun() ->
                receive
                    {af_msg, Value} -> SelfPid ! {got, Value}
                end
            end),
            ActorVal = #{pid => Pid, type_name => undefined, vocab => #{}},
            C1 = #continuation{data_stack = [{'Actor', ActorVal}, {'Int', 42}]},
            Token = #token{value = "send"},
            C2 = af_interpreter:interpret_token(Token, C1),
            ?assertEqual([], C2#continuation.data_stack),
            receive
                {got, {'Int', 42}} -> ok
            after 1000 ->
                ?assert(false)
            end
        end} end,
        fun(_) -> {"! is alias for send", fun() ->
            Pid = spawn(fun() ->
                receive {af_msg, _} -> ok end
            end),
            ActorVal = #{pid => Pid, type_name => undefined, vocab => #{}},
            C1 = #continuation{data_stack = [{'Actor', ActorVal}, {'Int', 99}]},
            Token = #token{value = "!"},
            C2 = af_interpreter:interpret_token(Token, C1),
            ?assertEqual([], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"receive blocks until message arrives", fun() ->
            Self = self(),
            Pid = spawn(fun() ->
                Cont = #continuation{},
                Token = #token{value = "receive"},
                ResultCont = af_interpreter:interpret_token(Token, Cont),
                [Value] = ResultCont#continuation.data_stack,
                Self ! {received, Value}
            end),
            timer:sleep(50),
            Pid ! {af_msg, {'Int', 777}},
            receive
                {received, {'Int', 777}} -> ok
            after 1000 ->
                ?assert(false)
            end
        end} end,
        fun(_) -> {"receive-timeout returns false on timeout", fun() ->
            C1 = eval("50 int receive-timeout", af_interpreter:new_continuation()),
            [{'Bool', false}, {'Atom', "timeout"}] = C1#continuation.data_stack
        end} end,
        fun(_) -> {"receive-timeout returns value when message arrives", fun() ->
            Self = self(),
            spawn(fun() ->
                timer:sleep(10),
                Self ! {af_msg, {'Int', 42}}
            end),
            C1 = eval("200 int receive-timeout", af_interpreter:new_continuation()),
            [{'Bool', true}, {'Int', 42}] = C1#continuation.data_stack
        end} end
    ]}.

%% --- Message type ---

message_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"msg creates tagged message", fun() ->
            C1 = eval("42 int \"answer\" msg", af_interpreter:new_continuation()),
            [{'Message', #{tag := <<"answer">>, data := {'Int', 42}}}] =
                C1#continuation.data_stack
        end} end,
        fun(_) -> {"msg-tag extracts tag non-destructively", fun() ->
            C1 = eval("42 int \"answer\" msg msg-tag", af_interpreter:new_continuation()),
            [{'String', <<"answer">>}, {'Message', _}] = C1#continuation.data_stack
        end} end,
        fun(_) -> {"msg-data extracts data non-destructively", fun() ->
            C1 = eval("42 int \"answer\" msg msg-data", af_interpreter:new_continuation()),
            [{'Int', 42}, {'Message', _}] = C1#continuation.data_stack
        end} end
    ]}.

%% --- Selective receive ---

selective_receive_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"receive-match selects by tag", fun() ->
            Self = self(),
            spawn(fun() ->
                timer:sleep(10),
                Self ! {af_msg, {'Message', #{tag => <<"noise">>, data => {'Int', 0}}}},
                timer:sleep(10),
                Self ! {af_msg, {'Message', #{tag => <<"target">>, data => {'Int', 42}}}}
            end),
            C1 = eval("\"target\" receive-match", af_interpreter:new_continuation()),
            [{'Int', 42}] = C1#continuation.data_stack,
            %% The noise message is still in mailbox
            receive
                {af_msg, {'Message', #{tag := <<"noise">>}}} -> ok
            after 500 ->
                ?assert(false)
            end
        end} end,
        fun(_) -> {"receive-match-timeout returns false on timeout", fun() ->
            C1 = eval("\"missing\" 50 int receive-match-timeout",
                       af_interpreter:new_continuation()),
            [{'Bool', false}, {'Atom', "timeout"}] = C1#continuation.data_stack
        end} end
    ]}.

%% --- Type-checked dispatch ---

type_checked_dispatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"dispatch validates argument types", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": add Counter Int -> Counter ; swap value rot + value! .", C1),
            C3 = eval("0 int counter server", C2),
            %% Correct type: Int arg
            C4 = eval("<< 5 int add >>", C3),
            [{'Actor', _}] = C4#continuation.data_stack,
            eval("<< stop >>", C4),
            ok
        end} end,
        fun(_) -> {"dispatch rejects wrong argument type", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": add Counter Int -> Counter ; swap value rot + value! .", C1),
            C3 = eval("0 int counter server", C2),
            %% Wrong type: Bool instead of Int
            ?assertError({actor_type_error, "add", _},
                eval("<< true bool add >>", C3))
        end} end
    ]}.

%% --- Multi-clause dispatch (find_matching_entry with multiple entries) ---

multi_clause_dispatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"dispatch with overloaded word picks correct clause", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            %% Define two overloads: add with Int arg and add with Bool arg
            C2 = eval(": add Counter Int -> Counter ; swap value rot + value! .", C1),
            C3 = eval(": toggle Counter Bool -> Counter ; drop value 1 int + value! .", C2),
            C4 = eval(": count Counter -> Counter Int ; value .", C3),
            C5 = eval("0 int counter server", C4),
            C6 = eval("<< 5 int add >>", C5),
            C7 = eval("<< count >>", C6),
            [{'Int', 5}, {'Actor', _}] = C7#continuation.data_stack,
            eval("<< stop >>", C7),
            ok
        end} end,
        fun(_) -> {"dispatch with not enough args raises error", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": add Counter Int -> Counter ; swap value rot + value! .", C1),
            C3 = eval("0 int counter server", C2),
            %% Try to call add with no args on the local stack
            ?assertError({actor_arg_error, "add", _},
                eval("<< add >>", C3))
        end} end
    ]}.

%% --- supervised-server ---

supervised_server_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"supervised-server spawns actor under supervisor", fun() ->
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": count Counter -> Counter Int ; value .", C1),
            C3 = eval("0 int counter supervised-server", C2),
            [{'Actor', Info}] = C3#continuation.data_stack,
            ?assert(is_pid(maps:get(pid, Info))),
            ?assertEqual(true, maps:get(supervised, Info)),
            ?assertEqual('Counter', maps:get(type_name, Info)),
            %% Cleanup: stop the supervised actor
            gen_server:cast(maps:get(pid, Info), {cast, "stop", []})
        end} end
    ]}.

%% --- spawn via ActorForth word ---

spawn_word_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"spawn word creates actor from atom", fun() ->
            %% Define a word with a sig_in so typing its name pushes as Atom
            C1 = eval(": echo-back Int -> ; receive drop .", af_interpreter:new_continuation()),
            %% Now echo-back has sig_in [Int], so typing it without Int won't match
            %% and it gets pushed as Atom
            C2 = eval("echo-back spawn", C1),
            [{'Actor', Info}] = C2#continuation.data_stack,
            ?assert(is_pid(maps:get(pid, Info))),
            %% Cleanup: send a message so the spawned process terminates
            Pid = maps:get(pid, Info),
            Pid ! {af_msg, {'Int', 0}},
            timer:sleep(50)
        end} end
    ]}.

%% --- find_matching_entry / find_typed_match / match_args ---

matching_entry_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"multi-overload word dispatches to correct entry based on arg type", fun() ->
            C1 = eval("type Box value Int .", af_interpreter:new_continuation()),
            %% Define two overloads of 'put' that differ by arg type
            C2 = eval(": put Box Int -> Box ; value! .", C1),
            C3 = eval(": reset Box -> Box ; 0 int value! .", C2),
            C4 = eval(": get Box -> Box Int ; value .", C3),
            C5 = eval("0 int box server", C4),
            %% put with Int arg should hit the first overload
            C6 = eval("<< 42 int put >>", C5),
            C7 = eval("<< get >>", C6),
            [{'Int', 42}, {'Actor', _}] = C7#continuation.data_stack,
            %% reset should hit the no-arg overload
            C8 = eval("drop << reset >>", C7),
            C9 = eval("<< get >>", C8),
            [{'Int', 0}, {'Actor', _}] = C9#continuation.data_stack,
            eval("<< stop >>", C9),
            ok
        end} end
    ]}.

%% --- receive-match-timeout success path ---

receive_match_timeout_success_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"receive-match-timeout returns data when tag matches", fun() ->
            Self = self(),
            spawn(fun() ->
                timer:sleep(10),
                Self ! {af_msg, {'Message', #{tag => <<"hello">>, data => {'Int', 99}}}}
            end),
            C1 = eval("\"hello\" 200 int receive-match-timeout",
                       af_interpreter:new_continuation()),
            [{'Bool', true}, {'Int', 99}] = C1#continuation.data_stack
        end} end
    ]}.

%% --- Compiled actor loop tests ---

compiled_loop_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile_actor_loop returns error for unknown type", fun() ->
            ?assertEqual(error, af_type_actor:compile_actor_loop('NonExistentType'))
        end} end,

        fun(_) -> {"compile_actor_loop generates module for Counter", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            eval("\"bump\" compile", C2),
            {ok, Mod} = af_type_actor:compile_actor_loop('Counter'),
            ?assert(is_atom(Mod)),
            ?assert(erlang:module_loaded(Mod))
        end} end,

        fun(_) -> {"compiled loop handles cast via pattern match", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"bump\" compile", C3),
            eval("\"get_count\" compile", C3),
            C4 = eval("0 counter server", C3),
            [{'Actor', ActorInfo} | _] = C4#continuation.data_stack,
            Pid = maps:get(pid, ActorInfo),
            %% Send atom-keyed bump (from compiled code path)
            Pid ! {cast, bump, []},
            Pid ! {cast, bump, []},
            timer:sleep(50),
            Ref = make_ref(),
            Pid ! {call, get_count, [], self(), Ref},
            receive {reply, Ref, [{'Int', Count}]} ->
                ?assertEqual(2, Count)
            after 2000 -> ?assert(false)
            end,
            Pid ! {cast, stop, []}
        end} end,

        fun(_) -> {"compiled loop handles string stop for backwards compat", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            eval("\"bump\" compile", C2),
            C3 = eval("0 counter server", C2),
            [{'Actor', ActorInfo} | _] = C3#continuation.data_stack,
            Pid = maps:get(pid, ActorInfo),
            MonRef = monitor(process, Pid),
            Pid ! {cast, "stop", []},
            receive {'DOWN', MonRef, process, Pid, _} -> ok
            after 2000 -> ?assert(false)
            end
        end} end,

        fun(_) -> {"actor_loop_module_name generates correct name", fun() ->
            ?assertEqual(af_actor_loop_counter, af_type_actor:actor_loop_module_name('Counter'))
        end} end
    ]}.

%% --- Product field update via actor (regression: bank_actor.a4 bug) ---
%% The pattern "swap dup <getter> rot <op> <setter>" was broken because
%% non-destructive getters push value AND keep instance, making rot produce
%% wrong stack layout. Correct pattern: "over <getter> rot <op> <setter> swap drop"

product_field_update_actor_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"credit updates balance through actor (cast word with args)", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Account balance Int .", C0),
            C2 = eval(": credit Account Int -> Account ; over balance rot + balance! swap drop .", C1),
            C3 = eval(": get-balance Account -> Account Int ; balance .", C2),
            C4 = eval("1000 account server", C3),
            C5 = eval("<< 500 credit >>", C4),
            C6 = eval("<< get-balance >>", C5),
            [{'Int', 1500}, {'Actor', _}] = C6#continuation.data_stack,
            eval("<< stop >>", C6),
            ok
        end} end,

        fun(_) -> {"debit subtracts balance through actor", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Account balance Int .", C0),
            C2 = eval(": credit Account Int -> Account ; over balance rot + balance! swap drop .", C1),
            C3 = eval(": debit Account Int -> Account ; over balance rot - balance! swap drop .", C2),
            C4 = eval(": get-balance Account -> Account Int ; balance .", C3),
            C5 = eval("1000 account server", C4),
            C6 = eval("<< 500 credit >>", C5),
            C7 = eval("<< 200 debit >>", C6),
            C8 = eval("<< get-balance >>", C7),
            [{'Int', 1300}, {'Actor', _}] = C8#continuation.data_stack,
            eval("<< stop >>", C8),
            ok
        end} end,

        fun(_) -> {"multi-word actor pipeline (deposit = add-tx + credit)", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Transaction kind Atom amount Int .", C0),
            C2 = eval("type Account balance Int ledger List .", C1),
            C3 = eval(": make-tx Atom Int -> Transaction ; transaction .", C2),
            C4 = eval(": add-tx Account Transaction -> Account ; over ledger rot cons ledger! swap drop .", C3),
            C5 = eval(": credit Account Int -> Account ; over balance rot + balance! swap drop .", C4),
            C6 = eval(": deposit Account Int -> Account ; dup Deposit swap make-tx rot swap add-tx swap credit .", C5),
            C7 = eval(": get-balance Account -> Account Int ; balance .", C6),
            C8 = eval(": tx-count Account -> Account Int ; ledger length .", C7),
            C9 = eval("1000 nil account server", C8),
            C10 = eval("<< 500 deposit >>", C9),
            C11 = eval("<< 250 deposit >>", C10),
            C12 = eval("<< get-balance >>", C11),
            [{'Int', 1750}, {'Actor', _}] = C12#continuation.data_stack,
            C13 = eval("drop << tx-count >>", C12),
            [{'Int', 2}, {'Actor', _}] = C13#continuation.data_stack,
            eval("<< stop >>", C13),
            ok
        end} end,

        fun(_) -> {"execute_actor_word returns correct state via separate_reply", fun() ->
            %% Regression: execute_actor_word used lists:last which returned wrong item
            %% when word body produced extra stack items due to bugs
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Account balance Int .", C0),
            C2 = eval(": credit Account Int -> Account ; over balance rot + balance! swap drop .", C1),
            Instance = {'Account', #{balance => {'Int', 1000}}},
            NewState = af_type_actor:execute_actor_word("credit", [{'Int', 500}], 'Account', Instance),
            ?assertMatch({'Account', _}, NewState),
            {'Account', Fields} = NewState,
            ?assertEqual({'Int', 1500}, maps:get(balance, Fields))
        end} end,

        fun(_) -> {"execute_actor_call returns reply values and updated state", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Account balance Int .", C0),
            _C2 = eval(": get-balance Account -> Account Int ; balance .", C1),
            Instance = {'Account', #{balance => {'Int', 1000}}},
            {Reply, NewState} = af_type_actor:execute_actor_call("get-balance", [], 'Account', Instance),
            ?assertEqual([{'Int', 1000}], Reply),
            ?assertEqual(Instance, NewState)
        end} end
    ]}.

%% --- actor_loop coverage: direct message dispatch ---

actor_loop_direct_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"actor_loop handles string cast messages (legacy compat)", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            C4 = eval("0 counter server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            Pid = maps:get(pid, Info),
            %% Send string-keyed cast (legacy format) — should be converted to atom
            Pid ! {cast, "bump", []},
            timer:sleep(50),
            %% Query via string-keyed call (legacy format)
            Ref = make_ref(),
            Pid ! {call, "get_count", [], self(), Ref},
            receive {reply, Ref, [{'Int', Count}]} ->
                ?assertEqual(1, Count)
            after 2000 -> ?assert(false)
            end,
            Pid ! {cast, stop, []}
        end} end,

        fun(_) -> {"actor_loop handles atom cast/call with cache hit", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"bump\" compile", C3),
            eval("\"get_count\" compile", C3),
            C4 = eval("0 counter server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            Pid = maps:get(pid, Info),
            %% These should hit the native cache (compiled words)
            Pid ! {cast, bump, []},
            Pid ! {cast, bump, []},
            Pid ! {cast, bump, []},
            timer:sleep(50),
            Ref = make_ref(),
            Pid ! {call, get_count, [], self(), Ref},
            receive {reply, Ref, [{'Int', Count}]} ->
                ?assertEqual(3, Count)
            after 2000 -> ?assert(false)
            end,
            Pid ! {cast, stop, []}
        end} end,

        fun(_) -> {"actor_loop handles cast with args via cache", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": add Counter Int -> Counter ; over count rot + count! swap drop .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"add\" compile", C3),
            eval("\"get_count\" compile", C3),
            C4 = eval("0 counter server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            Pid = maps:get(pid, Info),
            Pid ! {cast, add, [{'Int', 10}]},
            timer:sleep(50),
            Ref = make_ref(),
            Pid ! {call, get_count, [], self(), Ref},
            receive {reply, Ref, [{'Int', Count}]} ->
                ?assertEqual(10, Count)
            after 2000 -> ?assert(false)
            end,
            Pid ! {cast, stop, []}
        end} end,

        fun(_) -> {"actor_loop call with args via cache", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": add-get Counter Int -> Counter Int ; over count rot + dup rot swap count! swap .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"add-get\" compile", C3),
            eval("\"get_count\" compile", C3),
            C4 = eval("0 counter server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            Pid = maps:get(pid, Info),
            Ref = make_ref(),
            Pid ! {call, 'add-get', [{'Int', 7}], self(), Ref},
            receive {reply, Ref, [{'Int', 7}]} -> ok
            after 2000 -> ?assert(false)
            end,
            Pid ! {cast, stop, []}
        end} end
    ]}.

%% --- Generic actor_loop with native cache (covers cache-hit path) ---

actor_loop_cache_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"actor_loop/3 dispatches cast through native cache", fun() ->
            %% Directly test generic actor_loop with a pre-populated cache
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"bump\" compile", C3),
            eval("\"get_count\" compile", C3),
            %% Build cache manually and spawn generic loop
            Cache = af_actor_worker:build_native_cache('Counter'),
            Instance = {'Counter', #{count => {'Int', 0}}},
            Self = self(),
            Pid = spawn(fun() ->
                %% Tell parent we're ready
                Self ! ready,
                af_type_actor:actor_loop('Counter', Instance, Cache)
            end),
            receive ready -> ok end,
            %% Cast with atom key (cache hit, no args)
            Pid ! {cast, bump, []},
            Pid ! {cast, bump, []},
            timer:sleep(50),
            %% Call with atom key (cache hit, no args)
            Ref = make_ref(),
            Pid ! {call, get_count, [], self(), Ref},
            receive {reply, Ref, [{'Int', Count}]} ->
                ?assertEqual(2, Count)
            after 2000 -> ?assert(false)
            end,
            Pid ! {cast, stop, []}
        end} end,

        fun(_) -> {"actor_loop/3 dispatches cast with args through cache", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": add Counter Int -> Counter ; over count rot + count! swap drop .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"add\" compile", C3),
            eval("\"get_count\" compile", C3),
            Cache = af_actor_worker:build_native_cache('Counter'),
            Instance = {'Counter', #{count => {'Int', 0}}},
            Self = self(),
            Pid = spawn(fun() ->
                Self ! ready,
                af_type_actor:actor_loop('Counter', Instance, Cache)
            end),
            receive ready -> ok end,
            Pid ! {cast, add, [{'Int', 10}]},
            timer:sleep(50),
            Ref = make_ref(),
            Pid ! {call, get_count, [], self(), Ref},
            receive {reply, Ref, [{'Int', Count}]} ->
                ?assertEqual(10, Count)
            after 2000 -> ?assert(false)
            end,
            Pid ! {cast, stop, []}
        end} end,

        fun(_) -> {"actor_loop/3 call with args through cache", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": add Counter Int -> Counter ; over count rot + count! swap drop .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"add\" compile", C3),
            eval("\"get_count\" compile", C3),
            Cache = af_actor_worker:build_native_cache('Counter'),
            Instance = {'Counter', #{count => {'Int', 0}}},
            Self = self(),
            Pid = spawn(fun() ->
                Self ! ready,
                af_type_actor:actor_loop('Counter', Instance, Cache)
            end),
            receive ready -> ok end,
            Ref = make_ref(),
            Pid ! {call, add, [{'Int', 7}], self(), Ref},
            receive {reply, Ref, _} -> ok
            after 2000 -> ?assert(false)
            end,
            Pid ! {cast, stop, []}
        end} end,

        fun(_) -> {"actor_loop/3 string call converted to atom", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": get_count Counter -> Counter Int ; count .", C1),
            eval("\"get_count\" compile", C2),
            Cache = af_actor_worker:build_native_cache('Counter'),
            Instance = {'Counter', #{count => {'Int', 5}}},
            Self = self(),
            Pid = spawn(fun() ->
                Self ! ready,
                af_type_actor:actor_loop('Counter', Instance, Cache)
            end),
            receive ready -> ok end,
            Ref = make_ref(),
            Pid ! {call, "get_count", [], self(), Ref},
            receive {reply, Ref, [{'Int', 5}]} -> ok
            after 2000 -> ?assert(false)
            end,
            Pid ! {cast, stop, []}
        end} end
    ]}.

%% --- separate_reply edge cases ---

separate_reply_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"separate_reply with empty stack returns undefined", fun() ->
            {Reply, State} = af_type_actor:separate_reply('Counter', []),
            ?assertEqual([], Reply),
            ?assertEqual(undefined, State)
        end} end,

        fun(_) -> {"separate_reply with no matching type returns all as reply", fun() ->
            Stack = [{'Int', 1}, {'Int', 2}],
            {Reply, State} = af_type_actor:separate_reply('Counter', Stack),
            ?assertEqual([{'Int', 1}, {'Int', 2}], Reply),
            ?assertEqual(undefined, State)
        end} end
    ]}.

%% --- find_matching_entry / match_args edge cases ---

match_args_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"dispatch picks correct overload from multiple entries", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Box value Int .", C0),
            C2 = eval(": update Box Int -> Box ; value! .", C1),
            C3 = eval(": get Box -> Box Int ; value .", C2),
            C4 = eval("0 box server", C3),
            C5 = eval("<< 42 update >>", C4),
            C6 = eval("<< get >>", C5),
            [{'Int', 42}, {'Actor', _}] = C6#continuation.data_stack,
            eval("<< stop >>", C6),
            ok
        end} end,

        fun(_) -> {"dispatch selects matching entry from multiple vocab entries", fun() ->
            %% Two words with same name and different arg types test find_matching_entry
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": set Counter Int -> Counter ; count! .", C1),
            C3 = eval(": set Counter Bool -> Counter ; drop 0 count! .", C2),
            C4 = eval(": get Counter -> Counter Int ; count .", C3),
            C5 = eval("0 counter server", C4),
            %% Both 'set' overloads should be in vocab with different arg types
            [{'Actor', Info}] = C5#continuation.data_stack,
            Vocab = maps:get(vocab, Info),
            SetEntries = maps:get("set", Vocab),
            ?assert(length(SetEntries) >= 2),
            %% Int arg dispatches to first overload
            C6 = eval("<< 42 set >>", C5),
            C7 = eval("<< get >>", C6),
            [{'Int', 42}, {'Actor', _}] = C7#continuation.data_stack,
            %% Bool arg dispatches to second overload (reset to 0)
            C8 = eval("drop << true bool set >>", C7),
            C9 = eval("<< get >>", C8),
            [{'Int', 0}, {'Actor', _}] = C9#continuation.data_stack,
            eval("<< stop >>", C9),
            ok
        end} end
    ]}.

%% --- supervised-server with compiled words (covers af_actor_worker cache hit) ---

supervised_compiled_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"supervised-server cast/call through native cache", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"bump\" compile", C3),
            eval("\"get_count\" compile", C3),
            C4 = eval("0 counter supervised-server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            Pid = maps:get(pid, Info),
            %% Cast through gen_server (exercises af_actor_worker cache hit)
            gen_server:cast(Pid, {cast, bump, []}),
            gen_server:cast(Pid, {cast, bump, []}),
            timer:sleep(50),
            %% Call through gen_server
            {ok, [{'Int', Count}]} = gen_server:call(Pid, {call, get_count, []}),
            ?assertEqual(2, Count),
            gen_server:cast(Pid, {cast, stop, []})
        end} end,

        fun(_) -> {"supervised-server cast with args through cache", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": add Counter Int -> Counter ; over count rot + count! swap drop .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"add\" compile", C3),
            eval("\"get_count\" compile", C3),
            C4 = eval("0 counter supervised-server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            Pid = maps:get(pid, Info),
            gen_server:cast(Pid, {cast, add, [{'Int', 10}]}),
            timer:sleep(50),
            {ok, [{'Int', Count}]} = gen_server:call(Pid, {call, get_count, []}),
            ?assertEqual(10, Count),
            gen_server:cast(Pid, {cast, stop, []})
        end} end,

        fun(_) -> {"supervised-server call with args through cache", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": add Counter Int -> Counter ; over count rot + count! swap drop .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"add\" compile", C3),
            eval("\"get_count\" compile", C3),
            C4 = eval("0 counter supervised-server", C3),
            [{'Actor', Info}] = C4#continuation.data_stack,
            Pid = maps:get(pid, Info),
            {ok, _} = gen_server:call(Pid, {call, add, [{'Int', 5}]}),
            {ok, [{'Int', Count}]} = gen_server:call(Pid, {call, get_count, []}),
            ?assertEqual(5, Count),
            gen_server:cast(Pid, {cast, stop, []})
        end} end
    ]}.

%% --- Atom key conversion tests ---

atom_key_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"to_atom_key converts string to atom", fun() ->
            ?assertEqual(bump, af_type_actor:to_atom_key("bump")),
            ?assertEqual(bump, af_type_actor:to_atom_key(bump))
        end} end,

        fun(_) -> {"to_string_key converts atom to string", fun() ->
            ?assertEqual("bump", af_type_actor:to_string_key(bump)),
            ?assertEqual("bump", af_type_actor:to_string_key("bump"))
        end} end,

        fun(_) -> {"send_cast accepts atom keys", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"bump\" compile", C3),
            eval("\"get_count\" compile", C3),
            C4 = eval("0 counter server", C3),
            [{'Actor', ActorInfo} | _] = C4#continuation.data_stack,
            %% Send via send_cast with atom key
            af_type_actor:send_cast(ActorInfo, bump, []),
            af_type_actor:send_cast(ActorInfo, bump, []),
            timer:sleep(50),
            [{'Int', Count}] = af_type_actor:send_call(ActorInfo, get_count, []),
            ?assertEqual(2, Count),
            maps:get(pid, ActorInfo) ! {cast, stop, []}
        end} end,

        fun(_) -> {"send_cast accepts string keys (backwards compat)", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            C3 = eval(": get_count Counter -> Counter Int ; count .", C2),
            eval("\"bump\" compile", C3),
            eval("\"get_count\" compile", C3),
            C4 = eval("0 counter server", C3),
            [{'Actor', ActorInfo} | _] = C4#continuation.data_stack,
            %% Send via send_cast with string key
            af_type_actor:send_cast(ActorInfo, "bump", []),
            timer:sleep(50),
            [{'Int', Count}] = af_type_actor:send_call(ActorInfo, "get_count", []),
            ?assertEqual(1, Count),
            maps:get(pid, ActorInfo) ! {cast, stop, []}
        end} end,

        fun(_) -> {"native cache uses atom keys", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            eval("\"bump\" compile", C2),
            Cache = af_actor_worker:build_native_cache('Counter'),
            %% Key should be atom, not string
            ?assertMatch({ok, {_, _}}, maps:find(bump, Cache)),
            ?assertEqual(error, maps:find("bump", Cache))
        end} end
    ]}.
