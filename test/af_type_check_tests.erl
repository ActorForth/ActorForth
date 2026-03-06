-module(af_type_check_tests).

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
    af_type_compiler:init().

%% --- Direct af_type_check API tests ---

check_word_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"correct word type checks ok (dup +)", fun() ->
            %% Body ops have empty sigs (as resolve_compile_token creates).
            %% The type checker looks up by name in the registry.
            %% For : double Int -> Int ; dup + .
            %% Start: [Int] -> dup -> [Int, Int] -> + -> [Int] ✓
            DupOp = #operation{name = "dup", sig_in = [], sig_out = []},
            PlusOp = #operation{name = "+", sig_in = [], sig_out = []},
            Result = af_type_check:check_word("double", ['Int'], ['Int'], [DupOp, PlusOp]),
            ?assertEqual(ok, Result)
        end} end,

        fun(_) -> {"type mismatch detected (dup only)", fun() ->
            %% Declare Int -> Int but body does dup (returns two values)
            DupOp = #operation{name = "dup", sig_in = [], sig_out = []},
            Result = af_type_check:check_word("bad", ['Int'], ['Int'], [DupOp]),
            ?assertMatch({error, {type_mismatch, "bad", _}}, Result)
        end} end,

        fun(_) -> {"infer_stack with dup", fun() ->
            %% dup on [Int] -> [Int, Int] (via registry lookup)
            DupOp = #operation{name = "dup", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack([DupOp], ['Int']),
            ?assertEqual(['Int', 'Int'], Result)
        end} end,

        fun(_) -> {"infer_stack with empty body", fun() ->
            {ok, Result} = af_type_check:infer_stack([], ['Int', 'Bool']),
            ?assertEqual(['Int', 'Bool'], Result)
        end} end,

        fun(_) -> {"unknown token becomes Atom", fun() ->
            UnknownOp = #operation{name = "xyz_unknown", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack([UnknownOp], ['Int']),
            ?assertEqual(['Atom', 'Int'], Result)
        end} end,

        fun(_) -> {"type check resolves Any to concrete type", fun() ->
            %% dup: Any -> Any Any. When applied to [Int], resolves to [Int, Int]
            {ok, DupOp} = af_type:find_op_by_name("dup", 'Any'),
            {ok, Result} = af_type_check:infer_stack([DupOp], ['Int']),
            ?assertEqual(['Int', 'Int'], Result)
        end} end
    ]}.

%% --- Integration: type check during word compilation ---

compile_check_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"well-typed word compiles without warning", fun() ->
            %% Capture io output to check for warnings
            OldGL = group_leader(),
            {ok, Pid} = start_capture(),
            group_leader(Pid, self()),
            eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            group_leader(OldGL, self()),
            Output = stop_capture(Pid),
            %% Should not contain "Warning"
            ?assertEqual(nomatch, string:find(Output, "Warning"))
        end} end,

        fun(_) -> {"mistyped word detected by check_word", fun() ->
            %% Test the type checker directly with a dup-only body
            %% dup: ['Any'] -> ['Any', 'Any']
            %% Starting with ['Int'], dup gives ['Int', 'Int']
            %% But declared output is ['Int'] (one item) — mismatch
            DupOp = #operation{name = "dup", sig_in = [], sig_out = []},
            Result = af_type_check:check_word("bad", ['Int'], ['Int'], [DupOp]),
            ?assertMatch({error, {type_mismatch, "bad", _}}, Result)
        end} end
    ]}.

%% Simple IO capture for testing warnings
start_capture() ->
    Parent = self(),
    Pid = spawn_link(fun() -> capture_loop(Parent, []) end),
    {ok, Pid}.

stop_capture(Pid) ->
    Pid ! {get, self()},
    receive
        {captured, Data} -> lists:flatten(Data)
    after 1000 -> ""
    end.

capture_loop(Parent, Acc) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _Enc, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Parent, [Acc, Chars]);
        {io_request, From, ReplyAs, {put_chars, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Parent, [Acc, Chars]);
        {io_request, From, ReplyAs, _} ->
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Parent, Acc);
        {get, Requester} ->
            Requester ! {captured, lists:flatten(Acc)}
    end.
