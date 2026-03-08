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
        end} end,

        fun(_) -> {"try_literal_type detects float (line 65)", fun() ->
            FloatOp = #operation{name = "3.14", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack([FloatOp], []),
            ?assertEqual(['Float'], Result)
        end} end,

        fun(_) -> {"check_word output mismatch with Any in result (line 128)", fun() ->
            %% dup on ['Int'] produces ['Int', 'Int']
            %% Declare output as ['Any', 'Any'] which should match via type_compatible
            DupOp = #operation{name = "dup", sig_in = [], sig_out = []},
            Result = af_type_check:check_word("dupthing", ['Int'], ['Any', 'Any'], [DupOp]),
            ?assertEqual(ok, Result)
        end} end,

        fun(_) -> {"check_word output with Any on stack side (line 130)", fun() ->
            %% Start with ['Any'], dup gives ['Any', 'Any']
            %% Declare output as ['Int', 'Int'] -- 'Any' on stack should match 'Int' in sig
            DupOp = #operation{name = "dup", sig_in = [], sig_out = []},
            Result = af_type_check:check_word("dupthing2", ['Any'], ['Int', 'Int'], [DupOp]),
            ?assertEqual(ok, Result)
        end} end,

        fun(_) -> {"check_word output value constraint matches base type (line 131-132)", fun() ->
            %% Push a literal int (e.g., "42") which produces ['Int'] on type stack
            %% Declare output as ['Int'] - should match
            LitOp = #operation{name = "42", sig_in = [], sig_out = []},
            Result = af_type_check:check_word("lit42", [], ['Int'], [LitOp]),
            ?assertEqual(ok, Result)
        end} end,

        fun(_) -> {"infer_stack with drop on two items", fun() ->
            %% drop removes TOS, verify it works with two items
            DropOp = #operation{name = "drop", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack([DropOp], ['Int', 'Bool']),
            ?assertEqual(['Bool'], Result)
        end} end,

        fun(_) -> {"infer_stack with swap preserves both types", fun() ->
            %% swap: _a _b -> _b _a. Named type variables track each position.
            %% [Int, Bool] (Int=TOS) -> swap -> [Bool, Int] (Bool=TOS)
            SwapOp = #operation{name = "swap", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack([SwapOp], ['Int', 'Bool']),
            ?assertEqual(['Bool', 'Int'], Result)
        end} end,

        fun(_) -> {"match_output with value constraint in SigOut vs base type result (line 132)", fun() ->
            %% ResultStack will have ['Int'] (base type)
            %% SigOut has [{Int, 42}] (value constraint)
            %% type_compatible(Int, {Int, 42}) on line 132 should match
            %% Use identity body (empty) so result = input
            Result = af_type_check:check_word("constword", ['Int'], [{'Int', 42}], []),
            ?assertEqual(ok, Result)
        end} end,

        fun(_) -> {"match_output with value constraint in result vs base type SigOut (line 131)", fun() ->
            %% If we can get a {Type, Value} in the result stack...
            %% Actually infer_stack always produces atoms, not tuples.
            %% But we can test via check_word with empty body and constrained SigIn.
            %% check_word("x", [{Int,5}], [{Int,5}], []) -- SigIn has constraint,
            %% infer_stack starts with [{Int,5}], empty body, result = [{Int,5}].
            %% match_output([{Int,5}], [{Int,5}]) -> type_compatible({Int,5},{Int,5})
            %% That hits line 133 (same base type, different value constraints? or line 129)
            Result = af_type_check:check_word("constword2", [{'Int', 5}], [{'Int', 5}], []),
            ?assertEqual(ok, Result)
        end} end,

        fun(_) -> {"match_output value constraint result against base type sig (line 131)", fun() ->
            %% Start with {Int, 5} in type stack, empty body, result = [{Int,5}]
            %% SigOut = [Int] (base type)
            %% match_output([{Int,5}], [Int]) -> type_compatible({Int,5}, Int)
            %% This hits line 131: type_compatible({Type, _Value}, Type)
            Result = af_type_check:check_word("constword3", [{'Int', 5}], ['Int'], []),
            ?assertEqual(ok, Result)
        end} end,

        fun(_) -> {"match_output length mismatch (line 143)", fun() ->
            %% Result stack has 2 items, SigOut has 1
            DupOp = #operation{name = "dup", sig_in = [], sig_out = []},
            Result = af_type_check:check_word("badlen", ['Int'], ['Int'], [DupOp]),
            ?assertMatch({error, {type_mismatch, "badlen", _}}, Result)
        end} end
    ]}.

%% --- Named type variable tests ---

type_variable_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"is_type_variable recognizes _name atoms", fun() ->
            ?assert(af_type_check:is_type_variable('_a')),
            ?assert(af_type_check:is_type_variable('_alpha')),
            ?assert(af_type_check:is_type_variable('_1')),
            ?assertNot(af_type_check:is_type_variable('_')),
            ?assertNot(af_type_check:is_type_variable('Any')),
            ?assertNot(af_type_check:is_type_variable('Int')),
            ?assertNot(af_type_check:is_type_variable(42))
        end} end,

        fun(_) -> {"swap with different types preserves both", fun() ->
            SwapOp = #operation{name = "swap", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack([SwapOp], ['Actor', {'Int', 0}]),
            ?assertEqual([{'Int', 0}, 'Actor'], Result)
        end} end,

        fun(_) -> {"swap then drop leaves correct type (blast base case)", fun() ->
            %% This is the exact pattern that was failing:
            %% : blast 0 Int Actor -> Actor ; swap drop .
            %% Input: [{Int,0}, Actor] (Actor=TOS) -> swap -> [{Int,0} is now TOS]
            %%   -> drop -> [Actor] remaining
            SwapOp = #operation{name = "swap", sig_in = [], sig_out = []},
            DropOp = #operation{name = "drop", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack(
                [SwapOp, DropOp], ['Actor', {'Int', 0}]),
            ?assertEqual(['Actor'], Result)
        end} end,

        fun(_) -> {"blast base case type checks ok", fun() ->
            %% : blast 0 Int Actor -> Actor ; swap drop .
            SwapOp = #operation{name = "swap", sig_in = [], sig_out = []},
            DropOp = #operation{name = "drop", sig_in = [], sig_out = []},
            Result = af_type_check:check_word("blast",
                ['Actor', {'Int', 0}], ['Actor'], [SwapOp, DropOp]),
            ?assertEqual(ok, Result)
        end} end,

        fun(_) -> {"rot preserves all three types", fun() ->
            %% rot: _a _b _c -> _c _a _b (brings 3rd to top)
            %% Stack [Int, Bool, String] (Int=TOS) -> [String, Int, Bool]
            RotOp = #operation{name = "rot", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack(
                [RotOp], ['Int', 'Bool', 'String']),
            ?assertEqual(['String', 'Int', 'Bool'], Result)
        end} end,

        fun(_) -> {"over preserves types and copies second", fun() ->
            %% over: _a _b -> _a _b _a (copies 2nd to top)
            %% Stack [Int, Bool] (Int=TOS) -> [Bool, Int, Bool]
            OverOp = #operation{name = "over", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack(
                [OverOp], ['Int', 'Bool']),
            ?assertEqual(['Bool', 'Int', 'Bool'], Result)
        end} end,

        fun(_) -> {"2dup preserves both types", fun() ->
            %% 2dup: _a _b -> _a _b _a _b
            %% Stack [Int, Bool] -> [Int, Bool, Int, Bool]
            TwoDupOp = #operation{name = "2dup", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack(
                [TwoDupOp], ['Int', 'Bool']),
            ?assertEqual(['Int', 'Bool', 'Int', 'Bool'], Result)
        end} end,

        fun(_) -> {"dup with value constraint preserves it", fun() ->
            %% dup: _a -> _a _a
            %% Stack [{Int,5}] -> [{Int,5}, {Int,5}]
            DupOp = #operation{name = "dup", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack(
                [DupOp], [{'Int', 5}]),
            ?assertEqual([{'Int', 5}, {'Int', 5}], Result)
        end} end,

        fun(_) -> {"swap then dup chain preserves types", fun() ->
            %% [Int, Bool] -> swap -> [Bool, Int] -> dup -> [Int, Int, Bool]
            SwapOp = #operation{name = "swap", sig_in = [], sig_out = []},
            DupOp = #operation{name = "dup", sig_in = [], sig_out = []},
            {ok, Result} = af_type_check:infer_stack(
                [SwapOp, DupOp], ['Int', 'Bool']),
            ?assertEqual(['Bool', 'Bool', 'Int'], Result)
        end} end,

        fun(_) -> {"anonymous wildcard _ does not track binding", fun() ->
            %% drop uses '_' — should work on any type without tracking
            DropOp = #operation{name = "drop", sig_in = [], sig_out = []},
            {ok, R1} = af_type_check:infer_stack([DropOp], ['Int', 'Bool']),
            ?assertEqual(['Bool'], R1),
            {ok, R2} = af_type_check:infer_stack([DropOp], ['String']),
            ?assertEqual([], R2)
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
