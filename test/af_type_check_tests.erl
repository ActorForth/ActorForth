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
    af_type:reset().

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

%% --- Error paths: check_word returning error, stack underflow (lines 28, 39, 102, 117) ---

error_path_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"check_word type mismatch from body (line 28 path)", fun() ->
            %% dup on Int produces [Int, Int], but sig_out is [Bool] -> mismatch
            DupOp = #operation{name = "dup"},
            Result = af_type_check:check_word("fail", ['Int'], ['Bool'], [DupOp]),
            ?assertMatch({error, {type_mismatch, "fail", _}}, Result)
        end} end,
        fun(_) -> {"infer_stack error from binding mismatch (line 39 path)", fun() ->
            %% Register op requiring same type twice
            TestOp = #operation{
                name = "same2_err",
                sig_in = ['_a', '_a'],
                sig_out = ['_a'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            %% _a binds to Int, then Bool doesn't match -> error from consume_types
            Result = af_type_check:infer_stack([#operation{name = "same2_err"}], ['Int', 'Bool']),
            ?assertMatch({error, _}, Result)
        end} end
    ]}.

%% --- Named variable binding mismatch (lines 128, 130, 132) ---

binding_mismatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"named variable _a bound then mismatched (lines 128-132)", fun() ->
            %% Register an op with _a _a sig_in (requires same type twice)
            TestOp = #operation{
                name = "same_type_op",
                sig_in = ['_a', '_a'],
                sig_out = ['_a'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            %% Try with mismatched types: Int and Bool
            Body = [#operation{name = "same_type_op"}],
            Result = af_type_check:infer_stack(Body, ['Int', 'Bool']),
            %% _a binds to Int, then Bool doesn't match -> error
            ?assertMatch({error, {stack_underflow, "same_type_op",
                                  {expected, 'Int', got, 'Bool'}}}, Result)
        end} end,
        fun(_) -> {"named variable _a bound then matching (line 130)", fun() ->
            TestOp = #operation{
                name = "same_type_ok",
                sig_in = ['_a', '_a'],
                sig_out = ['_a'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "same_type_ok"}],
            {ok, Result} = af_type_check:infer_stack(Body, ['Int', 'Int']),
            ?assertEqual(['Int'], Result)
        end} end
    ]}.

%% --- Concrete type mismatch in consume_types (line 142) ---
%% Note: This is hard to hit via infer_stack because find_op_for_inference
%% uses match_sig which would also reject mismatched types. The concrete
%% mismatch path is reached when an op has 'Any' in sig_in for match_sig
%% purposes but a more specific type in the actual sig_in used by consume_types.
%% We test it indirectly through the binding_mismatch tests above.

concrete_mismatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"concrete type in sig_in with matching stack type", fun() ->
            %% Register an op that requires exactly Int, give it Int
            TestOp = #operation{
                name = "int_exact_op",
                sig_in = ['Int'],
                sig_out = ['Bool'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "int_exact_op"}],
            {ok, Result} = af_type_check:infer_stack(Body, ['Int']),
            ?assertEqual(['Bool'], Result)
        end} end
    ]}.

%% --- classify_sig_type with {Type, Value} constraints (line 154) ---

classify_value_constraint_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"{Type, Value} constraint in sig_in classified as concrete (line 154)", fun() ->
            %% Register an op with value constraint in sig_in
            TestOp = #operation{
                name = "zero_only",
                sig_in = [{'Int', 0}],
                sig_out = ['Int'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "zero_only"}],
            %% Use {Int,0} in type stack so match_sig also matches
            {ok, Result} = af_type_check:infer_stack(Body, [{'Int', 0}]),
            ?assertEqual(['Int'], Result)
        end} end,
        fun(_) -> {"{Type, Value} constraint in sig_in with matching base type on stack", fun() ->
            %% Register an op with both a value-constrained AND an Any-signatured variant
            %% so match_sig passes even when stack has just plain Int
            TestOp = #operation{
                name = "val_or_any",
                sig_in = ['Any'],
                sig_out = ['Bool'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "val_or_any"}],
            %% With value constraint on type stack
            {ok, Result} = af_type_check:infer_stack(Body, [{'Int', 5}]),
            ?assertEqual(['Bool'], Result)
        end} end
    ]}.

%% --- resolve_type edge cases (lines 171, 174, 180, 184) ---

resolve_type_edge_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"Any in sig_out resolves to sole binding (line 171)", fun() ->
            %% dup has Any->Any Any; with single binding Int,
            %% Any resolves to Int. Already tested, but ensure direct path.
            DupOp = #operation{name = "dup"},
            {ok, Result} = af_type_check:infer_stack([DupOp], ['Bool']),
            ?assertEqual(['Bool', 'Bool'], Result)
        end} end,
        fun(_) -> {"underscore _ in sig_out resolves to Any (line 174)", fun() ->
            %% Register an op with _ in sig_out
            TestOp = #operation{
                name = "wild_out",
                sig_in = ['Int'],
                sig_out = ['_'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "wild_out"}],
            {ok, Result} = af_type_check:infer_stack(Body, ['Int']),
            %% _ in output should resolve to 'Any'
            ?assertEqual(['Any'], Result)
        end} end,
        fun(_) -> {"unbound named variable in sig_out stays as-is (line 180)", fun() ->
            %% Register an op with _b in sig_out but only _a in sig_in
            TestOp = #operation{
                name = "unbound_out",
                sig_in = ['_a'],
                sig_out = ['_b'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "unbound_out"}],
            {ok, Result} = af_type_check:infer_stack(Body, ['Int']),
            %% _b is unbound, stays as _b
            ?assertEqual(['_b'], Result)
        end} end,
        fun(_) -> {"{Type,Value} in sig_out passes through (line 184)", fun() ->
            TestOp = #operation{
                name = "const_val",
                sig_in = [],
                sig_out = [{'Int', 42}],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "const_val"}],
            {ok, Result} = af_type_check:infer_stack(Body, []),
            ?assertEqual([{'Int', 42}], Result)
        end} end
    ]}.

%% --- type_compatible with _ and type variables (lines 188, 191) ---

type_compatible_edge_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"_ in sig_in matches any stack type (line 188)", fun() ->
            %% Register op with bare _ (not named var) in sig_in
            TestOp = #operation{
                name = "drop_wild",
                sig_in = ['_'],
                sig_out = [],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "drop_wild"}],
            {ok, R1} = af_type_check:infer_stack(Body, ['String']),
            ?assertEqual([], R1),
            {ok, R2} = af_type_check:infer_stack(Body, ['Bool']),
            ?assertEqual([], R2)
        end} end,
        fun(_) -> {"type variable in sig_in matches anything (line 191)", fun() ->
            TestOp = #operation{
                name = "var_match",
                sig_in = ['_x'],
                sig_out = ['_x'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "var_match"}],
            {ok, Result} = af_type_check:infer_stack(Body, ['Float']),
            ?assertEqual(['Float'], Result)
        end} end
    ]}.

%% --- check_word propagates infer_stack error (line 28) ---

infer_error_propagation_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"check_word returns error when infer_stack fails (stack underflow)", fun() ->
            %% Register an op that requires 2 args
            TestOp = #operation{
                name = "needs_two",
                sig_in = ['Int', 'Int'],
                sig_out = ['Int'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            %% Call check_word with only 1 item in sig_in (stack has 1 item)
            %% Body tries to consume 2 items from a 1-item stack -> underflow
            Body = [#operation{name = "needs_two"}],
            Result = af_type_check:check_word("fail_infer", ['Int'], ['Int'], Body),
            ?assertMatch({error, _}, Result)
        end} end,


        fun(_) -> {"type_compatible with concrete type mismatch in consume_types (line 131)", fun() ->
            %% Register op requiring exactly Bool
            TestOp = #operation{
                name = "bool_only",
                sig_in = ['Bool'],
                sig_out = ['Int'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Bool', TestOp),
            %% Try with Bool on stack (should work)
            Body = [#operation{name = "bool_only"}],
            {ok, R} = af_type_check:infer_stack(Body, ['Bool']),
            ?assertEqual(['Int'], R)
        end} end,

        fun(_) -> {"resolve_type Any with multiple bindings stays Any", fun() ->
            %% Register op with _a _b -> Any
            TestOp = #operation{
                name = "multi_bind",
                sig_in = ['_a', '_b'],
                sig_out = ['Any'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "multi_bind"}],
            {ok, R} = af_type_check:infer_stack(Body, ['Int', 'Bool']),
            %% With 2 bindings, Any stays as Any (not resolved)
            ?assertEqual(['Any'], R)
        end} end,

        fun(_) -> {"is_type_variable with non-atom returns false", fun() ->
            ?assertNot(af_type_check:is_type_variable({tuple, value})),
            ?assertNot(af_type_check:is_type_variable("string"))
        end} end,

        fun(_) -> {"resolve_type Any with exactly one binding resolves (line 171)", fun() ->
            %% Register op with Any in sig_out and _a in sig_in
            %% When there's exactly 1 binding, Any resolves to it
            TestOp = #operation{
                name = "any_out_single",
                sig_in = ['_a'],
                sig_out = ['Any'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "any_out_single"}],
            {ok, R} = af_type_check:infer_stack(Body, ['Int']),
            ?assertEqual(['Int'], R)
        end} end,

        fun(_) -> {"type_compatible with underscore wildcard (line 188)", fun() ->
            %% Register op with _ in sig_in (not named var, just bare _)
            TestOp = #operation{
                name = "underscore_in",
                sig_in = ['_'],
                sig_out = ['Bool'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "underscore_in"}],
            %% _ should match any type
            {ok, R1} = af_type_check:infer_stack(Body, ['Float']),
            ?assertEqual(['Bool'], R1)
        end} end,

        fun(_) -> {"type_compatible type_variable in concrete sig (line 191)", fun() ->
            TestOp = #operation{
                name = "tvar_compat",
                sig_in = ['_x'],
                sig_out = ['_x'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            Body = [#operation{name = "tvar_compat"}],
            {ok, R} = af_type_check:infer_stack(Body, ['String']),
            ?assertEqual(['String'], R)
        end} end,

        fun(_) -> {"check_word propagates infer_stack binding error (line 28)", fun() ->
            %% Register an op with _a _a sig (requires same type twice)
            TestOp = #operation{
                name = "pair_check",
                sig_in = ['_a', '_a'],
                sig_out = ['_a'],
                impl = fun(Cont) -> Cont end
            },
            af_type:add_op('Any', TestOp),
            %% check_word: input stack [Int, Bool], body calls pair_check
            %% _a binds to Int (TOS), then Bool doesn't match -> error
            Body = [#operation{name = "pair_check"}],
            Result = af_type_check:check_word("bad_pair", ['Int', 'Bool'], ['Int'], Body),
            ?assertMatch({error, _}, Result)
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
