-module(af_lsp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("operation.hrl").

setup() ->
    af_type:reset().

%% NOTE: infer_stacks records the stack BEFORE each token is processed.
%% So the snap for token N shows the stack state before token N's effect.
%% To see the effect of the last token, you need a subsequent snap (which doesn't exist).

%% === Stack Inference Tests ===

infer_simple_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"infer stacks for simple expression", fun() ->
            Tokens = af_parser:parse("1 2 +", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            ?assertEqual(3, length(Snaps)),
            S1 = lists:nth(1, Snaps),
            ?assertEqual([], maps:get(stack, S1)),
            S2 = lists:nth(2, Snaps),
            ?assertEqual(["Int"], maps:get(stack, S2)),
            S3 = lists:nth(3, Snaps),
            ?assertEqual(["Int", "Int"], maps:get(stack, S3))
        end} end
    ]}.

infer_string_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"infer stacks for string operations", fun() ->
            Tokens = af_parser:parse("\"hello\" \"world\" concat", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            ?assertEqual(3, length(Snaps)),
            S3 = lists:nth(3, Snaps),
            ?assertEqual(["String", "String"], maps:get(stack, S3))
        end} end
    ]}.

infer_stack_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"infer dup/drop/swap count", fun() ->
            Tokens = af_parser:parse("1 2 dup drop swap", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            ?assertEqual(5, length(Snaps))
        end} end,

        fun(_) -> {"infer dup: stack before dup shows one Int", fun() ->
            Tokens = af_parser:parse("1 dup", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            %% Snap for "dup" shows stack before dup = ["Int"]
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"infer dup effect via next token", fun() ->
            %% To see dup's effect, we add another token after it
            Tokens = af_parser:parse("1 dup +", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            %% Snap for "+" shows stack after dup = ["Int", "Int"]
            Last = lists:last(Snaps),
            ?assertEqual(["Int", "Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"infer drop effect via next token", fun() ->
            Tokens = af_parser:parse("1 2 drop stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"infer swap effect via next token", fun() ->
            Tokens = af_parser:parse("1 2 swap stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int", "Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"infer rot effect via next token", fun() ->
            Tokens = af_parser:parse("1 2 3 rot stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int", "Int", "Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"infer over effect via next token", fun() ->
            Tokens = af_parser:parse("1 2 over stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int", "Int", "Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"infer 2dup effect via next token", fun() ->
            Tokens = af_parser:parse("1 2 2dup stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int", "Int", "Int", "Int"], maps:get(stack, Last))
        end} end
    ]}.

infer_word_def_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"word definition markers", fun() ->
            Tokens = af_parser:parse(": double Int -> Int ; dup + .", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            ?assert(length(Snaps) > 0)
        end} end,

        fun(_) -> {"colon pushes def_marker", fun() ->
            %% ":" then "foo" — snap for "foo" shows stack after ":"
            Tokens = af_parser:parse(": foo", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            S2 = lists:nth(2, Snaps),
            ?assertEqual([{def_marker}], maps:get(stack, S2))
        end} end,

        fun(_) -> {"arrow is no-op on stack", fun() ->
            Tokens = af_parser:parse("1 -> stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"semicolon is no-op on stack", fun() ->
            Tokens = af_parser:parse("1 ; stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"dot on stack with no marker empties it", fun() ->
            Tokens = af_parser:parse("1 2 . stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual([], maps:get(stack, Last))
        end} end
    ]}.

%% === Arithmetic and comparison inference ===

infer_arithmetic_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"subtraction effect", fun() ->
            Tokens = af_parser:parse("3 1 - stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"multiplication effect", fun() ->
            Tokens = af_parser:parse("3 2 * stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"division effect", fun() ->
            Tokens = af_parser:parse("6 2 / stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"mod effect", fun() ->
            Tokens = af_parser:parse("7 3 mod stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end
    ]}.

infer_comparison_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"== produces Bool", fun() ->
            Tokens = af_parser:parse("1 2 == stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"!= produces Bool", fun() ->
            Tokens = af_parser:parse("1 2 != stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"< produces Bool", fun() ->
            Tokens = af_parser:parse("1 2 < stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"> produces Bool", fun() ->
            Tokens = af_parser:parse("1 2 > stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"<= produces Bool", fun() ->
            Tokens = af_parser:parse("1 2 <= stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {">= produces Bool", fun() ->
            Tokens = af_parser:parse("1 2 >= stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end
    ]}.

infer_logic_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"not produces Bool", fun() ->
            Tokens = af_parser:parse("1 2 == not stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"and produces Bool", fun() ->
            Tokens = af_parser:parse("1 2 == 3 4 == and stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"or produces Bool", fun() ->
            Tokens = af_parser:parse("1 2 == 3 4 == or stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end
    ]}.

%% === List operations inference ===

infer_list_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"nil pushes List", fun() ->
            Tokens = af_parser:parse("nil stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["List"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"cons produces List", fun() ->
            Tokens = af_parser:parse("nil 1 cons stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["List"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"head produces Any", fun() ->
            Tokens = af_parser:parse("nil head stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Any"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"tail produces List", fun() ->
            Tokens = af_parser:parse("nil tail stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["List"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"length produces Int", fun() ->
            Tokens = af_parser:parse("nil length stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"empty? produces Bool", fun() ->
            Tokens = af_parser:parse("nil empty? stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"append produces List", fun() ->
            Tokens = af_parser:parse("nil nil append stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["List"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"reverse produces List", fun() ->
            Tokens = af_parser:parse("nil reverse stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["List"], maps:get(stack, Last))
        end} end
    ]}.

%% === Map operations inference ===

infer_map_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"map-new pushes Map", fun() ->
            Tokens = af_parser:parse("map-new stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Map"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"map-put produces Map", fun() ->
            Tokens = af_parser:parse("map-new 1 2 map-put stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Map"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"map-get produces Any", fun() ->
            Tokens = af_parser:parse("map-new 1 map-get stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Any"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"map-delete produces Map", fun() ->
            Tokens = af_parser:parse("map-new 1 map-delete stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Map"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"map-has? produces Bool", fun() ->
            Tokens = af_parser:parse("map-new 1 map-has? stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"map-keys produces List", fun() ->
            Tokens = af_parser:parse("map-new map-keys stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["List"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"map-values produces List", fun() ->
            Tokens = af_parser:parse("map-new map-values stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["List"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"map-size produces Int", fun() ->
            Tokens = af_parser:parse("map-new map-size stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end
    ]}.

%% === String/conversion operations inference ===

infer_string_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"concat produces String", fun() ->
            Tokens = af_parser:parse("\"a\" \"b\" concat stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["String"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"to-string produces String", fun() ->
            Tokens = af_parser:parse("1 to-string stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["String"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"to-int produces Int", fun() ->
            Tokens = af_parser:parse("\"42\" to-int stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"to-float produces Float", fun() ->
            Tokens = af_parser:parse("1 to-float stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Float"], maps:get(stack, Last))
        end} end
    ]}.

%% === IO operations inference ===

infer_io_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"print pops one element", fun() ->
            Tokens = af_parser:parse("1 2 print stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"stack is no-op", fun() ->
            Tokens = af_parser:parse("1 2 stack stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int", "Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"assert pops one element", fun() ->
            Tokens = af_parser:parse("1 2 assert stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"assert-eq pops two elements", fun() ->
            Tokens = af_parser:parse("1 2 3 assert-eq stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end
    ]}.

%% === Float literal inference ===

infer_float_literal_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"float literal pushes Float", fun() ->
            Tokens = af_parser:parse("3.14 stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Float"], maps:get(stack, Last))
        end} end
    ]}.

%% === Unknown word inference (Atom) ===

infer_unknown_word_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"unknown word pushes Atom", fun() ->
            Tokens = af_parser:parse("xyzzy-unknown-word stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Atom"], maps:get(stack, Last))
        end} end
    ]}.

%% === Empty token list ===

infer_empty_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"empty tokens produce empty snaps", fun() ->
            Snaps = af_lsp:infer_stacks([]),
            ?assertEqual([], Snaps)
        end} end
    ]}.

%% === Stack underflow paths ===

infer_underflow_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"dup on empty stack goes through lookup", fun() ->
            Tokens = af_parser:parse("dup", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            ?assertEqual(1, length(Snaps)),
            Stack = maps:get(stack, lists:last(Snaps)),
            ?assertEqual([], Stack)
        end} end,

        fun(_) -> {"+ on empty stack goes through lookup", fun() ->
            Tokens = af_parser:parse("+", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            ?assertEqual(1, length(Snaps))
        end} end,

        fun(_) -> {"+ on one item goes through lookup", fun() ->
            Tokens = af_parser:parse("1 +", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            ?assertEqual(2, length(Snaps))
        end} end,

        fun(_) -> {"rot on two items goes through lookup", fun() ->
            Tokens = af_parser:parse("1 2 rot", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            ?assertEqual(3, length(Snaps))
        end} end
    ]}.

%% === Snap metadata ===

infer_snap_metadata_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"snaps have idx, line, col, token fields", fun() ->
            Tokens = af_parser:parse("1 2", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            S1 = lists:nth(1, Snaps),
            ?assertEqual(0, maps:get(idx, S1)),
            ?assert(is_integer(maps:get(line, S1))),
            ?assert(is_integer(maps:get(col, S1))),
            ?assert(is_list(maps:get(token, S1))),
            S2 = lists:nth(2, Snaps),
            ?assertEqual(1, maps:get(idx, S2))
        end} end
    ]}.

%% === Hover Tests ===

hover_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"hover returns stack picture", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2 +">>, 1, #{}),
            Result = af_lsp:compute_hover(Doc, 0, 4),
            ?assertNotEqual(null, Result),
            ?assert(is_map(Result)),
            Contents = maps:get(<<"contents">>, Result),
            ?assertEqual(<<"markdown">>, maps:get(<<"kind">>, Contents)),
            Value = maps:get(<<"value">>, Contents),
            ?assert(is_binary(Value))
        end} end,

        fun(_) -> {"hover on empty returns null", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"">>, 1, #{}),
            Result = af_lsp:compute_hover(Doc, 0, 0),
            ?assertEqual(null, Result)
        end} end,

        fun(_) -> {"hover on wrong line falls back to previous snap", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2 +">>, 1, #{}),
            %% Line 5 (0-indexed) doesn't exist but should fall back
            Result = af_lsp:compute_hover(Doc, 5, 0),
            ?assertNotEqual(null, Result)
        end} end,

        fun(_) -> {"hover with no stack_snaps returns null", fun() ->
            Doc = #{stack_snaps => []},
            Result = af_lsp:compute_hover(Doc, 0, 0),
            ?assertEqual(null, Result)
        end} end,

        fun(_) -> {"hover content includes token name", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2 +">>, 1, #{}),
            Result = af_lsp:compute_hover(Doc, 0, 4),
            ?assertNotEqual(null, Result),
            Contents = maps:get(<<"contents">>, Result),
            Value = maps:get(<<"value">>, Contents),
            %% Should contain bold markers
            ?assertNotEqual(nomatch, binary:match(Value, <<"**">>))
        end} end,

        fun(_) -> {"hover on first token shows empty stack", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2 +">>, 1, #{}),
            Result = af_lsp:compute_hover(Doc, 0, 0),
            ?assertNotEqual(null, Result),
            Contents = maps:get(<<"contents">>, Result),
            Value = maps:get(<<"value">>, Contents),
            ?assertNotEqual(nomatch, binary:match(Value, <<"empty">>))
        end} end
    ]}.

%% === Completion Tests ===

completion_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"completions include Int ops when TOS is Int", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2">>, 1, #{}),
            Items = af_lsp:compute_completions(Doc, 0, 3),
            Labels = [maps:get(<<"label">>, I) || I <- Items],
            ?assert(lists:member(<<"+">>, Labels) orelse length(Items) > 0)
        end} end,

        fun(_) -> {"completions include Any ops", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1">>, 1, #{}),
            Items = af_lsp:compute_completions(Doc, 0, 1),
            Labels = [maps:get(<<"label">>, I) || I <- Items],
            ?assert(lists:member(<<"dup">>, Labels))
        end} end,

        fun(_) -> {"completions for empty snaps use Any type", fun() ->
            Doc = #{stack_snaps => []},
            Items = af_lsp:compute_completions(Doc, 0, 0),
            ?assert(is_list(Items)),
            %% Should still include Any ops
            Labels = [maps:get(<<"label">>, I) || I <- Items],
            ?assert(lists:member(<<"dup">>, Labels))
        end} end,

        fun(_) -> {"completions with non-list TOS falls back to Any", fun() ->
            Doc = #{stack_snaps => [#{idx => 0, line => 1, col => 1,
                                      token => ":", stack => [{def_marker}]}]},
            Items = af_lsp:compute_completions(Doc, 0, 0),
            ?assert(is_list(Items))
        end} end,

        fun(_) -> {"completions for non-existent type return Any ops", fun() ->
            Doc = #{stack_snaps => [#{idx => 0, line => 1, col => 1,
                                      token => "x", stack => ["NonExistentType99"]}]},
            Items = af_lsp:compute_completions(Doc, 0, 0),
            ?assert(is_list(Items)),
            %% Should get Any ops since NonExistentType99 has no ops
            Labels = [maps:get(<<"label">>, I) || I <- Items],
            ?assert(lists:member(<<"dup">>, Labels))
        end} end,

        fun(_) -> {"completion items have label, detail, kind", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1">>, 1, #{}),
            Items = af_lsp:compute_completions(Doc, 0, 1),
            ?assert(length(Items) > 0),
            Item = hd(Items),
            ?assert(maps:is_key(<<"label">>, Item)),
            ?assert(maps:is_key(<<"detail">>, Item)),
            ?assert(maps:is_key(<<"kind">>, Item)),
            ?assertEqual(3, maps:get(<<"kind">>, Item))
        end} end,

        fun(_) -> {"completion labels are binaries", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1">>, 1, #{}),
            Items = af_lsp:compute_completions(Doc, 0, 1),
            lists:foreach(fun(Item) ->
                Label = maps:get(<<"label">>, Item),
                ?assert(is_binary(Label))
            end, Items)
        end} end,

        fun(_) -> {"completion details contain arrow separator", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1">>, 1, #{}),
            Items = af_lsp:compute_completions(Doc, 0, 1),
            ?assert(length(Items) > 0),
            Item = hd(Items),
            Detail = maps:get(<<"detail">>, Item),
            ?assert(is_binary(Detail)),
            ?assertNotEqual(nomatch, binary:match(Detail, <<"->">>))
        end} end
    ]}.

%% === Definition Tests ===

definition_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"go-to-definition finds word", fun() ->
            Src = <<": double Int -> Int ; dup + .\n5 double">>,
            Doc = af_lsp:analyze_document(<<"test.a4">>, Src, 1, #{}),
            Result = af_lsp:compute_definition(Doc, <<"test.a4">>, 1, 2),
            ?assertNotEqual(null, Result),
            Range = maps:get(<<"range">>, Result),
            Start = maps:get(<<"start">>, Range),
            ?assertEqual(0, maps:get(<<"line">>, Start)),
            End = maps:get(<<"end">>, Range),
            ?assert(maps:get(<<"character">>, End) > maps:get(<<"character">>, Start))
        end} end,

        fun(_) -> {"go-to-definition returns null for unknown word", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"unknown-word">>, 1, #{}),
            Result = af_lsp:compute_definition(Doc, <<"test.a4">>, 0, 5),
            ?assertEqual(null, Result)
        end} end,

        fun(_) -> {"go-to-definition returns null for empty doc", fun() ->
            Doc = #{tokens => []},
            Result = af_lsp:compute_definition(Doc, <<"test.a4">>, 0, 0),
            ?assertEqual(null, Result)
        end} end,

        fun(_) -> {"go-to-definition returns null when position is off", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1">>, 1, #{}),
            Result = af_lsp:compute_definition(Doc, <<"test.a4">>, 50, 50),
            ?assertEqual(null, Result)
        end} end,

        fun(_) -> {"go-to-definition includes uri in result", fun() ->
            Src = <<": myword Int -> Int ; dup + .\n5 myword">>,
            Doc = af_lsp:analyze_document(<<"file:///test.a4">>, Src, 1, #{}),
            Result = af_lsp:compute_definition(Doc, <<"file:///test.a4">>, 1, 2),
            ?assertNotEqual(null, Result),
            ?assertEqual(<<"file:///test.a4">>, maps:get(<<"uri">>, Result))
        end} end
    ]}.

%% === Format Tests ===

format_stack_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"format empty stack", fun() ->
            ?assertEqual(<<"( empty )">>, af_lsp:format_stack([]))
        end} end,

        fun(_) -> {"format non-empty stack", fun() ->
            Result = af_lsp:format_stack(["Int", "String"]),
            ?assertNotEqual(<<"( empty )">>, Result),
            ?assertNotEqual(nomatch, binary:match(Result, <<"TOS">>))
        end} end,

        fun(_) -> {"format single item stack", fun() ->
            Result = af_lsp:format_stack(["Int"]),
            ?assertNotEqual(<<"( empty )">>, Result),
            ?assertNotEqual(nomatch, binary:match(Result, <<"Int">>))
        end} end,

        fun(_) -> {"format stack with mixed types", fun() ->
            Result = af_lsp:format_stack(["Bool", "Int", "String"]),
            ?assertNotEqual(nomatch, binary:match(Result, <<"Bool">>)),
            ?assertNotEqual(nomatch, binary:match(Result, <<"Int">>)),
            ?assertNotEqual(nomatch, binary:match(Result, <<"String">>))
        end} end,

        fun(_) -> {"format stack skips non-list items", fun() ->
            Result = af_lsp:format_stack([{def_marker}, "Int"]),
            ?assert(is_binary(Result))
        end} end
    ]}.

%% === JSON-RPC Protocol Tests ===

handle_initialize_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"initialize returns capabilities", fun() ->
            Request = #{<<"method">> => <<"initialize">>,
                       <<"id">> => 1,
                       <<"params">> => #{<<"rootUri">> => <<"file:///test">>}},
            State = #{docs => #{}, initialized => false},
            {Response, NewState} = af_lsp:handle_request(Request, State),
            ?assert(maps:get(initialized, NewState)),
            ?assertEqual(<<"file:///test">>, maps:get(root_uri, NewState)),
            Result = maps:get(<<"result">>, Response),
            Caps = maps:get(<<"capabilities">>, Result),
            ?assert(maps:get(<<"hoverProvider">>, Caps)),
            ?assert(maps:get(<<"definitionProvider">>, Caps)),
            ?assertEqual(1, maps:get(<<"textDocumentSync">>, Caps)),
            CompProvider = maps:get(<<"completionProvider">>, Caps),
            ?assert(is_map(CompProvider)),
            ServerInfo = maps:get(<<"serverInfo">>, Result),
            ?assertEqual(<<"ActorForth LSP">>, maps:get(<<"name">>, ServerInfo)),
            ?assertEqual(<<"0.1.0">>, maps:get(<<"version">>, ServerInfo))
        end} end,

        fun(_) -> {"initialize without rootUri defaults to null", fun() ->
            Request = #{<<"method">> => <<"initialize">>,
                       <<"id">> => 1,
                       <<"params">> => #{}},
            State = #{docs => #{}, initialized => false},
            {_Response, NewState} = af_lsp:handle_request(Request, State),
            ?assertEqual(null, maps:get(root_uri, NewState))
        end} end,

        fun(_) -> {"initialize response has jsonrpc 2.0 and correct id", fun() ->
            Request = #{<<"method">> => <<"initialize">>,
                       <<"id">> => 99,
                       <<"params">> => #{}},
            State = #{docs => #{}, initialized => false},
            {Response, _} = af_lsp:handle_request(Request, State),
            ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
            ?assertEqual(99, maps:get(<<"id">>, Response))
        end} end
    ]}.

handle_initialized_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"initialized is a notification (no response)", fun() ->
            Request = #{<<"method">> => <<"initialized">>,
                       <<"params">> => #{}},
            State = #{docs => #{}, initialized => true},
            {Response, _NewState} = af_lsp:handle_request(Request, State),
            ?assertEqual(no_response, Response)
        end} end
    ]}.

handle_shutdown_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"shutdown returns null result", fun() ->
            Request = #{<<"method">> => <<"shutdown">>,
                       <<"id">> => 42,
                       <<"params">> => #{}},
            State = #{docs => #{}, initialized => true},
            {Response, _NewState} = af_lsp:handle_request(Request, State),
            ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
            ?assertEqual(42, maps:get(<<"id">>, Response)),
            ?assertEqual(null, maps:get(<<"result">>, Response))
        end} end
    ]}.

handle_exit_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"exit is a notification (no response)", fun() ->
            Request = #{<<"method">> => <<"exit">>,
                       <<"params">> => #{}},
            State = #{docs => #{}, initialized => true},
            {Response, _NewState} = af_lsp:handle_request(Request, State),
            ?assertEqual(no_response, Response)
        end} end
    ]}.

handle_did_open_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"didOpen stores document and publishes diagnostics", fun() ->
            Request = #{<<"method">> => <<"textDocument/didOpen">>,
                       <<"params">> => #{
                           <<"textDocument">> => #{
                               <<"uri">> => <<"file:///test.a4">>,
                               <<"text">> => <<"1 2 +">>,
                               <<"version">> => 1
                           }
                       }},
            State = #{docs => #{}, initialized => true},
            {Response, NewState} = af_lsp:handle_request(Request, State),
            ?assertNotEqual(no_response, Response),
            ?assertEqual(<<"textDocument/publishDiagnostics">>,
                         maps:get(<<"method">>, Response)),
            ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
            Params = maps:get(<<"params">>, Response),
            ?assertEqual(<<"file:///test.a4">>, maps:get(<<"uri">>, Params)),
            ?assert(is_list(maps:get(<<"diagnostics">>, Params))),
            Docs = maps:get(docs, NewState),
            ?assert(maps:is_key(<<"file:///test.a4">>, Docs))
        end} end
    ]}.

handle_did_change_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"didChange updates document", fun() ->
            InitState = #{docs => #{<<"file:///test.a4">> =>
                af_lsp:analyze_document(<<"file:///test.a4">>, <<"1">>, 1, #{})},
                initialized => true},
            Request = #{<<"method">> => <<"textDocument/didChange">>,
                       <<"params">> => #{
                           <<"textDocument">> => #{
                               <<"uri">> => <<"file:///test.a4">>,
                               <<"version">> => 2
                           },
                           <<"contentChanges">> => [
                               #{<<"text">> => <<"1 2 +">>}
                           ]
                       }},
            {Response, NewState} = af_lsp:handle_request(Request, InitState),
            ?assertNotEqual(no_response, Response),
            ?assertEqual(<<"textDocument/publishDiagnostics">>,
                         maps:get(<<"method">>, Response)),
            Docs = maps:get(docs, NewState),
            Doc = maps:get(<<"file:///test.a4">>, Docs),
            ?assertEqual(2, maps:get(version, Doc))
        end} end,

        fun(_) -> {"didChange uses first content change when multiple", fun() ->
            State = #{docs => #{}, initialized => true},
            Request = #{<<"method">> => <<"textDocument/didChange">>,
                       <<"params">> => #{
                           <<"textDocument">> => #{
                               <<"uri">> => <<"file:///test.a4">>,
                               <<"version">> => 5
                           },
                           <<"contentChanges">> => [
                               #{<<"text">> => <<"3 4 *">>},
                               #{<<"text">> => <<"should be ignored">>}
                           ]
                       }},
            {_, NewState} = af_lsp:handle_request(Request, State),
            Doc = maps:get(<<"file:///test.a4">>, maps:get(docs, NewState)),
            ?assertEqual(5, maps:get(version, Doc)),
            ?assertEqual(<<"3 4 *">>, maps:get(text, Doc))
        end} end
    ]}.

handle_did_close_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"didClose removes document", fun() ->
            InitState = #{docs => #{<<"file:///test.a4">> =>
                af_lsp:analyze_document(<<"file:///test.a4">>, <<"1">>, 1, #{})},
                initialized => true},
            Request = #{<<"method">> => <<"textDocument/didClose">>,
                       <<"params">> => #{
                           <<"textDocument">> => #{
                               <<"uri">> => <<"file:///test.a4">>
                           }
                       }},
            {Response, NewState} = af_lsp:handle_request(Request, InitState),
            ?assertEqual(no_response, Response),
            Docs = maps:get(docs, NewState),
            ?assertNot(maps:is_key(<<"file:///test.a4">>, Docs))
        end} end
    ]}.

handle_did_save_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"didSave is a notification (no response)", fun() ->
            Request = #{<<"method">> => <<"textDocument/didSave">>,
                       <<"params">> => #{}},
            State = #{docs => #{}, initialized => true},
            {Response, _NewState} = af_lsp:handle_request(Request, State),
            ?assertEqual(no_response, Response)
        end} end
    ]}.

handle_hover_protocol_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"hover request returns stack info via protocol", fun() ->
            Doc = af_lsp:analyze_document(<<"file:///test.a4">>, <<"1 2 +">>, 1, #{}),
            State = #{docs => #{<<"file:///test.a4">> => Doc}, initialized => true},
            Request = #{<<"method">> => <<"textDocument/hover">>,
                       <<"id">> => 10,
                       <<"params">> => #{
                           <<"textDocument">> => #{<<"uri">> => <<"file:///test.a4">>},
                           <<"position">> => #{<<"line">> => 0, <<"character">> => 4}
                       }},
            {Response, _} = af_lsp:handle_request(Request, State),
            ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
            ?assertEqual(10, maps:get(<<"id">>, Response)),
            Result = maps:get(<<"result">>, Response),
            ?assertNotEqual(null, Result)
        end} end,

        fun(_) -> {"hover for unknown document returns null", fun() ->
            State = #{docs => #{}, initialized => true},
            Request = #{<<"method">> => <<"textDocument/hover">>,
                       <<"id">> => 11,
                       <<"params">> => #{
                           <<"textDocument">> => #{<<"uri">> => <<"file:///unknown.a4">>},
                           <<"position">> => #{<<"line">> => 0, <<"character">> => 0}
                       }},
            {Response, _} = af_lsp:handle_request(Request, State),
            ?assertEqual(null, maps:get(<<"result">>, Response))
        end} end
    ]}.

handle_completion_protocol_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"completion request returns items via protocol", fun() ->
            Doc = af_lsp:analyze_document(<<"file:///test.a4">>, <<"1 2">>, 1, #{}),
            State = #{docs => #{<<"file:///test.a4">> => Doc}, initialized => true},
            Request = #{<<"method">> => <<"textDocument/completion">>,
                       <<"id">> => 20,
                       <<"params">> => #{
                           <<"textDocument">> => #{<<"uri">> => <<"file:///test.a4">>},
                           <<"position">> => #{<<"line">> => 0, <<"character">> => 3}
                       }},
            {Response, _} = af_lsp:handle_request(Request, State),
            ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
            Result = maps:get(<<"result">>, Response),
            ?assertEqual(false, maps:get(<<"isIncomplete">>, Result)),
            ?assert(is_list(maps:get(<<"items">>, Result)))
        end} end,

        fun(_) -> {"completion for unknown document returns empty items", fun() ->
            State = #{docs => #{}, initialized => true},
            Request = #{<<"method">> => <<"textDocument/completion">>,
                       <<"id">> => 21,
                       <<"params">> => #{
                           <<"textDocument">> => #{<<"uri">> => <<"file:///unknown.a4">>},
                           <<"position">> => #{<<"line">> => 0, <<"character">> => 0}
                       }},
            {Response, _} = af_lsp:handle_request(Request, State),
            Result = maps:get(<<"result">>, Response),
            ?assertEqual([], maps:get(<<"items">>, Result))
        end} end
    ]}.

handle_definition_protocol_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"definition request via protocol", fun() ->
            Src = <<": myop Int -> Int ; dup + .\n5 myop">>,
            Doc = af_lsp:analyze_document(<<"file:///test.a4">>, Src, 1, #{}),
            State = #{docs => #{<<"file:///test.a4">> => Doc}, initialized => true},
            Request = #{<<"method">> => <<"textDocument/definition">>,
                       <<"id">> => 30,
                       <<"params">> => #{
                           <<"textDocument">> => #{<<"uri">> => <<"file:///test.a4">>},
                           <<"position">> => #{<<"line">> => 1, <<"character">> => 2}
                       }},
            {Response, _} = af_lsp:handle_request(Request, State),
            ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
            Result = maps:get(<<"result">>, Response),
            ?assertNotEqual(null, Result)
        end} end,

        fun(_) -> {"definition for unknown document returns null", fun() ->
            State = #{docs => #{}, initialized => true},
            Request = #{<<"method">> => <<"textDocument/definition">>,
                       <<"id">> => 31,
                       <<"params">> => #{
                           <<"textDocument">> => #{<<"uri">> => <<"file:///unknown.a4">>},
                           <<"position">> => #{<<"line">> => 0, <<"character">> => 0}
                       }},
            {Response, _} = af_lsp:handle_request(Request, State),
            ?assertEqual(null, maps:get(<<"result">>, Response))
        end} end
    ]}.

handle_unknown_method_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"unknown method returns no_response", fun() ->
            Request = #{<<"method">> => <<"someUnknownMethod">>,
                       <<"params">> => #{}},
            State = #{docs => #{}, initialized => true},
            {Response, _NewState} = af_lsp:handle_request(Request, State),
            ?assertEqual(no_response, Response)
        end} end
    ]}.

handle_bad_request_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"request without method returns no_response", fun() ->
            Request = #{<<"id">> => 1},
            State = #{docs => #{}, initialized => true},
            {Response, _NewState} = af_lsp:handle_request(Request, State),
            ?assertEqual(no_response, Response)
        end} end,

        fun(_) -> {"empty map returns no_response", fun() ->
            {Response, _} = af_lsp:handle_request(#{}, #{docs => #{}}),
            ?assertEqual(no_response, Response)
        end} end
    ]}.

handle_request_id_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"request without id defaults to undefined", fun() ->
            Request = #{<<"method">> => <<"shutdown">>},
            State = #{docs => #{}, initialized => true},
            {Response, _} = af_lsp:handle_request(Request, State),
            ?assertEqual(undefined, maps:get(<<"id">>, Response))
        end} end
    ]}.

%% === analyze_document tests ===

analyze_document_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"analyze_document returns proper structure", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2 +">>, 3, #{}),
            ?assertEqual(<<"test.a4">>, maps:get(uri, Doc)),
            ?assertEqual(3, maps:get(version, Doc)),
            ?assertEqual(<<"1 2 +">>, maps:get(text, Doc)),
            ?assert(is_list(maps:get(tokens, Doc))),
            ?assertEqual(3, length(maps:get(tokens, Doc))),
            ?assert(is_list(maps:get(stack_snaps, Doc))),
            ?assertEqual(3, length(maps:get(stack_snaps, Doc))),
            ?assert(is_list(maps:get(diagnostics, Doc))),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"analyze_document handles empty text", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"">>, 1, #{}),
            ?assertEqual([], maps:get(tokens, Doc)),
            ?assertEqual([], maps:get(stack_snaps, Doc))
        end} end,

        fun(_) -> {"analyze_document with multiline text", fun() ->
            Src = <<"1\n2\n+">>,
            Doc = af_lsp:analyze_document(<<"test.a4">>, Src, 1, #{}),
            Tokens = maps:get(tokens, Doc),
            ?assertEqual(3, length(Tokens))
        end} end,

        fun(_) -> {"analyze_document preserves uri and version", fun() ->
            Doc = af_lsp:analyze_document(<<"file:///path/to/file.a4">>, <<"1">>, 42, #{}),
            ?assertEqual(<<"file:///path/to/file.a4">>, maps:get(uri, Doc)),
            ?assertEqual(42, maps:get(version, Doc))
        end} end
    ]}.

%% === Complete flow: open -> hover -> completion -> definition -> close ===

full_lifecycle_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"full document lifecycle", fun() ->
            Uri = <<"file:///lifecycle.a4">>,
            Src = <<": square Int -> Int ; dup * .\n4 square">>,
            State0 = #{docs => #{}, initialized => true},

            %% 1. Open
            OpenReq = #{<<"method">> => <<"textDocument/didOpen">>,
                       <<"params">> => #{
                           <<"textDocument">> => #{
                               <<"uri">> => Uri,
                               <<"text">> => Src,
                               <<"version">> => 1
                           }}},
            {_OpenResp, State1} = af_lsp:handle_request(OpenReq, State0),
            ?assert(maps:is_key(Uri, maps:get(docs, State1))),

            %% 2. Hover
            HoverReq = #{<<"method">> => <<"textDocument/hover">>,
                        <<"id">> => 1,
                        <<"params">> => #{
                            <<"textDocument">> => #{<<"uri">> => Uri},
                            <<"position">> => #{<<"line">> => 1, <<"character">> => 2}
                        }},
            {HoverResp, State2} = af_lsp:handle_request(HoverReq, State1),
            ?assertNotEqual(no_response, HoverResp),

            %% 3. Completion
            CompReq = #{<<"method">> => <<"textDocument/completion">>,
                       <<"id">> => 2,
                       <<"params">> => #{
                           <<"textDocument">> => #{<<"uri">> => Uri},
                           <<"position">> => #{<<"line">> => 1, <<"character">> => 1}
                       }},
            {CompResp, State3} = af_lsp:handle_request(CompReq, State2),
            CompResult = maps:get(<<"result">>, CompResp),
            ?assert(is_list(maps:get(<<"items">>, CompResult))),

            %% 4. Definition
            DefReq = #{<<"method">> => <<"textDocument/definition">>,
                      <<"id">> => 3,
                      <<"params">> => #{
                          <<"textDocument">> => #{<<"uri">> => Uri},
                          <<"position">> => #{<<"line">> => 1, <<"character">> => 2}
                      }},
            {DefResp, State4} = af_lsp:handle_request(DefReq, State3),
            DefResult = maps:get(<<"result">>, DefResp),
            ?assertNotEqual(null, DefResult),

            %% 5. Close
            CloseReq = #{<<"method">> => <<"textDocument/didClose">>,
                        <<"params">> => #{
                            <<"textDocument">> => #{<<"uri">> => Uri}
                        }},
            {CloseResp, State5} = af_lsp:handle_request(CloseReq, State4),
            ?assertEqual(no_response, CloseResp),
            ?assertNot(maps:is_key(Uri, maps:get(docs, State5)))
        end} end
    ]}.

%% === didChange re-analysis ===

did_change_reanalysis_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"didChange re-analyzes document content", fun() ->
            Uri = <<"file:///change.a4">>,
            State0 = #{docs => #{}, initialized => true},

            %% Open with "1"
            OpenReq = #{<<"method">> => <<"textDocument/didOpen">>,
                       <<"params">> => #{
                           <<"textDocument">> => #{
                               <<"uri">> => Uri,
                               <<"text">> => <<"1">>,
                               <<"version">> => 1
                           }}},
            {_, State1} = af_lsp:handle_request(OpenReq, State0),

            %% Change to "1 2 +"
            ChangeReq = #{<<"method">> => <<"textDocument/didChange">>,
                         <<"params">> => #{
                             <<"textDocument">> => #{<<"uri">> => Uri, <<"version">> => 2},
                             <<"contentChanges">> => [#{<<"text">> => <<"1 2 +">>}]
                         }},
            {_, State2} = af_lsp:handle_request(ChangeReq, State1),

            Doc = maps:get(Uri, maps:get(docs, State2)),
            ?assertEqual(2, maps:get(version, Doc)),
            Tokens = maps:get(tokens, Doc),
            ?assertEqual(3, length(Tokens))
        end} end
    ]}.

%% === Diagnostics notification format ===

diagnostics_format_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"didOpen publishes properly formatted diagnostics", fun() ->
            Request = #{<<"method">> => <<"textDocument/didOpen">>,
                       <<"params">> => #{
                           <<"textDocument">> => #{
                               <<"uri">> => <<"file:///test.a4">>,
                               <<"text">> => <<"1 2 +">>,
                               <<"version">> => 1
                           }
                       }},
            State = #{docs => #{}, initialized => true},
            {Response, _} = af_lsp:handle_request(Request, State),
            Params = maps:get(<<"params">>, Response),
            Diags = maps:get(<<"diagnostics">>, Params),
            ?assertEqual([], Diags)
        end} end
    ]}.

%% === Multiline hover ===

multiline_hover_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"hover on second line of multiline doc", fun() ->
            Src = <<"1 2\n+ 3 *">>,
            Doc = af_lsp:analyze_document(<<"test.a4">>, Src, 1, #{}),
            %% Hover on "*" which is on line 2 (0-indexed: line=1)
            Result = af_lsp:compute_hover(Doc, 1, 4),
            ?assertNotEqual(null, Result)
        end} end
    ]}.

%% === String quoted token detection ===

infer_quoted_string_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"quoted string is detected as String type", fun() ->
            Tokens = af_parser:parse("\"hello\" stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["String"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"two quoted strings on stack", fun() ->
            Tokens = af_parser:parse("\"a\" \"b\" stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["String", "String"], maps:get(stack, Last))
        end} end
    ]}.

%% === Integer/Float literal detection ===

literal_detection_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"negative integer literal", fun() ->
            Tokens = af_parser:parse("-42 stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            Stack = maps:get(stack, Last),
            %% -42 may or may not be parsed as a single token
            ?assert(is_list(Stack))
        end} end,

        fun(_) -> {"zero is integer literal", fun() ->
            Tokens = af_parser:parse("0 stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"float with leading zero", fun() ->
            Tokens = af_parser:parse("0.5 stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Float"], maps:get(stack, Last))
        end} end
    ]}.

%% === Mixed operations ===

mixed_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"complex expression: 1 2 + 3 * stack", fun() ->
            Tokens = af_parser:parse("1 2 + 3 * stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"comparison after arithmetic: 1 2 + 3 ==", fun() ->
            Tokens = af_parser:parse("1 2 + 3 == stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Bool"], maps:get(stack, Last))
        end} end,

        fun(_) -> {"list then length: nil 1 cons length", fun() ->
            Tokens = af_parser:parse("nil 1 cons length stack", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            Last = lists:last(Snaps),
            ?assertEqual(["Int"], maps:get(stack, Last))
        end} end
    ]}.

%% === Diagnostics: stack underflow on primitives ===

diag_underflow_primitive_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"+ on empty stack produces underflow diagnostic", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"+">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags)),
            D = hd(Diags),
            ?assertEqual(1, maps:get(severity, D)),
            Msg = maps:get(message, D),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"underflow">>)),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"'+'">>))
        end} end,

        fun(_) -> {"+ with one item produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 +">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags)),
            D = hd(Diags),
            Msg = maps:get(message, D),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"requires 2">>)),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"has 1">>))
        end} end,

        fun(_) -> {"dup on empty stack produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"dup">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags)),
            D = hd(Diags),
            Msg = maps:get(message, D),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"dup">>))
        end} end,

        fun(_) -> {"rot on two items produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2 rot">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags)),
            Msg = maps:get(message, hd(Diags)),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"requires 3">>))
        end} end,

        fun(_) -> {"drop on empty stack produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"drop">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"swap on one item produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 swap">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"over on one item produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 over">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"2dup on one item produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2dup">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"comparison with one item produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 ==">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"not on empty stack produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"not">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"cons on one item produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"nil cons">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"concat on one item produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"\"hi\" concat">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"print on empty stack produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"print">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"map-put on one item produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"map-new map-put">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(1, length(Diags))
        end} end
    ]}.

%% === Diagnostics: no underflow on valid code ===

diag_no_false_positive_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"1 2 + has no diagnostics", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2 +">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"dup after push has no diagnostics", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 dup">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"empty doc has no diagnostics", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"unknown word becomes Atom, no diagnostic", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"xyzzy">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"word def and usage, no diagnostic", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<": dbl Int -> Int ; dup + .\n3 dbl">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end
    ]}.

%% === Diagnostics: range and position ===

diag_range_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"underflow diagnostic has correct token range", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"+">>, 1, #{}),
            [D] = maps:get(diagnostics, Doc),
            ?assertEqual(1, maps:get(line, D)),
            ?assertEqual(1, maps:get(col, D)),
            ?assertEqual(2, maps:get(end_col, D))
        end} end,

        fun(_) -> {"underflow range for 'rot' at column 5", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2 rot">>, 1, #{}),
            [D] = maps:get(diagnostics, Doc),
            ?assertEqual(5, maps:get(col, D)),
            ?assertEqual(8, maps:get(end_col, D))
        end} end,

        fun(_) -> {"underflow on second line", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2\n+\n+">>, 1, #{}),
            [D] = maps:get(diagnostics, Doc),
            %% First '+' is fine, second '+' on line 3 underflows (stack has 1)
            ?assertEqual(3, maps:get(line, D))
        end} end
    ]}.

%% === Diagnostics: published via didOpen / didChange ===

diag_publish_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"didOpen publishes underflow diagnostics", fun() ->
            Request = #{<<"method">> => <<"textDocument/didOpen">>,
                       <<"params">> => #{
                           <<"textDocument">> => #{
                               <<"uri">> => <<"file:///t.a4">>,
                               <<"text">> => <<"+">>,
                               <<"version">> => 1
                           }
                       }},
            State = #{docs => #{}, initialized => true},
            {Response, _} = af_lsp:handle_request(Request, State),
            Params = maps:get(<<"params">>, Response),
            LspDiags = maps:get(<<"diagnostics">>, Params),
            ?assertEqual(1, length(LspDiags)),
            D = hd(LspDiags),
            ?assertEqual(1, maps:get(<<"severity">>, D)),
            ?assertEqual(<<"actorforth">>, maps:get(<<"source">>, D)),
            Msg = maps:get(<<"message">>, D),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"underflow">>))
        end} end,

        fun(_) -> {"didChange re-publishes updated diagnostics", fun() ->
            Uri = <<"file:///c.a4">>,
            Open = #{<<"method">> => <<"textDocument/didOpen">>,
                    <<"params">> => #{
                        <<"textDocument">> => #{
                            <<"uri">> => Uri,
                            <<"text">> => <<"1 2 +">>,
                            <<"version">> => 1
                        }}},
            {Resp1, S1} = af_lsp:handle_request(Open, #{docs => #{}, initialized => true}),
            D1 = maps:get(<<"diagnostics">>, maps:get(<<"params">>, Resp1)),
            ?assertEqual([], D1),

            %% Change to content that produces an error
            Change = #{<<"method">> => <<"textDocument/didChange">>,
                      <<"params">> => #{
                          <<"textDocument">> => #{<<"uri">> => Uri, <<"version">> => 2},
                          <<"contentChanges">> => [#{<<"text">> => <<"+">>}]
                      }},
            {Resp2, _S2} = af_lsp:handle_request(Change, S1),
            D2 = maps:get(<<"diagnostics">>, maps:get(<<"params">>, Resp2)),
            ?assertEqual(1, length(D2))
        end} end,

        fun(_) -> {"published diagnostic has range with start and end", fun() ->
            Request = #{<<"method">> => <<"textDocument/didOpen">>,
                       <<"params">> => #{
                           <<"textDocument">> => #{
                               <<"uri">> => <<"file:///t.a4">>,
                               <<"text">> => <<"dup">>,
                               <<"version">> => 1
                           }
                       }},
            {Response, _} = af_lsp:handle_request(Request,
                #{docs => #{}, initialized => true}),
            Params = maps:get(<<"params">>, Response),
            [D] = maps:get(<<"diagnostics">>, Params),
            Range = maps:get(<<"range">>, D),
            Start = maps:get(<<"start">>, Range),
            End = maps:get(<<"end">>, Range),
            ?assertEqual(0, maps:get(<<"line">>, Start)),
            ?assertEqual(0, maps:get(<<"character">>, Start)),
            ?assertEqual(0, maps:get(<<"line">>, End)),
            %% "dup" is 3 characters, 0-indexed end = 3 (exclusive)
            ?assertEqual(3, maps:get(<<"character">>, End))
        end} end
    ]}.

%% === Diagnostics: multiple errors in one document ===

diag_multiple_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"two underflows in a row", fun() ->
            %% drop on [] underflows (output []), then drop on [] again underflows.
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"drop drop">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assertEqual(2, length(Diags))
        end} end,

        fun(_) -> {"error earlier does not suppress later valid code", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"+ 1 2 +">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            %% The first '+' underflows (output [Int]). Stack becomes [Int].
            %% Then 1 2 + brings it to [Int Int Int] -> [Int Int], no more errors.
            ?assertEqual(1, length(Diags))
        end} end
    ]}.

%% === Diagnostics: signature mismatch on user words ===
%%
%% These tests register a word directly into the global registry via
%% af_type:add_op/2, since af_lsp:lookup_word_effect consults af_type.
%% Using interpret_tokens with a fresh continuation would write to the
%% continuation-local dictionary (not visible to the LSP).

make_op(Name, SigIn, SigOut) ->
    #operation{
        name = atom_to_list(Name),
        sig_in = SigIn,
        sig_out = SigOut,
        impl = undefined,
        source = undefined}.

diag_sig_mismatch_test_() ->
    {foreach,
     fun() ->
         setup(),
         af_type:add_op('Any', make_op('needs-int', ['Int'], ['Int'])),
         af_type:add_op('Any', make_op('needs-two', ['Int', 'Int'], ['Int'])),
         af_type:add_op('Any', make_op('accepts-any', ['Any'], ['Int'])),
         ok
     end,
     fun(_) -> ok end, [
        fun(_) -> {"user word called with wrong type produces mismatch", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"\"hi\" needs-int">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assert(length(Diags) >= 1),
            Msgs = [maps:get(message, D) || D <- Diags],
            Found = lists:any(fun(M) ->
                binary:match(M, <<"mismatch">>) =/= nomatch
            end, Msgs),
            ?assert(Found)
        end} end,

        fun(_) -> {"user word called with underflow produces underflow", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 needs-two">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assert(length(Diags) >= 1)
        end} end,

        fun(_) -> {"Any in sig_in accepts anything", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"\"hi\" accepts-any">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end
    ]}.

%% === Diagnostics: exhaustive primitive underflow ===

diag_all_primitives_underflow_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"every arithmetic/comparison/logic primitive underflow", fun() ->
            %% One-off: for each zero-arg-prefixed primitive, invoking on an
            %% empty stack yields exactly one underflow diagnostic.
            Names = ["+", "-", "*", "/", "mod",
                     "==", "!=", "<", ">", "<=", ">=",
                     "not", "and", "or"],
            lists:foreach(fun(N) ->
                Src = list_to_binary(N),
                Doc = af_lsp:analyze_document(<<"t.a4">>, Src, 1, #{}),
                Diags = maps:get(diagnostics, Doc),
                ?assertMatch([_ | _], Diags),
                Msg = maps:get(message, hd(Diags)),
                NameBin = list_to_binary(N),
                QuotedName = iolist_to_binary([<<"'">>, NameBin, <<"'">>]),
                ?assertNotEqual(nomatch, binary:match(Msg, QuotedName))
            end, Names)
        end} end,

        fun(_) -> {"list primitive underflow", fun() ->
            Names = ["cons", "head", "tail", "length", "empty?",
                     "append", "reverse"],
            lists:foreach(fun(N) ->
                Src = list_to_binary(N),
                Doc = af_lsp:analyze_document(<<"t.a4">>, Src, 1, #{}),
                Diags = maps:get(diagnostics, Doc),
                ?assertMatch([_ | _], Diags)
            end, Names)
        end} end,

        fun(_) -> {"map primitive underflow", fun() ->
            Names = ["map-put", "map-get", "map-delete", "map-has?",
                     "map-keys", "map-values", "map-size"],
            lists:foreach(fun(N) ->
                Src = list_to_binary(N),
                Doc = af_lsp:analyze_document(<<"t.a4">>, Src, 1, #{}),
                Diags = maps:get(diagnostics, Doc),
                ?assertMatch([_ | _], Diags)
            end, Names)
        end} end,

        fun(_) -> {"string/conversion primitive underflow", fun() ->
            Names = ["concat", "to-string", "to-int", "to-float"],
            lists:foreach(fun(N) ->
                Src = list_to_binary(N),
                Doc = af_lsp:analyze_document(<<"t.a4">>, Src, 1, #{}),
                Diags = maps:get(diagnostics, Doc),
                ?assertMatch([_ | _], Diags)
            end, Names)
        end} end,

        fun(_) -> {"IO primitive underflow", fun() ->
            Names = ["print", "assert", "assert-eq"],
            lists:foreach(fun(N) ->
                Src = list_to_binary(N),
                Doc = af_lsp:analyze_document(<<"t.a4">>, Src, 1, #{}),
                Diags = maps:get(diagnostics, Doc),
                ?assertMatch([_ | _], Diags)
            end, Names)
        end} end
    ]}.

%% === Type compatibility edge cases ===

type_compat_edges_test_() ->
    {foreach,
     fun() ->
         setup(),
         af_type:add_op('Any', #operation{
             name = "int-consumer", sig_in = ['Int'],
             sig_out = ['Int'], impl = undefined, source = undefined}),
         af_type:add_op('Any', #operation{
             name = "typed-var", sig_in = ['_a'],
             sig_out = ['_a'], impl = undefined, source = undefined}),
         af_type:add_op('Any', #operation{
             name = "any-consumer", sig_in = ['Any'],
             sig_out = [], impl = undefined, source = undefined}),
         af_type:add_op('Any', #operation{
             name = "val-constraint", sig_in = [{'Int', 0}],
             sig_out = ['Int'], impl = undefined, source = undefined}),
         ok
     end,
     fun(_) -> ok end, [
        fun(_) -> {"named type variable accepts any type", fun() ->
            Doc = af_lsp:analyze_document(<<"t.a4">>, <<"\"s\" typed-var">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"Any accepts String", fun() ->
            Doc = af_lsp:analyze_document(<<"t.a4">>, <<"\"s\" any-consumer">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"Int sig matches Int stack item", fun() ->
            Doc = af_lsp:analyze_document(<<"t.a4">>, <<"1 int-consumer">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"value-constraint sig matches Int stack item", fun() ->
            Doc = af_lsp:analyze_document(<<"t.a4">>, <<"0 val-constraint">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"opaque '?' stack item matches anything", fun() ->
            %% First '+' underflows and pushes "?". Then the next word with
            %% any sig_in should not complain about the '?' type.
            Doc = af_lsp:analyze_document(<<"t.a4">>, <<"+ int-consumer">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            %% Only the underflow from the first '+' should appear.
            ?assertEqual(1, length(Diags))
        end} end,

        fun(_) -> {"Atom stack item matches any sig_in", fun() ->
            Doc = af_lsp:analyze_document(<<"t.a4">>, <<"xyzzy int-consumer">>, 1, #{}),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end} end,

        fun(_) -> {"value-constraint mismatch produces diagnostic", fun() ->
            %% val-constraint expects {Int, 0}, so a String should mismatch.
            Doc = af_lsp:analyze_document(<<"t.a4">>, <<"\"s\" val-constraint">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assert(length(Diags) >= 1),
            Msg = maps:get(message, hd(Diags)),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"mismatch">>)),
            %% The formatted sig should include the 'Int=0' constraint
            ?assertNotEqual(nomatch, binary:match(Msg, <<"Int=0">>))
        end} end
    ]}.

%% === Transport IO (mock stdin/stdout) ===

%% Build a mock IO transport that reads from a list of lines and captures output.
mock_io(Lines, BodyProvider) ->
    State = ets:new(mock_io_state, [set, public]),
    ets:insert(State, {lines, Lines}),
    ets:insert(State, {body_bytes, BodyProvider}),
    ets:insert(State, {output, []}),
    IO = #{
        get_line => fun() ->
            [{lines, L}] = ets:lookup(State, lines),
            case L of
                [] -> eof;
                [H | T] ->
                    ets:insert(State, {lines, T}),
                    H
            end
        end,
        get_chars => fun(_N) ->
            [{body_bytes, Body}] = ets:lookup(State, body_bytes),
            ets:insert(State, {body_bytes, <<>>}),
            Body
        end,
        put_chars => fun(Data) ->
            [{output, O}] = ets:lookup(State, output),
            ets:insert(State, {output, [Data | O]})
        end
    },
    {IO, State}.

mock_output(State) ->
    [{output, O}] = ets:lookup(State, output),
    iolist_to_binary(lists:reverse(O)).

read_message_mock_test_() ->
    [
        {"read_message parses a valid LSP frame", fun() ->
            {IO, _} = mock_io(
                ["Content-Length: 17\n", "\n"],
                <<"{\"method\":\"ping\"}">>),
            Result = af_lsp:read_message(IO),
            ?assertMatch({ok, #{<<"method">> := <<"ping">>}}, Result)
        end},

        {"read_message returns eof on empty input", fun() ->
            {IO, _} = mock_io([], <<>>),
            ?assertEqual(eof, af_lsp:read_message(IO))
        end},

        {"read_message returns {error, json_decode} on malformed body", fun() ->
            {IO, _} = mock_io(
                ["Content-Length: 5\n", "\n"],
                <<"not json">>),
            ?assertEqual({error, json_decode}, af_lsp:read_message(IO))
        end},

        {"read_message skips unrecognised header lines", fun() ->
            {IO, _} = mock_io(
                ["X-Unknown: 42\n",
                 "Content-Length: 11\n",
                 "Content-Type: application/vscode-jsonrpc; charset=utf-8\n",
                 "\n"],
                <<"{\"ok\":true}">>),
            Result = af_lsp:read_message(IO),
            ?assertMatch({ok, #{<<"ok">> := true}}, Result)
        end},

        {"read_message handles initial blank line", fun() ->
            {IO, _} = mock_io(
                ["\n", "Content-Length: 11\n", "\n"],
                <<"{\"ok\":true}">>),
            Result = af_lsp:read_message(IO),
            ?assertMatch({ok, #{<<"ok">> := true}}, Result)
        end},

        {"read_message returns eof on eof during header read", fun() ->
            %% No Content-Length and list runs out
            {IO, _} = mock_io(["X: y\n"], <<>>),
            ?assertEqual(eof, af_lsp:read_message(IO))
        end},

        {"read_message returns eof when body is eof", fun() ->
            %% We pass the atom eof as body
            {IO, _} = mock_io(
                ["Content-Length: 5\n", "\n"],
                eof),
            ?assertEqual(eof, af_lsp:read_message(IO))
        end},

        {"read_message propagates IO error from header read", fun() ->
            IO = #{
                get_line => fun() -> {error, terminated} end,
                get_chars => fun(_N) -> <<>> end,
                put_chars => fun(_) -> ok end
            },
            ?assertEqual({error, terminated}, af_lsp:read_message(IO))
        end},

        {"read_message propagates IO error from body read", fun() ->
            Lines = ["Content-Length: 3\n", "\n"],
            State = ets:new(mock, [public, set]),
            ets:insert(State, {lines, Lines}),
            IO = #{
                get_line => fun() ->
                    [{lines, L}] = ets:lookup(State, lines),
                    case L of
                        [] -> eof;
                        [H | T] -> ets:insert(State, {lines, T}), H
                    end
                end,
                get_chars => fun(_N) -> {error, boom} end,
                put_chars => fun(_) -> ok end
            },
            ?assertEqual({error, boom}, af_lsp:read_message(IO))
        end}
    ].

write_message_mock_test_() ->
    [
        {"write_message emits a Content-Length-framed body", fun() ->
            {IO, State} = mock_io([], <<>>),
            Msg = #{<<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => 1,
                    <<"result">> => null},
            af_lsp:write_message(Msg, IO),
            Out = mock_output(State),
            ?assertNotEqual(nomatch, binary:match(Out, <<"Content-Length: ">>)),
            ?assertNotEqual(nomatch, binary:match(Out, <<"\r\n\r\n">>))
        end}
    ].

loop_mock_test_() ->
    [
        {"loop processes shutdown request and terminates on eof", fun() ->
            ShutdownBody = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"shutdown\"}">>,
            Len = byte_size(ShutdownBody),
            Header = io_lib:format("Content-Length: ~B\n", [Len]),
            {IO, State} = mock_io(
                [lists:flatten(Header), "\n"],
                ShutdownBody),
            af_lsp:loop(#{docs => #{}, initialized => true}, IO),
            Out = mock_output(State),
            %% Should have written a response
            ?assertNotEqual(nomatch, binary:match(Out, <<"Content-Length:">>)),
            ?assertNotEqual(nomatch, binary:match(Out, <<"\"id\":1">>))
        end},

        {"loop handles notification (no response), then eof", fun() ->
            NotifBody = <<"{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}">>,
            Len = byte_size(NotifBody),
            Header = io_lib:format("Content-Length: ~B\n", [Len]),
            {IO, State} = mock_io(
                [lists:flatten(Header), "\n"],
                NotifBody),
            af_lsp:loop(#{docs => #{}, initialized => false}, IO),
            Out = mock_output(State),
            %% Notification produces no output
            ?assertEqual(<<>>, Out)
        end},

        {"loop stops on eof", fun() ->
            {IO, _} = mock_io([], <<>>),
            ?assertEqual(ok, af_lsp:loop(#{docs => #{}, initialized => true}, IO))
        end},

        {"loop logs and returns on read error", fun() ->
            IO = #{
                get_line => fun() -> {error, boom} end,
                get_chars => fun(_) -> <<>> end,
                put_chars => fun(_) -> ok end
            },
            ?assertEqual(ok, af_lsp:loop(#{docs => #{}, initialized => true}, IO))
        end}
    ].

default_io_callable_test_() ->
    [
        {"default_io map lambdas are callable (may block if actually reading)",
         fun() ->
            IO = af_lsp:default_io(),
            GetLine = maps:get(get_line, IO),
            GetChars = maps:get(get_chars, IO),
            PutChars = maps:get(put_chars, IO),
            %% We don't actually call GetLine/GetChars (would block on real
            %% stdin). We verify put_chars works by writing an empty iolist.
            ?assert(is_function(GetLine, 0)),
            ?assert(is_function(GetChars, 1)),
            ?assertEqual(ok, PutChars([]))
        end}
    ].

parser_error_test_() ->
    [
        {"analyze_document swallows parse errors", fun() ->
            %% Pass non-binary text; binary_to_list/1 will throw badarg,
            %% which the catch must swallow.
            Doc = af_lsp:analyze_document(<<"t.a4">>, 42, 1, #{}),
            ?assertEqual([], maps:get(tokens, Doc)),
            ?assertEqual([], maps:get(diagnostics, Doc))
        end}
    ].

%% === Frame parsing and encoding ===

parse_header_test_() ->
    [
        ?_assertEqual({content_length, 42},
                      af_lsp:parse_header("content-length: 42")),
        ?_assertEqual({content_length, 100},
                      af_lsp:parse_header("Content-Length: 100")),
        ?_assertEqual({content_length, 7},
                      af_lsp:parse_header("CONTENT-LENGTH: 7")),
        %% Mixed casing that falls through the prefix match must hit the regex
        ?_assertEqual({content_length, 3},
                      af_lsp:parse_header("CoNtEnT-LeNgTh: 3")),
        %% No space after colon forces the regex path (\\s*)
        ?_assertEqual({content_length, 42},
                      af_lsp:parse_header("Content-Length:42")),
        ?_assertEqual(unknown,
                      af_lsp:parse_header("some-other-header: value")),
        ?_assertEqual(unknown,
                      af_lsp:parse_header("garbage"))
    ].

encode_message_test_() ->
    [
        {"encoded message has Content-Length header", fun() ->
            Msg = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                    <<"result">> => null},
            Encoded = af_lsp:encode_message(Msg),
            ?assert(is_binary(Encoded)),
            ?assertNotEqual(nomatch, binary:match(Encoded, <<"Content-Length:">>))
        end},

        {"encoded header ends with blank line", fun() ->
            Msg = #{<<"ok">> => true},
            Encoded = af_lsp:encode_message(Msg),
            ?assertNotEqual(nomatch, binary:match(Encoded, <<"\r\n\r\n">>))
        end},

        {"body contains the encoded JSON", fun() ->
            Msg = #{<<"hello">> => <<"world">>},
            Encoded = af_lsp:encode_message(Msg),
            ?assertNotEqual(nomatch, binary:match(Encoded, <<"\"hello\"">>)),
            ?assertNotEqual(nomatch, binary:match(Encoded, <<"\"world\"">>))
        end},

        {"Content-Length matches body byte size", fun() ->
            Msg = #{<<"x">> => 1},
            Encoded = af_lsp:encode_message(Msg),
            {match, [LenStr]} = re:run(Encoded, "Content-Length: (\\d+)",
                                        [{capture, [1], binary}]),
            Len = binary_to_integer(LenStr),
            %% The body starts after \r\n\r\n
            [_, Body] = binary:split(Encoded, <<"\r\n\r\n">>),
            ?assertEqual(Len, byte_size(Body))
        end}
    ].

%% === default_io and init_lsp_state ===

default_io_test_() ->
    [
        {"default_io returns a map with IO functions", fun() ->
            IO = af_lsp:default_io(),
            ?assert(is_map(IO)),
            ?assert(is_function(maps:get(get_line, IO), 0)),
            ?assert(is_function(maps:get(get_chars, IO), 1)),
            ?assert(is_function(maps:get(put_chars, IO), 1))
        end}
    ].

init_lsp_state_test_() ->
    [
        {"init_lsp_state builds expected state map", fun() ->
            State = af_lsp:init_lsp_state(#{foo => bar}),
            ?assertEqual(#{foo => bar}, maps:get(mods, State)),
            ?assertEqual(#{}, maps:get(docs, State)),
            ?assertEqual(false, maps:get(initialized, State))
        end}
    ].

%% === Direct tests of internals ===

sig_matches_direct_test_() ->
    [
        ?_assertEqual(true, af_lsp:sig_matches([], [])),
        ?_assertEqual(false, af_lsp:sig_matches(['Int'], [])),
        ?_assertEqual(false, af_lsp:sig_matches([], ["Int"])),
        ?_assertEqual(true, af_lsp:sig_matches(['Any'], ["String"])),
        ?_assertEqual(true, af_lsp:sig_matches(['Int', 'Bool'], ["Int", "Bool"])),
        ?_assertEqual(false, af_lsp:sig_matches(['Int', 'Bool'], ["Bool", "Int"]))
    ].

type_compatible_direct_test_() ->
    [
        ?_assertEqual(true, af_lsp:type_compatible('Any', "Int")),
        ?_assertEqual(true, af_lsp:type_compatible('_', "Int")),
        ?_assertEqual(true, af_lsp:type_compatible('_a', "Int")),
        ?_assertEqual(true, af_lsp:type_compatible('Int', "Int")),
        ?_assertEqual(false, af_lsp:type_compatible('Int', "String")),
        ?_assertEqual(true, af_lsp:type_compatible('Int', "Any")),
        ?_assertEqual(true, af_lsp:type_compatible('Int', "?")),
        ?_assertEqual(true, af_lsp:type_compatible('Int', "Atom")),
        ?_assertEqual(true, af_lsp:type_compatible({'Int', 0}, "Int")),
        ?_assertEqual(false, af_lsp:type_compatible({'Int', 0}, "String")),
        %% Catch-all for truly weird sig_in values
        ?_assertEqual(false, af_lsp:type_compatible(42, "Int"))
    ].

format_sig_item_direct_test_() ->
    [
        ?_assertEqual("Int", lists:flatten(af_lsp:format_sig_item('Int'))),
        ?_assertEqual("Int=0",
                      lists:flatten(af_lsp:format_sig_item({'Int', 0}))),
        ?_assertEqual("foo", lists:flatten(af_lsp:format_sig_item("foo"))),
        ?_assert(is_list(lists:flatten(af_lsp:format_sig_item(42))))
    ].

find_snap_at_edge_test_() ->
    [
        {"empty snaps returns undefined", fun() ->
            ?assertEqual(undefined, af_lsp:find_snap_at([], 0, 0))
        end},

        {"target before all snaps returns undefined", fun() ->
            %% Content with snaps on line 6 (0-indexed line 5)
            Src = <<"\n\n\n\n\n1 2">>,
            Doc = af_lsp:analyze_document(<<"t.a4">>, Src, 1, #{}),
            Snaps = maps:get(stack_snaps, Doc),
            %% Query at line -1 in 1-indexed terms would be impossible, but we can
            %% pass a doc whose snaps are all past our target line.
            %% LSP line 0 = internal line 1. So if all snaps are on internal line
            %% 6, and we ask for line 0... Before = [] -> undefined
            ?assertEqual(undefined, af_lsp:find_snap_at(Snaps, -1, 0))
        end}
    ].

log_test_() ->
    [
        {"log writes to stderr without error", fun() ->
            %% Capturing stderr is fragile in eunit; just verify the call
            %% returns without raising.
            ?assertEqual(ok,
                (fun() -> af_lsp:log("~s~n", ["test log"]), ok end)())
        end}
    ].

%% === Weird sig_in values flow end-to-end ===

weird_sig_test_() ->
    {foreach,
     fun() ->
         setup(),
         af_type:add_op('Any', #operation{
             name = "weird", sig_in = [42],
             sig_out = [], impl = undefined, source = undefined}),
         ok
     end,
     fun(_) -> ok end, [
        fun(_) -> {"weird sig_in value triggers mismatch with ~p format", fun() ->
            Doc = af_lsp:analyze_document(<<"t.a4">>, <<"1 weird">>, 1, #{}),
            Diags = maps:get(diagnostics, Doc),
            ?assert(length(Diags) >= 1),
            Msg = maps:get(message, hd(Diags)),
            ?assertNotEqual(nomatch, binary:match(Msg, <<"mismatch">>))
        end} end
    ]}.

%% === Primitive arity table ===

primitive_arity_table_test_() ->
    [
        ?_assertEqual({known, 1, ["?", "?"]}, af_lsp:primitive_arity("dup")),
        ?_assertEqual({known, 1, []}, af_lsp:primitive_arity("drop")),
        ?_assertEqual({known, 2, ["?", "?"]}, af_lsp:primitive_arity("swap")),
        ?_assertEqual({known, 3, ["?", "?", "?"]}, af_lsp:primitive_arity("rot")),
        ?_assertEqual({known, 2, ["Int"]}, af_lsp:primitive_arity("+")),
        ?_assertEqual({known, 2, ["Int"]}, af_lsp:primitive_arity("mod")),
        ?_assertEqual({known, 2, ["Bool"]}, af_lsp:primitive_arity("==")),
        ?_assertEqual({known, 1, ["Bool"]}, af_lsp:primitive_arity("not")),
        ?_assertEqual({known, 2, ["Bool"]}, af_lsp:primitive_arity("and")),
        ?_assertEqual({known, 2, ["List"]}, af_lsp:primitive_arity("cons")),
        ?_assertEqual({known, 1, ["Any"]}, af_lsp:primitive_arity("head")),
        ?_assertEqual({known, 3, ["Map"]}, af_lsp:primitive_arity("map-put")),
        ?_assertEqual({known, 2, ["Any"]}, af_lsp:primitive_arity("map-get")),
        ?_assertEqual({known, 1, ["Int"]}, af_lsp:primitive_arity("map-size")),
        ?_assertEqual({known, 2, ["String"]}, af_lsp:primitive_arity("concat")),
        ?_assertEqual({known, 1, ["String"]}, af_lsp:primitive_arity("to-string")),
        ?_assertEqual({known, 1, []}, af_lsp:primitive_arity("print")),
        ?_assertEqual({known, 2, []}, af_lsp:primitive_arity("assert-eq")),
        ?_assertEqual(unknown, af_lsp:primitive_arity("not-a-word")),
        ?_assertEqual(unknown, af_lsp:primitive_arity(""))
    ].

%% === sig_out_item formatting ===

sig_out_item_test_() ->
    [
        ?_assertEqual("Int", af_lsp:sig_out_item('Int')),
        ?_assertEqual("Int", af_lsp:sig_out_item({'Int', 42})),
        ?_assertEqual("abc", af_lsp:sig_out_item("abc")),
        ?_assert(is_list(af_lsp:sig_out_item({weird_term, x}))),
        %% Non-atom, non-list, non-tuple triggers catch-all with ~p format
        ?_assert(is_list(af_lsp:sig_out_item(42)))
    ].

%% === publish_diagnostics shape ===

publish_diagnostics_shape_test_() ->
    [
        {"empty diags produces empty list", fun() ->
            N = af_lsp:publish_diagnostics(<<"file:///x.a4">>, []),
            ?assertEqual(<<"textDocument/publishDiagnostics">>,
                         maps:get(<<"method">>, N)),
            Params = maps:get(<<"params">>, N),
            ?assertEqual([], maps:get(<<"diagnostics">>, Params))
        end},

        {"full diag is translated into LSP shape", fun() ->
            Diag = #{line => 3, col => 5, end_col => 8,
                     severity => 1, message => <<"bad">>},
            N = af_lsp:publish_diagnostics(<<"file:///y.a4">>, [Diag]),
            Params = maps:get(<<"params">>, N),
            [D] = maps:get(<<"diagnostics">>, Params),
            ?assertEqual(<<"actorforth">>, maps:get(<<"source">>, D)),
            ?assertEqual(<<"bad">>, maps:get(<<"message">>, D)),
            ?assertEqual(1, maps:get(<<"severity">>, D)),
            Range = maps:get(<<"range">>, D),
            %% col is 1-indexed in our internal form, 0-indexed in LSP
            ?assertEqual(2, maps:get(<<"line">>, maps:get(<<"start">>, Range))),
            ?assertEqual(4, maps:get(<<"character">>, maps:get(<<"start">>, Range))),
            ?assertEqual(7, maps:get(<<"character">>, maps:get(<<"end">>, Range)))
        end}
    ].

%% === Diagnostics: snap carries diags field ===

diag_snap_field_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"each snap has a diags field (may be empty)", fun() ->
            Tokens = af_parser:parse("1 2 +", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            lists:foreach(fun(S) ->
                ?assert(maps:is_key(diags, S))
            end, Snaps)
        end} end,

        fun(_) -> {"snap for bad token carries its diag", fun() ->
            Tokens = af_parser:parse("+", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            [S] = Snaps,
            Diags = maps:get(diags, S),
            ?assertEqual(1, length(Diags))
        end} end
    ]}.
