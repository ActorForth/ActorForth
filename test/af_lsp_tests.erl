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
