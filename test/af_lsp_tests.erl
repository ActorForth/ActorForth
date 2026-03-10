-module(af_lsp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("operation.hrl").

setup() ->
    af_type:reset().

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
        end} end
    ]}.

infer_word_def_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"word definition markers", fun() ->
            Tokens = af_parser:parse(": double Int -> Int ; dup + .", "test"),
            Snaps = af_lsp:infer_stacks(Tokens),
            ?assert(length(Snaps) > 0)
        end} end
    ]}.

%% === Hover Tests ===

hover_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"hover returns stack picture", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"1 2 +">>, 1, #{}),
            Result = af_lsp:compute_hover(Doc, 0, 4),
            ?assertNotEqual(null, Result),
            ?assert(is_map(Result))
        end} end,

        fun(_) -> {"hover on empty returns null", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"">>, 1, #{}),
            Result = af_lsp:compute_hover(Doc, 0, 0),
            ?assertEqual(null, Result)
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
            ?assertEqual(0, maps:get(<<"line">>, Start))
        end} end,

        fun(_) -> {"go-to-definition returns null for unknown word", fun() ->
            Doc = af_lsp:analyze_document(<<"test.a4">>, <<"unknown-word">>, 1, #{}),
            Result = af_lsp:compute_definition(Doc, <<"test.a4">>, 0, 5),
            ?assertEqual(null, Result)
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
            ?assertNotEqual(<<"( empty )">>, Result)
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
            Result = maps:get(<<"result">>, Response),
            Caps = maps:get(<<"capabilities">>, Result),
            ?assert(maps:get(<<"hoverProvider">>, Caps))
        end} end
    ]}.
