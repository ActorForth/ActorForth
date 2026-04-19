-module(af_type_strict_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

%% Extended tests for the strict type system: bracket literals, quoted
%% strings, boolean literals, auto-field bindings, forward-reference
%% deferral, and end-to-end compilation positives / negatives.

setup() ->
    af_type:reset().

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

eval_new(Input) ->
    eval(Input, af_interpreter:new_continuation()).


%% -------------------------------------------------------------------
%% Bracket literal `[ ... ]` resolution
%% -------------------------------------------------------------------

bracket_literal_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"empty bracket yields List", fun() ->
            StartOp = #operation{name = "["},
            EndOp = #operation{name = "]"},
            {ok, Result} = af_type_check:infer_stack([StartOp, EndOp], []),
            ?assertEqual(['List'], Result)
        end} end,

        fun(_) -> {"bracket collapses inner items to one List", fun() ->
            StartOp = #operation{name = "["},
            One = #operation{name = "1"},
            Two = #operation{name = "2"},
            EndOp = #operation{name = "]"},
            {ok, Result} = af_type_check:infer_stack(
                [StartOp, One, Two, EndOp], []),
            ?assertEqual(['List'], Result)
        end} end,

        fun(_) -> {"nested brackets collapse into nested List types", fun() ->
            StartOp = #operation{name = "["},
            EndOp = #operation{name = "]"},
            %% [ [ 1 ] ]
            Body = [StartOp, StartOp,
                    #operation{name = "1"},
                    EndOp, EndOp],
            {ok, Result} = af_type_check:infer_stack(Body, []),
            ?assertEqual(['List'], Result)
        end} end,

        fun(_) -> {"unbalanced `]` without matching `[` reports error", fun() ->
            EndOp = #operation{name = "]"},
            Result = af_type_check:infer_stack([EndOp], ['Int']),
            ?assertMatch({error, {unbalanced_bracket, "]", _}}, Result)
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Quoted string literal → String
%% -------------------------------------------------------------------

string_literal_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"quoted-string body op pushes String", fun() ->
            QStr = #operation{name = "hello", source = quoted_string},
            {ok, Result} = af_type_check:infer_stack([QStr], []),
            ?assertEqual(['String'], Result)
        end} end,

        fun(_) -> {"quoted-string concatenation type checks", fun() ->
            %% Compile a full word: : greet -> String ; "hello" "world" concat .
            setup(),
            eval_new(": greet -> String ; \"hello\" \"world\" concat ."),
            {ok, _Op} = af_type:find_op_by_name("greet", 'Any'),
            ok
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Boolean literals True / False → Bool
%% -------------------------------------------------------------------

bool_literal_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"True token pushes Bool", fun() ->
            TrueOp = #operation{name = "True"},
            {ok, Result} = af_type_check:infer_stack([TrueOp], []),
            ?assertEqual(['Bool'], Result)
        end} end,

        fun(_) -> {"False token pushes Bool", fun() ->
            FalseOp = #operation{name = "False"},
            {ok, Result} = af_type_check:infer_stack([FalseOp], []),
            ?assertEqual(['Bool'], Result)
        end} end,

        fun(_) -> {"lowercase true/false also recognised", fun() ->
            T = #operation{name = "true"},
            F = #operation{name = "false"},
            {ok, R1} = af_type_check:infer_stack([T], []),
            {ok, R2} = af_type_check:infer_stack([F], []),
            ?assertEqual(['Bool'], R1),
            ?assertEqual(['Bool'], R2)
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Auto-field bindings from product-type sig_in
%% -------------------------------------------------------------------

auto_field_binding_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"bare field name resolves to field type via virtual locals", fun() ->
            %% Define a product type and compile a word that uses .field notation.
            eval_new("type Point x Int y Int ."),
            eval_new(": getx Point -> Point Int ; x ."),
            {ok, _Op} = af_type:find_op_by_name("getx", 'Point'),
            ok
        end} end,

        fun(_) -> {"dot-prefix .x works when unambiguous", fun() ->
            eval_new("type P2 a Int b Int ."),
            eval_new(": fstbare P2 -> P2 Int ; .a ."),
            {ok, _Op} = af_type:find_op_by_name("fstbare", 'P2'),
            ok
        end} end,

        fun(_) -> {"two-product word uses positional .x / ..x", fun() ->
            %% Non-destructive field access: the two P3 instances stay on the
            %% stack, so the declared sig must acknowledge that.
            eval_new("type P3 a Int b Int ."),
            eval_new(": distx P3 P3 -> P3 P3 Int ; ..a .a - ."),
            {ok, _Op} = af_type:find_op_by_name("distx", 'P3'),
            ok
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Forward-reference deferral
%% -------------------------------------------------------------------

defer_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"self-recursion resolves on same-word registration", fun() ->
            %% countdown pattern: self-recursive, no forward ref needed
            %% because the word is added before its body is checked
            %% (reorder fix in register_single_word / register_multi_word).
            eval_new(": ctd Int -> ; : 0 -> ; drop : Int -> ; 1 - ctd ."),
            {ok, _Op} = af_type:find_op_by_name("ctd", 'Int'),
            ok
        end} end,

        fun(_) -> {"mutual recursion across two words resolves via defer+retry", fun() ->
            %% even? calls odd?, odd? calls even? — two-word cycle.
            %% Defer should hold the first word's check until the second
            %% is registered, then retry and succeed.
            eval_new(
                ": evn Int -> Bool ;"
                "    : 0 -> Bool ; drop True"
                "    : Int -> Bool ; 1 - odd ."),
            eval_new(
                ": odd Int -> Bool ;"
                "    : 0 -> Bool ; drop False"
                "    : Int -> Bool ; 1 - evn ."),
            {ok, _} = af_type:find_op_by_name("evn", 'Int'),
            {ok, _} = af_type:find_op_by_name("odd", 'Int'),
            ok
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Known-name sig-mismatch: reports stack_underflow, not Atom push
%% -------------------------------------------------------------------

known_name_sig_mismatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"swap on single-item stack reports stack_underflow", fun() ->
            %% Previously: swap with 1-item stack would fall through to
            %% Atom push because match_sig failed (not 2 items). That
            %% masked real underflows. Now we look up by name across
            %% types and apply the sig, surfacing the error.
            SwapOp = #operation{name = "swap"},
            Result = af_type_check:infer_stack([SwapOp], ['Int']),
            ?assertMatch({error, {stack_underflow, "swap", _}}, Result)
        end} end,

        fun(_) -> {"drop on empty stack reports stack_underflow", fun() ->
            DropOp = #operation{name = "drop"},
            Result = af_type_check:infer_stack([DropOp], []),
            ?assertMatch({error, {stack_underflow, "drop", _}}, Result)
        end} end
    ]}.


%% -------------------------------------------------------------------
%% End-to-end: well-typed words compile; mistyped words raise with a
%% useful error message.
%% -------------------------------------------------------------------

end_to_end_positive_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"simple arithmetic word compiles", fun() ->
            eval_new(": sqr Int -> Int ; dup * ."),
            ok
        end} end,

        fun(_) -> {"word using bracket literal compiles", fun() ->
            eval_new(": mklist Int Int -> List ; [ ] swap cons swap cons ."),
            ok
        end} end,

        fun(_) -> {"word using quoted string compiles", fun() ->
            eval_new(": hello -> String ; \"Hello, world!\" ."),
            ok
        end} end,

        fun(_) -> {"word with Bool literal compiles", fun() ->
            eval_new(": yes -> Bool ; True ."),
            ok
        end} end,

        fun(_) -> {"word with sub-clause value dispatch compiles", fun() ->
            eval_new(": fact Int -> Int ;"
                     " : 0 -> Int ; drop 1"
                     " : Int -> Int ; dup 1 - fact * ."),
            ok
        end} end,

        fun(_) -> {"word with named type variable compiles", fun() ->
            eval_new(": nip2 _a _b -> _b ; swap drop ."),
            ok
        end} end
    ]}.


end_to_end_negative_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"declared output type != produced raises type_error", fun() ->
            try
                eval_new(": broken Int -> String ; dup + ."),
                ?assert(false)
            catch
                error:{type_error, "broken", Msg} ->
                    ?assert(is_list(Msg)),
                    ?assertNotEqual(nomatch, string:find(Msg, "String")),
                    ?assertNotEqual(nomatch, string:find(Msg, "Int"))
            end
        end} end,

        fun(_) -> {"more outputs than declared raises type_error", fun() ->
            try
                eval_new(": toomany Int -> Int ; dup ."),
                ?assert(false)
            catch
                error:{type_error, "toomany", _} -> ok
            end
        end} end,

        fun(_) -> {"fewer outputs than declared raises type_error", fun() ->
            try
                eval_new(": toofew Int Int -> Int Int ; + ."),
                ?assert(false)
            catch
                error:{type_error, "toofew", _} -> ok
            end
        end} end,

        fun(_) -> {"error message mentions the word name", fun() ->
            try
                eval_new(": woof Int -> String ; ."),
                ?assert(false)
            catch
                error:{type_error, Name, Msg} ->
                    ?assertEqual("woof", Name),
                    ?assertNotEqual(nomatch, string:find(Msg, "woof"))
            end
        end} end,

        fun(_) -> {"error message contains declared vs inferred", fun() ->
            try
                eval_new(": flipTypes Int -> Bool ; ."),
                ?assert(false)
            catch
                error:{type_error, _, Msg} ->
                    ?assertNotEqual(nomatch, string:find(Msg, "declared")),
                    ?assertNotEqual(nomatch, string:find(Msg, "inferred"))
            end
        end} end
    ]}.
