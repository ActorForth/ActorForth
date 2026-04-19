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
    af_type:reset(),
    af_type_compiler:clear_pending_checks().

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


%% -------------------------------------------------------------------
%% Pre-dispatch body + sub-clauses (the classic `over 0 == : True ... `
%% pattern). The parasitic compiler used to silently discard the
%% pre-dispatch body; it now preserves it and registers the word as a
%% single op whose body runs pre-dispatch and then a runtime
%% select-clause dispatcher.
%% -------------------------------------------------------------------

predispatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"pre-dispatch Bool matcher: true branch runs", fun() ->
            eval_new(
                ": choose Int -> Int ;"
                "    dup 0 <"
                "    : True -> Int ; drop 0 swap - "
                "    : False -> Int ; drop ."),
            R = eval_new("-5 choose"),
            ?assertEqual([{'Int', 5}], R#continuation.data_stack)
        end} end,

        fun(_) -> {"pre-dispatch Bool matcher: false branch runs", fun() ->
            eval_new(
                ": choose Int -> Int ;"
                "    dup 0 <"
                "    : True -> Int ; drop 0 swap -"
                "    : False -> Int ; drop ."),
            R = eval_new("7 choose"),
            ?assertEqual([{'Int', 7}], R#continuation.data_stack)
        end} end,

        fun(_) -> {"pop-n: recursive, uses pre-dispatch + sub-clauses in two words", fun() ->
            %% The historically-broken pop-n pattern. pop-n and
            %% pop-n-step mutually recurse; the pre-dispatch body in
            %% each does `over 0 ==` / `dup empty?` respectively.
            C0 = af_interpreter:new_continuation(),
            C1 = eval(
                ": pop-n Int List -> List ;"
                "    over 0 =="
                "    : True -> List ; drop swap drop"
                "    : False -> List ; drop pop-n-step .", C0),
            C2 = eval(
                ": pop-n-step Int List -> List ;"
                "    dup empty?"
                "    : True -> List ; drop swap drop"
                "    : False -> List ; drop swap 1 - swap tail pop-n .", C1),
            C3 = eval("3 [ 1 2 3 4 5 ] pop-n", C2),
            ?assertMatch([{'List', [{'Int', 4}, {'Int', 5}]}],
                         C3#continuation.data_stack)
        end} end,

        fun(_) -> {"pre-dispatch tokens are preserved (regression)", fun() ->
            %% Previously the compiler silently dropped `42 +`; the word
            %% would then always return its input unchanged. With the
            %% fix, the pre-dispatch runs and the sub-clause sees the
            %% modified value.
            C0 = af_interpreter:new_continuation(),
            C1 = eval(
                ": plus42-then-sign Int -> Int ;"
                "    42 + dup 0 <"
                "    : True -> Int ; drop drop -1"
                "    : False -> Int ; drop drop 1 .", C0),
            R1 = eval("-100 plus42-then-sign", C1),
            ?assertEqual([{'Int', -1}], R1#continuation.data_stack),
            R2 = eval("10 plus42-then-sign", C1),
            ?assertEqual([{'Int', 1}], R2#continuation.data_stack)
        end} end
    ]}.


%% -------------------------------------------------------------------
%% Deferred type-check visibility — the REPL-first driver requires
%% unresolved forward references to be surfaced, not silently carried.
%% -------------------------------------------------------------------

defer_visibility_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"unresolved forward reference stays pending", fun() ->
            %% `typo?` never defined — the check for `refers-typo` is
            %% deferred, and nothing resolves it.
            eval_new(": refers-typo Int -> Int ; typo? ."),
            Pending = af_type_compiler:list_pending_checks(),
            ?assert(lists:any(fun({N, _, _}) -> N =:= "refers-typo" end, Pending))
        end} end,

        fun(_) -> {"mutual recursion resolves both pending entries", fun() ->
            %% odd/even? point at each other. Both start pending; after
            %% both are defined the retry loop drains the list.
            eval_new(
                ": odd? Int -> Bool ;"
                "    : 0 -> Bool ; drop False"
                "    : Int -> Bool ; 1 - even? ."),
            eval_new(
                ": even? Int -> Bool ;"
                "    : 0 -> Bool ; drop True"
                "    : Int -> Bool ; 1 - odd? ."),
            Pending = af_type_compiler:list_pending_checks(),
            ?assertNot(lists:any(
                fun({N, _, _}) -> N =:= "odd?" orelse N =:= "even?" end,
                Pending))
        end} end,

        fun(_) -> {"finalize_pending_checks returns {pending, List} when unresolved", fun() ->
            eval_new(": forward-only Int -> Int ; never-defined ."),
            Result = af_type_compiler:finalize_pending_checks(),
            ?assertMatch({pending, _}, Result)
        end} end,

        fun(_) -> {"finalize_pending_checks returns ok when none pending", fun() ->
            eval_new(": just-dup Int -> Int Int ; dup ."),
            Result = af_type_compiler:finalize_pending_checks(),
            ?assertEqual(ok, Result)
        end} end,

        fun(_) -> {"clear_pending_checks empties the list", fun() ->
            eval_new(": bad Int -> Int ; missing ."),
            ?assertNotEqual([], af_type_compiler:list_pending_checks()),
            af_type_compiler:clear_pending_checks(),
            ?assertEqual([], af_type_compiler:list_pending_checks())
        end} end,

        fun(_) -> {"visualizer-style mutual recursion drains to zero", fun() ->
            %% Regression: the HOS visualizer's show-fmap / show-fm-children /
            %% show-fm-children-step chain used to leave 2 entries pending
            %% because (a) Any on the stack didn't match concrete sig types,
            %% and (b) value-constrained sub-clauses couldn't dispatch
            %% through the checker when the stack value was undefined. Both
            %% are fixed in match_sig; all three words should now fully
            %% resolve.
            eval_new("type FMNode name String children List ."),
            %% Intentionally define in forward-reference order.
            eval_new(
                ": show-step List String Bool -> ;"
                "    : List String True -> ; drop drop drop"
                "    : List String False -> ;"
                "        drop over head over show-one "
                "        swap tail swap show-kids ."),
            eval_new(
                ": show-kids List String -> ;"
                "    over empty? show-step ."),
            eval_new(
                ": show-one FMNode String -> ;"
                "    over over drop drop "
                "    swap children rot \"  \" concat rot drop "
                "    show-kids ."),
            af_type_compiler:finalize_pending_checks(),
            ?assertEqual([], af_type_compiler:list_pending_checks())
        end} end
    ]}.


%% -------------------------------------------------------------------
%% REPL-first property: the same code that works in a file must work
%% when typed incrementally, one word at a time, into the interpreter.
%% -------------------------------------------------------------------

repl_first_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"mutual recursion typed word-at-a-time", fun() ->
            %% Each call to eval/2 simulates a separate REPL line.
            C0 = af_interpreter:new_continuation(),
            C1 = eval(
                ": odd2? Int -> Bool ;"
                "    : 0 -> Bool ; drop False"
                "    : Int -> Bool ; 1 - even2? .", C0),
            %% At this point `odd2?`'s check is deferred (even2? unknown).
            ?assert(lists:any(
                fun({N, _, _}) -> N =:= "odd2?" end,
                af_type_compiler:list_pending_checks())),
            C2 = eval(
                ": even2? Int -> Bool ;"
                "    : 0 -> Bool ; drop True"
                "    : Int -> Bool ; 1 - odd2? .", C1),
            %% Both defined — retry drains the list.
            ?assertNot(lists:any(
                fun({N, _, _}) -> N =:= "odd2?" orelse N =:= "even2?" end,
                af_type_compiler:list_pending_checks())),
            %% Works at runtime.
            C3 = eval("5 odd2?", C2),
            ?assertEqual([{'Bool', true}], C3#continuation.data_stack)
        end} end,

        fun(_) -> {"self-recursive word defined in one line", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(
                ": fact Int -> Int ;"
                "    : 0 -> Int ; drop 1"
                "    : Int -> Int ; dup 1 - fact * .", C0),
            R = eval("5 fact", C1),
            ?assertEqual([{'Int', 120}], R#continuation.data_stack)
        end} end,

        fun(_) -> {"pre-dispatch word typed incrementally alongside its helper", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(
                ": pop-n2 Int List -> List ;"
                "    over 0 =="
                "    : True -> List ; drop swap drop"
                "    : False -> List ; drop pop-n-step2 .", C0),
            C2 = eval(
                ": pop-n-step2 Int List -> List ;"
                "    dup empty?"
                "    : True -> List ; drop swap drop"
                "    : False -> List ; drop swap 1 - swap tail pop-n2 .", C1),
            C3 = eval("2 [ 10 20 30 40 ] pop-n2", C2),
            ?assertMatch([{'List', [{'Int', 30}, {'Int', 40}]}],
                         C3#continuation.data_stack)
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
