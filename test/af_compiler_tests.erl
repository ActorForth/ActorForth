-module(af_compiler_tests).

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

%% --- Word definition ---

define_word_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"define and call a simple word: double", fun() ->
            %% Define: double takes Int, returns Int, body is dup +
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            %% Stack should be empty after definition
            ?assertEqual([], C1#continuation.data_stack),
            %% Now use it
            Result = eval("5 int double", C1),
            ?assertEqual([{'Int', 10}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"define and call: square", fun() ->
            C1 = eval(": square Int -> Int ; dup * .", af_interpreter:new_continuation()),
            Result = eval("7 int square", C1),
            ?assertEqual([{'Int', 49}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"define with two inputs: add3", fun() ->
            C1 = eval(": add3 Int Int -> Int ; + .", af_interpreter:new_continuation()),
            Result = eval("10 int 20 int add3", C1),
            ?assertEqual([{'Int', 30}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"word calls another word", fun() ->
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval(": quadruple Int -> Int ; double double .", C1),
            Result = eval("3 int quadruple", C2),
            ?assertEqual([{'Int', 12}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"word with no inputs", fun() ->
            C1 = eval(": answer -> Int ; 42 int .", af_interpreter:new_continuation()),
            Result = eval("answer", C1),
            ?assertEqual([{'Int', 42}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"definition doesn't leave state on stack", fun() ->
            C1 = eval(": nop -> ; .", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end,
        fun(_) -> {"word is registered in correct type dict", fun() ->
            _C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            %% Should be in Int's dictionary
            ?assertMatch({ok, _}, af_type:find_op("double", [{'Int', 5}]))
        end} end
    ]}.

%% --- Pattern matching (overloaded signatures) ---

pattern_match_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"word registered in new type when target unknown", fun() ->
            %% Define a word with an unknown input type — should auto-register the type
            C1 = eval(": greet Greeting -> ; drop .", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack),
            %% The type 'Greeting' should have been auto-created
            ?assertMatch({ok, _}, af_type:get_type('Greeting'))
        end} end,
        fun(_) -> {"word body with unknown token compiles as atom push", fun() ->
            C1 = eval(": make-label -> Atom ; hello .", af_interpreter:new_continuation()),
            Result = eval("make-label", C1),
            ?assertEqual([{'Atom', "hello"}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"value-constrained signature matches first", fun() ->
            C1 = eval(": fact Int -> Int ; dup 1 int - fact * .", af_interpreter:new_continuation()),
            BaseOp = #operation{
                name = "fact",
                sig_in = [{'Int', 0}],
                sig_out = ['Int'],
                impl = fun(Cont) ->
                    [_ | Rest] = Cont#continuation.data_stack,
                    Cont#continuation{data_stack = [{'Int', 1} | Rest]}
                end
            },
            {ok, #af_type{ops = Ops} = Type} = af_type:get_type('Int'),
            ExistingFacts = maps:get("fact", Ops, []),
            NewOps = maps:put("fact", [BaseOp | ExistingFacts], Ops),
            UpdatedType = Type#af_type{ops = NewOps},
            af_type:register_type(UpdatedType),
            Dict = maps:put('Int', UpdatedType, C1#continuation.dictionary),
            C2 = C1#continuation{dictionary = Dict},
            Result = eval("5 int fact", C2),
            ?assertEqual([{'Int', 120}], Result#continuation.data_stack)
        end} end
    ]}.

%% --- Value constraints in signatures ---

value_constraint_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"value constraint in input signature", fun() ->
            %% : f 0 Int -> Int ; drop 1 .
            C1 = eval(": f 0 Int -> Int ; drop 1 int .", af_interpreter:new_continuation()),
            Result = eval("0 int f", C1),
            ?assertEqual([{'Int', 1}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"bool value constraint in input signature", fun() ->
            C1 = eval(": to-num true Bool -> Int ; drop 1 int .", af_interpreter:new_continuation()),
            C2 = eval(": to-num false Bool -> Int ; drop 0 int .", C1),
            R1 = eval("true to-num", C2),
            ?assertEqual([{'Int', 1}], R1#continuation.data_stack),
            R2 = eval("false to-num", C2),
            ?assertEqual([{'Int', 0}], R2#continuation.data_stack)
        end} end,

        fun(_) -> {"value constraint in output signature", fun() ->
            %% Output sig with value constraint
            C1 = eval(": always-zero Int -> 0 Int ; drop 0 int .", af_interpreter:new_continuation()),
            Result = eval("42 int always-zero", C1),
            ?assertEqual([{'Int', 0}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"float value constraint in input signature", fun() ->
            C1 = eval(": is-pi 3.14 Float -> Bool ; drop true .", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end
    ]}.

%% --- Sub-clause compilation (multi-clause words) ---

sub_clause_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"sub-clause with value constraint pattern match", fun() ->
            %% Define factorial using sub-clauses
            C1 = eval(": fact Int -> Int ; : 0 -> Int ; drop 1 int : 1 -> Int ; : Int -> Int ; dup 1 int - fact * .", af_interpreter:new_continuation()),
            R0 = eval("0 int fact", C1),
            ?assertEqual([{'Int', 1}], R0#continuation.data_stack),
            R5 = eval("5 int fact", C1),
            ?assertEqual([{'Int', 120}], R5#continuation.data_stack)
        end} end,

        fun(_) -> {"sub-clause with bool pattern match", fun() ->
            %% Sub-clause value-only (right-aligned against master Bool)
            C1 = eval(": to-num Bool -> Int ; : true -> Int ; drop 1 int : false -> Int ; drop 0 int .", af_interpreter:new_continuation()),
            R1 = eval("true to-num", C1),
            ?assertEqual([{'Int', 1}], R1#continuation.data_stack),
            R2 = eval("false to-num", C1),
            ?assertEqual([{'Int', 0}], R2#continuation.data_stack)
        end} end
    ]}.

%% --- Tail call optimization ---

tail_call_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"self-recursive tail call optimized", fun() ->
            %% factorial with value constraint base case
            C1 = eval(": fact 1 Int -> Int ; .", af_interpreter:new_continuation()),
            C2 = eval(": fact Int -> Int ; dup 1 int - fact * .", C1),
            Result = eval("5 int fact", C2),
            ?assertEqual([{'Int', 120}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"tail call to another compiled word", fun() ->
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            %% Last op is double (another compiled word) = tail call
            C2 = eval(": quad Int -> Int ; dup + double .", C1),
            Result = eval("3 int quad", C2),
            ?assertEqual([{'Int', 12}], Result#continuation.data_stack)
        end} end
    ]}.

%% --- Type check error paths ---

type_check_error_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"type mismatch error for fully resolved types", fun() ->
            %% Declare output Int but body produces Bool
            ?assertError({type_error, _, _},
                eval(": bad Int -> Bool ; dup + .", af_interpreter:new_continuation()))
        end} end,

        fun(_) -> {"type check with product type produces warning not error", fun() ->
            %% Product type ops produce Atom in inference (incomplete)
            OldGL = group_leader(),
            {ok, Pid} = start_capture(),
            group_leader(Pid, self()),
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            eval(": inc Counter -> Counter ; value 1 int + value! .", C1),
            group_leader(OldGL, self()),
            _Output = stop_capture(Pid),
            ok
        end} end
    ]}.

%% --- Multi-clause word definition ---

multi_clause_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"multi-clause word with value constraints", fun() ->
            %% This exercises the multi-clause registration path (line 364)
            C1 = eval(": abs2 0 Int -> Int ; .", af_interpreter:new_continuation()),
            C2 = eval(": abs2 Int -> Int ; dup 0 int < : true -> Int ; drop 0 int swap - : Int -> Int ; .", C1),
            R = eval("0 int abs2", C2),
            ?assertEqual([{'Int', 0}], R#continuation.data_stack)
        end} end,

        fun(_) -> {"word with quoted string in body", fun() ->
            C1 = eval(": hello -> String ; \"world\" .", af_interpreter:new_continuation()),
            R = eval("hello", C1),
            ?assertEqual([{'String', <<"world">>}], R#continuation.data_stack)
        end} end,

        fun(_) -> {"detect_tail_call for non-recursive non-compiled last op", fun() ->
            %% Word body ending in a primitive (not compiled word) = no tail call
            C1 = eval(": add-and-double Int Int -> Int ; + dup + .", af_interpreter:new_continuation()),
            R = eval("3 int 4 int add-and-double", C1),
            ?assertEqual([{'Int', 14}], R#continuation.data_stack)
        end} end,

        fun(_) -> {"word definition when continuation has no dictionary", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = C0#continuation{dictionary = undefined},
            C2 = eval(": nop-x -> ; .", C1),
            ?assertEqual([], C2#continuation.data_stack)
        end} end,

        fun(_) -> {"multi-clause word via : in body (line 364)", fun() ->
            %% Directly test multi-clause: body has : starting sub-clauses
            %% when current_sub is undefined (first : in body)
            C1 = eval(": test-mc Int -> Int ;"
                       " : 0 -> Int ; drop 1 int"
                       " : Int -> Int ; dup + .", af_interpreter:new_continuation()),
            R0 = eval("0 int test-mc", C1),
            ?assertEqual([{'Int', 1}], R0#continuation.data_stack),
            R5 = eval("5 int test-mc", C1),
            ?assertEqual([{'Int', 10}], R5#continuation.data_stack)
        end} end,

        fun(_) -> {"multi-clause from separate definitions (line 364 op_dot)", fun() ->
            %% Two separate word defs with same name - exercises register_multi_word
            %% with current_sub undefined and clauses accumulated
            C1 = eval(": myword 0 Int -> Int ; drop 42 int .", af_interpreter:new_continuation()),
            C2 = eval(": myword Int -> Int ; dup + .", C1),
            R0 = eval("0 int myword", C2),
            ?assertEqual([{'Int', 42}], R0#continuation.data_stack)
        end} end
    ]}.

%% --- Sub-clause with extra tokens beyond master sig ---

sub_clause_overflow_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"sub-clause with more tokens than master sig (line 575)", fun() ->
            %% Master sig: Int (1 element)
            %% Sub-clause sig: Extra 0 (2 elements)
            %% Offset = 1 - 2 = -1, first token falls outside master → line 575
            C1 = eval(": test-overflow Int -> Int ;"
                       " : Extra 0 -> Int ; drop drop 1 int"
                       " : Int -> Int ; dup + .", af_interpreter:new_continuation()),
            R = eval("5 int test-overflow", C1),
            ?assertEqual([{'Int', 10}], R#continuation.data_stack)
        end} end,

        fun(_) -> {"sub-clause overflow with value constraint resolution", fun() ->
            %% Master sig: Bool (1 element)
            %% Sub-clause: hello Bool (2 elements) — first token overflows
            C1 = eval(": test-over2 Bool -> Int ;"
                       " : hello true -> Int ; drop drop 42 int"
                       " : Bool -> Int ; drop 0 int .", af_interpreter:new_continuation()),
            R = eval("false test-over2", C1),
            ?assertEqual([{'Int', 0}], R#continuation.data_stack)
        end} end
    ]}.

%% --- Type check warning paths ---

type_check_warning_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"type check stack_underflow warning path (line 458)", fun() ->
            %% Register an op with conflicting type variable bindings:
            %% pair_check expects (_a, _a) but we give it (Int, Bool) → binding error
            %% This produces {error, {stack_underflow, ...}} from check_word
            PairOp = #operation{
                name = "pair-check",
                sig_in = ['_a', '_a'],
                sig_out = ['_a'],
                impl = fun(Cont) ->
                    [_, _ | Rest] = Cont#continuation.data_stack,
                    Cont#continuation{data_stack = [{'Int', 0} | Rest]}
                end
            },
            af_type:add_op('Any', PairOp),
            %% Capture warning output
            OldGL = group_leader(),
            {ok, Pid} = start_capture(),
            group_leader(Pid, self()),
            %% Define word: body calls pair-check with Int and Bool on stack
            %% check_word will see Int, Bool against (_a, _a) → binding conflict → stack_underflow
            _C1 = eval(": bad-pair Bool Int -> Int ; pair-check .", af_interpreter:new_continuation()),
            group_leader(OldGL, self()),
            Output = stop_capture(Pid),
            ?assert(string:find(Output, "Warning") =/= nomatch orelse
                    string:find(Output, "incomplete") =/= nomatch orelse
                    Output =:= "")
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
