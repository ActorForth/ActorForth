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
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init().

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
            %% Register the base case with value constraint (manually, since
            %% compiler doesn't yet support value constraints in signatures)
            BaseOp = #operation{
                name = "fact",
                sig_in = [{'Int', 0}],
                sig_out = ['Int'],
                impl = fun(Cont) ->
                    [_ | Rest] = Cont#continuation.data_stack,
                    Cont#continuation{data_stack = [{'Int', 1} | Rest]}
                end
            },
            %% Insert base case BEFORE the recursive case so it's tried first
            {ok, #af_type{ops = Ops} = Type} = af_type:get_type('Int'),
            ExistingFacts = maps:get("fact", Ops, []),
            NewOps = maps:put("fact", [BaseOp | ExistingFacts], Ops),
            af_type:register_type(Type#af_type{ops = NewOps}),
            Result = eval("5 int fact", C1),
            ?assertEqual([{'Int', 120}], Result#continuation.data_stack)
        end} end
    ]}.
