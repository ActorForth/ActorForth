-module(af_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

%% Helper: parse and interpret a string, return the data stack
eval(Input) ->
    Tokens = af_parser:parse(Input, "test"),
    Result = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    Result#continuation.data_stack.

%% Helper: parse and interpret, return continuation (for multi-line sessions)
eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init().

%% --- Integer operations ---

int_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"int constructor converts Atom to Int", fun() ->
            ?assertEqual([{'Int', 42}], eval("42 int"))
        end} end,
        fun(_) -> {"int addition", fun() ->
            ?assertEqual([{'Int', 8}], eval("3 int 5 int +"))
        end} end,
        fun(_) -> {"int subtraction", fun() ->
            ?assertEqual([{'Int', 2}], eval("5 int 3 int -"))
        end} end,
        fun(_) -> {"int multiplication", fun() ->
            ?assertEqual([{'Int', 200}], eval("10 int 20 int *"))
        end} end,
        fun(_) -> {"int division", fun() ->
            ?assertEqual([{'Int', 5}], eval("15 int 3 int /"))
        end} end,
        fun(_) -> {"chained arithmetic: 4 int 13 int + gives 17", fun() ->
            ?assertEqual([{'Int', 17}], eval("4 int 13 int +"))
        end} end,
        fun(_) -> {"complex expression: (10 + 20) * 2 = 60", fun() ->
            ?assertEqual([{'Int', 60}], eval("10 int 20 int + 2 int *"))
        end} end,
        fun(_) -> {"negative result", fun() ->
            ?assertEqual([{'Int', -5}], eval("3 int 8 int -"))
        end} end
    ]}.

%% --- Boolean operations ---

bool_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"bool constructor", fun() ->
            ?assertEqual([{'Bool', true}], eval("True bool"))
        end} end,
        fun(_) -> {"bool not", fun() ->
            ?assertEqual([{'Bool', false}], eval("True bool not"))
        end} end,
        fun(_) -> {"int equality", fun() ->
            ?assertEqual([{'Bool', true}], eval("5 int 5 int =="))
        end} end,
        fun(_) -> {"int inequality", fun() ->
            ?assertEqual([{'Bool', true}], eval("5 int 3 int !="))
        end} end,
        fun(_) -> {"int less than", fun() ->
            ?assertEqual([{'Bool', true}], eval("3 int 5 int <"))
        end} end,
        fun(_) -> {"int greater than", fun() ->
            ?assertEqual([{'Bool', true}], eval("5 int 3 int >"))
        end} end,
        fun(_) -> {"int less than or equal", fun() ->
            ?assertEqual([{'Bool', true}], eval("3 int 3 int <="))
        end} end,
        fun(_) -> {"int greater than or equal", fun() ->
            ?assertEqual([{'Bool', true}], eval("5 int 3 int >="))
        end} end
    ]}.

%% --- Stack operations ---

stack_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"dup duplicates TOS", fun() ->
            ?assertEqual([{'Int', 5}, {'Int', 5}], eval("5 int dup"))
        end} end,
        fun(_) -> {"drop removes TOS", fun() ->
            ?assertEqual([{'Int', 3}], eval("3 int 5 int drop"))
        end} end,
        fun(_) -> {"swap swaps top two", fun() ->
            %% Stack before swap: [{Int,3}, {Int,5}]. After: [{Int,5}, {Int,3}]
            ?assertEqual([{'Int', 5}, {'Int', 3}], eval("5 int 3 int swap"))
        end} end,
        fun(_) -> {"2dup duplicates top two", fun() ->
            ?assertEqual([{'Int', 5}, {'Int', 3}, {'Int', 5}, {'Int', 3}],
                         eval("3 int 5 int 2dup"))
        end} end
    ]}.

%% --- Atom fallback ---

atom_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"unknown words become Atoms", fun() ->
            ?assertEqual([{'Atom', "hello"}], eval("hello"))
        end} end,
        fun(_) -> {"mixed atoms and typed values", fun() ->
            ?assertEqual([{'Int', 42}, {'Atom', "hello"}],
                         eval("hello 42 int"))
        end} end
    ]}.

%% --- I/O operations ---

io_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"print removes TOS and produces output", fun() ->
            C = eval("42 int print", af_interpreter:new_continuation()),
            ?assertEqual([], C#continuation.data_stack)
        end} end,
        fun(_) -> {"stack on empty stack doesn't crash", fun() ->
            C = eval("stack", af_interpreter:new_continuation()),
            ?assertEqual([], C#continuation.data_stack)
        end} end,
        fun(_) -> {"stack on non-empty stack preserves stack", fun() ->
            C = eval("1 int 2 int stack", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 2}, {'Int', 1}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"words doesn't modify stack", fun() ->
            C = eval("5 int words", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"types doesn't modify stack", fun() ->
            C = eval("5 int types", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end
    ]}.

%% --- Bool edge cases ---

bool_edge_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"lowercase true", fun() ->
            ?assertEqual([{'Bool', true}], eval("true bool"))
        end} end,
        fun(_) -> {"lowercase false", fun() ->
            ?assertEqual([{'Bool', false}], eval("false bool"))
        end} end,
        fun(_) -> {"uppercase False", fun() ->
            ?assertEqual([{'Bool', false}], eval("False bool"))
        end} end,
        fun(_) -> {"invalid bool raises error", fun() ->
            ?assertError({type_error, {cannot_convert_to_bool, "garbage"}},
                eval("garbage bool"))
        end} end
    ]}.

%% --- Multi-line session (continuation threading) ---

session_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"continuation persists across lines", fun() ->
            C1 = eval("10 int", af_interpreter:new_continuation()),
            C2 = eval("20 int", C1),
            C3 = eval("+", C2),
            ?assertEqual([{'Int', 30}], C3#continuation.data_stack)
        end} end
    ]}.

%% --- see word ---

setup_with_compiler() ->
    setup(),
    af_type_compiler:init().

see_test_() ->
    {foreach, fun setup_with_compiler/0, fun(_) -> ok end, [
        fun(_) -> {"see shows built-in word", fun() ->
            C1 = eval("dup see", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end,

        fun(_) -> {"see shows compiled word body", fun() ->
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval("double see", C1),
            ?assertEqual([], C2#continuation.data_stack)
        end} end,

        fun(_) -> {"see unknown word says not found", fun() ->
            C1 = eval("nonexistent see", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack)
        end} end
    ]}.
