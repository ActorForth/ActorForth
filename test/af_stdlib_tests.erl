-module(af_stdlib_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_string:init(),
    af_type_map:init(),
    af_type_list:init(),
    af_type_tuple:init(),
    af_type_float:init(),
    %% Load stdlib
    StdlibPath = "priv/stdlib.a4",
    {ok, Content} = file:read_file(StdlibPath),
    Tokens = af_parser:parse(binary_to_list(Content), StdlibPath),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()).

%% --- mod ---

mod_test_() ->
    {foreach, fun() -> af_type:reset(), af_type_any:init(), af_type_int:init(),
                       af_type_bool:init(), af_type_compiler:init(), af_type_product:init(),
                       af_type_string:init() end,
     fun(_) -> ok end, [
        fun(_) -> {"mod computes remainder", fun() ->
            C = eval("10 3 mod", af_interpreter:new_continuation()),
            [{'Int', 1}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"mod with exact division", fun() ->
            C = eval("12 4 mod", af_interpreter:new_continuation()),
            [{'Int', 0}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"mod by zero raises error", fun() ->
            ?assertError(_, eval("10 0 mod", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- stdlib words ---

negate_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"negate positive", fun() ->
            C = eval("5 negate", C0),
            [{'Int', -5}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"negate negative", fun() ->
            C = eval("-3 negate", C0),
            [{'Int', 3}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"negate zero", fun() ->
            C = eval("0 negate", C0),
            [{'Int', 0}] = C#continuation.data_stack
        end} end
    ]}.

abs_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"abs of positive stays positive", fun() ->
            C = eval("5 abs", C0),
            [{'Int', 5}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"abs of negative becomes positive", fun() ->
            C = eval("-7 abs", C0),
            [{'Int', 7}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"abs of zero is zero", fun() ->
            C = eval("0 abs", C0),
            [{'Int', 0}] = C#continuation.data_stack
        end} end
    ]}.

square_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"square of 5 is 25", fun() ->
            C = eval("5 square", C0),
            [{'Int', 25}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"square of negative", fun() ->
            C = eval("-3 square", C0),
            [{'Int', 9}] = C#continuation.data_stack
        end} end
    ]}.

cube_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"cube of 3 is 27", fun() ->
            C = eval("3 cube", C0),
            [{'Int', 27}] = C#continuation.data_stack
        end} end
    ]}.

double_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"double of 7 is 14", fun() ->
            C = eval("7 double", C0),
            [{'Int', 14}] = C#continuation.data_stack
        end} end
    ]}.

max_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"max returns larger", fun() ->
            C = eval("3 7 max", C0),
            [{'Int', 7}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"max with equal values", fun() ->
            C = eval("5 5 max", C0),
            [{'Int', 5}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"max with negatives", fun() ->
            C = eval("-10 -3 max", C0),
            [{'Int', -3}] = C#continuation.data_stack
        end} end
    ]}.

min_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"min returns smaller", fun() ->
            C = eval("3 7 min", C0),
            [{'Int', 3}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"min with equal values", fun() ->
            C = eval("5 5 min", C0),
            [{'Int', 5}] = C#continuation.data_stack
        end} end
    ]}.

even_odd_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"even? on even", fun() ->
            C = eval("4 even?", C0),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"even? on odd", fun() ->
            C = eval("5 even?", C0),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"odd? on odd", fun() ->
            C = eval("7 odd?", C0),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"odd? on even", fun() ->
            C = eval("8 odd?", C0),
            [{'Bool', false}] = C#continuation.data_stack
        end} end
    ]}.

bool_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"and true true", fun() ->
            C = eval("True bool True bool and", C0),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"and true false", fun() ->
            C = eval("True bool False bool and", C0),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"and false true", fun() ->
            C = eval("False bool True bool and", C0),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"or false false", fun() ->
            C = eval("False bool False bool or", C0),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"or true false", fun() ->
            C = eval("True bool False bool or", C0),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"or false true", fun() ->
            C = eval("False bool True bool or", C0),
            [{'Bool', true}] = C#continuation.data_stack
        end} end
    ]}.

stack_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(C0) -> {"nip removes second item", fun() ->
            C = eval("1 2 nip", C0),
            [{'Int', 2}] = C#continuation.data_stack
        end} end,
        fun(C0) -> {"tuck copies TOS below second", fun() ->
            C = eval("1 2 tuck", C0),
            [{'Int', 2}, {'Int', 1}, {'Int', 2}] = C#continuation.data_stack
        end} end
    ]}.
