-module(af_word_compiler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("operation.hrl").
-include("af_type.hrl").

%% Helper to build a word definition tuple from components.
word_def(Name, SigIn, SigOut, BodyNames) ->
    Body = [#operation{name = N} || N <- BodyNames],
    {Name, SigIn, SigOut, Body}.

%% --- compile_words_to_module tests ---
%% Functions take/return tagged stack lists: [{Type, Val}, ...]

simple_double_test() ->
    Def = word_def("double", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_double} = af_word_compiler:compile_words_to_module(af_wc_test_double, [Def]),
    ?assertEqual([{'Int', 10}], af_wc_test_double:double([{'Int', 5}])),
    ?assertEqual([{'Int', 0}], af_wc_test_double:double([{'Int', 0}])),
    ?assertEqual([{'Int', -6}], af_wc_test_double:double([{'Int', -3}])).

simple_square_test() ->
    Def = word_def("square", ['Int'], ['Int'], ["dup", "*"]),
    {ok, af_wc_test_square} = af_word_compiler:compile_words_to_module(af_wc_test_square, [Def]),
    ?assertEqual([{'Int', 49}], af_wc_test_square:square([{'Int', 7}])),
    ?assertEqual([{'Int', 100}], af_wc_test_square:square([{'Int', 10}])),
    ?assertEqual([{'Int', 1}], af_wc_test_square:square([{'Int', -1}])).

identity_test() ->
    Def = word_def("id", ['Int'], ['Int'], []),
    {ok, af_wc_test_id} = af_word_compiler:compile_words_to_module(af_wc_test_id, [Def]),
    ?assertEqual([{'Int', 42}], af_wc_test_id:id([{'Int', 42}])).

two_arg_add_test() ->
    %% add on stack [X, Y | Rest] -> [Y+X | Rest]
    Def = word_def("add", ['Int', 'Int'], ['Int'], ["+"]),
    {ok, af_wc_test_add} = af_word_compiler:compile_words_to_module(af_wc_test_add, [Def]),
    ?assertEqual([{'Int', 15}], af_wc_test_add:add([{'Int', 7}, {'Int', 8}])),
    ?assertEqual([{'Int', 0}], af_wc_test_add:add([{'Int', 5}, {'Int', -5}])).

subtract_test() ->
    Def = word_def("sub", ['Int', 'Int'], ['Int'], ["-"]),
    {ok, af_wc_test_sub} = af_word_compiler:compile_words_to_module(af_wc_test_sub, [Def]),
    ?assertEqual([{'Int', 3}], af_wc_test_sub:sub([{'Int', 2}, {'Int', 5}])),
    ?assertEqual([{'Int', -1}], af_wc_test_sub:sub([{'Int', 3}, {'Int', 2}])).

multiply_test() ->
    Def = word_def("mul", ['Int', 'Int'], ['Int'], ["*"]),
    {ok, af_wc_test_mul} = af_word_compiler:compile_words_to_module(af_wc_test_mul, [Def]),
    ?assertEqual([{'Int', 35}], af_wc_test_mul:mul([{'Int', 5}, {'Int', 7}])).

divide_test() ->
    Def = word_def("divv", ['Int', 'Int'], ['Int'], ["/"]),
    {ok, af_wc_test_div} = af_word_compiler:compile_words_to_module(af_wc_test_div, [Def]),
    ?assertEqual([{'Int', 5}], af_wc_test_div:divv([{'Int', 2}, {'Int', 10}])).

stack_swap_test() ->
    Def = word_def("swap_sub", ['Int', 'Int'], ['Int'], ["swap", "-"]),
    {ok, af_wc_test_swap} = af_word_compiler:compile_words_to_module(af_wc_test_swap, [Def]),
    ?assertEqual([{'Int', -3}], af_wc_test_swap:swap_sub([{'Int', 2}, {'Int', 5}])),
    ?assertEqual([{'Int', 1}], af_wc_test_swap:swap_sub([{'Int', 3}, {'Int', 2}])).

stack_over_test() ->
    %% over on [X, Y] -> [Y, X, Y], then + on [Y, X, Y] -> [X+Y, Y]
    Def = word_def("over_add", ['Int', 'Int'], ['Int', 'Int'], ["over", "+"]),
    {ok, af_wc_test_over} = af_word_compiler:compile_words_to_module(af_wc_test_over, [Def]),
    ?assertEqual([{'Int', 12}, {'Int', 5}], af_wc_test_over:over_add([{'Int', 7}, {'Int', 5}])).

drop_test() ->
    %% second(X, Y) -> Y  (body: drop)
    Def = word_def("second", ['Int', 'Int'], ['Int'], ["drop"]),
    {ok, af_wc_test_drop} = af_word_compiler:compile_words_to_module(af_wc_test_drop, [Def]),
    ?assertEqual([{'Int', 99}], af_wc_test_drop:second([{'Int', 1}, {'Int', 99}])).

comparison_test() ->
    Def = word_def("gt", ['Int', 'Int'], ['Bool'], [">"]),
    {ok, af_wc_test_cmp} = af_word_compiler:compile_words_to_module(af_wc_test_cmp, [Def]),
    ?assertEqual([{'Bool', true}], af_wc_test_cmp:gt([{'Int', 3}, {'Int', 10}])),
    ?assertEqual([{'Bool', false}], af_wc_test_cmp:gt([{'Int', 10}, {'Int', 3}])).

not_test() ->
    Def = word_def("invert", ['Bool'], ['Bool'], ["not"]),
    {ok, af_wc_test_not} = af_word_compiler:compile_words_to_module(af_wc_test_not, [Def]),
    ?assertEqual([{'Bool', false}], af_wc_test_not:invert([{'Bool', true}])),
    ?assertEqual([{'Bool', true}], af_wc_test_not:invert([{'Bool', false}])).

literal_in_body_test() ->
    %% inc(X) -> X + 1  (body: 1 +)
    Def = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    {ok, af_wc_test_lit} = af_word_compiler:compile_words_to_module(af_wc_test_lit, [Def]),
    ?assertEqual([{'Int', 6}], af_wc_test_lit:inc([{'Int', 5}])),
    ?assertEqual([{'Int', 0}], af_wc_test_lit:inc([{'Int', -1}])).

complex_body_test() ->
    %% sum_squares(X, Y) -> X*X + Y*Y  (body: swap dup * swap dup * +)
    Def = word_def("sum_sq", ['Int', 'Int'], ['Int'],
                   ["swap", "dup", "*", "swap", "dup", "*", "+"]),
    {ok, af_wc_test_ssq} = af_word_compiler:compile_words_to_module(af_wc_test_ssq, [Def]),
    ?assertEqual([{'Int', 25}], af_wc_test_ssq:sum_sq([{'Int', 3}, {'Int', 4}])),
    ?assertEqual([{'Int', 2}], af_wc_test_ssq:sum_sq([{'Int', 1}, {'Int', 1}])).

multiple_functions_test() ->
    Def1 = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    Def2 = word_def("dec", ['Int'], ['Int'], ["1", "-"]),
    {ok, af_wc_test_multi} = af_word_compiler:compile_words_to_module(af_wc_test_multi, [Def1, Def2]),
    ?assertEqual([{'Int', 6}], af_wc_test_multi:inc([{'Int', 5}])),
    ?assertEqual([{'Int', 4}], af_wc_test_multi:dec([{'Int', 5}])).

no_compilable_words_test() ->
    ?assertEqual({error, no_compilable_words},
                 af_word_compiler:compile_words_to_module(af_wc_test_empty, [])).

two_dup_test() ->
    %% 2dup on [X, Y] -> [X, Y, X, Y]
    Def = word_def("both", ['Int', 'Int'], ['Int', 'Int', 'Int', 'Int'], ["2dup"]),
    {ok, af_wc_test_2dup} = af_word_compiler:compile_words_to_module(af_wc_test_2dup, [Def]),
    ?assertEqual([{'Int', 3}, {'Int', 7}, {'Int', 3}, {'Int', 7}],
                 af_wc_test_2dup:both([{'Int', 3}, {'Int', 7}])).

rot_test() ->
    %% rot on [A, B, C] -> [C, A, B]
    Def = word_def("myrot", ['Int', 'Int', 'Int'], ['Int', 'Int', 'Int'], ["rot"]),
    {ok, af_wc_test_rot} = af_word_compiler:compile_words_to_module(af_wc_test_rot, [Def]),
    ?assertEqual([{'Int', 3}, {'Int', 1}, {'Int', 2}],
                 af_wc_test_rot:myrot([{'Int', 1}, {'Int', 2}, {'Int', 3}])).

%% --- Inter-word call tests ---
%% These use apply_impl for ops after a word call, so the type system must be initialized.

setup_types() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init().

inter_word_call_test() ->
    setup_types(),
    Def1 = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    Def2 = word_def("double_inc", ['Int'], ['Int'], ["inc", "inc"]),
    {ok, af_wc_test_inter} = af_word_compiler:compile_words_to_module(af_wc_test_inter, [Def1, Def2]),
    ?assertEqual([{'Int', 6}], af_wc_test_inter:inc([{'Int', 5}])),
    ?assertEqual([{'Int', 7}], af_wc_test_inter:double_inc([{'Int', 5}])),
    ?assertEqual([{'Int', 2}], af_wc_test_inter:double_inc([{'Int', 0}])).

inter_word_chain_test() ->
    setup_types(),
    Def1 = word_def("double", ['Int'], ['Int'], ["dup", "+"]),
    Def2 = word_def("triple", ['Int'], ['Int'], ["dup", "double", "+"]),
    {ok, af_wc_test_chain} = af_word_compiler:compile_words_to_module(af_wc_test_chain, [Def1, Def2]),
    ?assertEqual([{'Int', 10}], af_wc_test_chain:double([{'Int', 5}])),
    ?assertEqual([{'Int', 15}], af_wc_test_chain:triple([{'Int', 5}])),
    ?assertEqual([{'Int', 0}], af_wc_test_chain:triple([{'Int', 0}])).

inter_word_complex_test() ->
    setup_types(),
    Def1 = word_def("square", ['Int'], ['Int'], ["dup", "*"]),
    Def2 = word_def("sum_sq", ['Int', 'Int'], ['Int'], ["swap", "square", "swap", "square", "+"]),
    {ok, af_wc_test_complex} = af_word_compiler:compile_words_to_module(af_wc_test_complex, [Def1, Def2]),
    ?assertEqual([{'Int', 9}], af_wc_test_complex:square([{'Int', 3}])),
    ?assertEqual([{'Int', 25}], af_wc_test_complex:sum_sq([{'Int', 3}, {'Int', 4}])),
    ?assertEqual([{'Int', 2}], af_wc_test_complex:sum_sq([{'Int', 1}, {'Int', 1}])).
