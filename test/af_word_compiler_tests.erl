-module(af_word_compiler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("operation.hrl").
-include("af_type.hrl").

%% Helper to build a word definition tuple from components.
word_def(Name, SigIn, SigOut, BodyNames) ->
    Body = [#operation{name = N} || N <- BodyNames],
    {Name, SigIn, SigOut, Body}.

%% --- compile_words_to_module tests ---

simple_double_test() ->
    %% double(X) -> X + X  (body: dup +)
    Def = word_def("double", ['Int'], ['Int'], ["dup", "+"]),
    {ok, af_wc_test_double} = af_word_compiler:compile_words_to_module(af_wc_test_double, [Def]),
    ?assertEqual(10, af_wc_test_double:double(5)),
    ?assertEqual(0, af_wc_test_double:double(0)),
    ?assertEqual(-6, af_wc_test_double:double(-3)).

simple_square_test() ->
    %% square(X) -> X * X  (body: dup *)
    Def = word_def("square", ['Int'], ['Int'], ["dup", "*"]),
    {ok, af_wc_test_square} = af_word_compiler:compile_words_to_module(af_wc_test_square, [Def]),
    ?assertEqual(49, af_wc_test_square:square(7)),
    ?assertEqual(100, af_wc_test_square:square(10)),
    ?assertEqual(1, af_wc_test_square:square(-1)).

identity_test() ->
    %% id(X) -> X  (empty body)
    Def = word_def("id", ['Int'], ['Int'], []),
    {ok, af_wc_test_id} = af_word_compiler:compile_words_to_module(af_wc_test_id, [Def]),
    ?assertEqual(42, af_wc_test_id:id(42)).

two_arg_add_test() ->
    %% add(X, Y) -> X + Y  (body: +)
    Def = word_def("add", ['Int', 'Int'], ['Int'], ["+"]),
    {ok, af_wc_test_add} = af_word_compiler:compile_words_to_module(af_wc_test_add, [Def]),
    ?assertEqual(15, af_wc_test_add:add(7, 8)),
    ?assertEqual(0, af_wc_test_add:add(5, -5)).

subtract_test() ->
    %% sub(X, Y) -> Y - X  (body: -)
    %% In stack terms: TOS=X, next=Y, so - gives Y-X
    Def = word_def("sub", ['Int', 'Int'], ['Int'], ["-"]),
    {ok, af_wc_test_sub} = af_word_compiler:compile_words_to_module(af_wc_test_sub, [Def]),
    ?assertEqual(3, af_wc_test_sub:sub(2, 5)),
    ?assertEqual(-1, af_wc_test_sub:sub(3, 2)).

multiply_test() ->
    Def = word_def("mul", ['Int', 'Int'], ['Int'], ["*"]),
    {ok, af_wc_test_mul} = af_word_compiler:compile_words_to_module(af_wc_test_mul, [Def]),
    ?assertEqual(35, af_wc_test_mul:mul(5, 7)).

divide_test() ->
    Def = word_def("divv", ['Int', 'Int'], ['Int'], ["/"]),
    {ok, af_wc_test_div} = af_word_compiler:compile_words_to_module(af_wc_test_div, [Def]),
    ?assertEqual(5, af_wc_test_div:divv(2, 10)).

stack_swap_test() ->
    %% swap_sub(X, Y) -> X - Y  (body: swap -)
    Def = word_def("swap_sub", ['Int', 'Int'], ['Int'], ["swap", "-"]),
    {ok, af_wc_test_swap} = af_word_compiler:compile_words_to_module(af_wc_test_swap, [Def]),
    ?assertEqual(-3, af_wc_test_swap:swap_sub(2, 5)),
    ?assertEqual(1, af_wc_test_swap:swap_sub(3, 2)).

stack_over_test() ->
    %% over_add(X, Y) -> (Y + X, Y)  -- body: over +, result stack has 2 items
    %% Actually: over on [X, Y] -> [Y, X, Y], then + on [Y, X, Y] -> [X+Y, Y]
    %% Returns tuple {X+Y, Y}
    Def = word_def("over_add", ['Int', 'Int'], ['Int', 'Int'], ["over", "+"]),
    {ok, af_wc_test_over} = af_word_compiler:compile_words_to_module(af_wc_test_over, [Def]),
    ?assertEqual({12, 5}, af_wc_test_over:over_add(7, 5)).

drop_test() ->
    %% second(X, Y) -> Y  (body: drop)
    Def = word_def("second", ['Int', 'Int'], ['Int'], ["drop"]),
    {ok, af_wc_test_drop} = af_word_compiler:compile_words_to_module(af_wc_test_drop, [Def]),
    ?assertEqual(99, af_wc_test_drop:second(1, 99)).

comparison_test() ->
    Def = word_def("gt", ['Int', 'Int'], ['Bool'], [">"]),
    {ok, af_wc_test_cmp} = af_word_compiler:compile_words_to_module(af_wc_test_cmp, [Def]),
    ?assertEqual(true, af_wc_test_cmp:gt(3, 10)),
    ?assertEqual(false, af_wc_test_cmp:gt(10, 3)).

not_test() ->
    %% invert(X) -> not X  (body: not)
    Def = word_def("invert", ['Bool'], ['Bool'], ["not"]),
    {ok, af_wc_test_not} = af_word_compiler:compile_words_to_module(af_wc_test_not, [Def]),
    ?assertEqual(false, af_wc_test_not:invert(true)),
    ?assertEqual(true, af_wc_test_not:invert(false)).

literal_in_body_test() ->
    %% inc(X) -> X + 1  (body: 1 +)
    %% "1" is parsed as integer literal in translate_op
    Def = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    {ok, af_wc_test_lit} = af_word_compiler:compile_words_to_module(af_wc_test_lit, [Def]),
    ?assertEqual(6, af_wc_test_lit:inc(5)),
    ?assertEqual(0, af_wc_test_lit:inc(-1)).

complex_body_test() ->
    %% sum_squares(X, Y) -> X*X + Y*Y  (body: swap dup * swap dup * +)
    Def = word_def("sum_sq", ['Int', 'Int'], ['Int'],
                   ["swap", "dup", "*", "swap", "dup", "*", "+"]),
    {ok, af_wc_test_ssq} = af_word_compiler:compile_words_to_module(af_wc_test_ssq, [Def]),
    ?assertEqual(25, af_wc_test_ssq:sum_sq(3, 4)),
    ?assertEqual(2, af_wc_test_ssq:sum_sq(1, 1)).

multiple_functions_test() ->
    %% Compile two functions into the same module
    Def1 = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    Def2 = word_def("dec", ['Int'], ['Int'], ["1", "-"]),
    {ok, af_wc_test_multi} = af_word_compiler:compile_words_to_module(af_wc_test_multi, [Def1, Def2]),
    ?assertEqual(6, af_wc_test_multi:inc(5)),
    ?assertEqual(4, af_wc_test_multi:dec(5)).

no_compilable_words_test() ->
    ?assertEqual({error, no_compilable_words},
                 af_word_compiler:compile_words_to_module(af_wc_test_empty, [])).

two_dup_test() ->
    %% 2dup on [X, Y] -> [X, Y, X, Y]
    Def = word_def("both", ['Int', 'Int'], ['Int', 'Int', 'Int', 'Int'], ["2dup"]),
    {ok, af_wc_test_2dup} = af_word_compiler:compile_words_to_module(af_wc_test_2dup, [Def]),
    ?assertEqual({3, 7, 3, 7}, af_wc_test_2dup:both(3, 7)).

rot_test() ->
    %% rot on [A, B, C] -> [C, A, B]
    Def = word_def("myrot", ['Int', 'Int', 'Int'], ['Int', 'Int', 'Int'], ["rot"]),
    {ok, af_wc_test_rot} = af_word_compiler:compile_words_to_module(af_wc_test_rot, [Def]),
    ?assertEqual({3, 1, 2}, af_wc_test_rot:myrot(1, 2, 3)).

%% --- Inter-word call tests ---

inter_word_call_test() ->
    %% double calls inc: double(X) = inc(inc(X)) where inc(X) = X + 1
    %% Compile both into same module — double should call inc directly
    Def1 = word_def("inc", ['Int'], ['Int'], ["1", "+"]),
    Def2 = word_def("double_inc", ['Int'], ['Int'], ["inc", "inc"]),
    {ok, af_wc_test_inter} = af_word_compiler:compile_words_to_module(af_wc_test_inter, [Def1, Def2]),
    ?assertEqual(6, af_wc_test_inter:inc(5)),
    ?assertEqual(7, af_wc_test_inter:double_inc(5)),
    ?assertEqual(2, af_wc_test_inter:double_inc(0)).

inter_word_chain_test() ->
    %% triple(X) = X + X + X = double(X) + X
    %% body: dup double +  (dup X, call double on TOS, add)
    Def1 = word_def("double", ['Int'], ['Int'], ["dup", "+"]),
    Def2 = word_def("triple", ['Int'], ['Int'], ["dup", "double", "+"]),
    {ok, af_wc_test_chain} = af_word_compiler:compile_words_to_module(af_wc_test_chain, [Def1, Def2]),
    ?assertEqual(10, af_wc_test_chain:double(5)),
    ?assertEqual(15, af_wc_test_chain:triple(5)),
    ?assertEqual(0, af_wc_test_chain:triple(0)).

inter_word_complex_test() ->
    %% square(X) = X * X, then sum_sq(X, Y) = square(X) + square(Y)
    Def1 = word_def("square", ['Int'], ['Int'], ["dup", "*"]),
    Def2 = word_def("sum_sq", ['Int', 'Int'], ['Int'], ["swap", "square", "swap", "square", "+"]),
    {ok, af_wc_test_complex} = af_word_compiler:compile_words_to_module(af_wc_test_complex, [Def1, Def2]),
    ?assertEqual(9, af_wc_test_complex:square(3)),
    ?assertEqual(25, af_wc_test_complex:sum_sq(3, 4)),
    ?assertEqual(2, af_wc_test_complex:sum_sq(1, 1)).
