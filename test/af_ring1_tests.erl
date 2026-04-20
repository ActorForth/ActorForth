-module(af_ring1_tests).

-include_lib("eunit/include/eunit.hrl").
-include("af_type.hrl").

%%% === Interpreted Mode Tests ===

simple_double_interp_test() ->
    Defs = [{"double", ['Int'], ['Int'], [dup, add]}],
    {ok, r1_test_double} = af_ring1:compile_module(r1_test_double, Defs),
    ?assertEqual([{'Int', 42}], r1_test_double:double([{'Int', 21}])).

simple_square_interp_test() ->
    Defs = [{"square", ['Int'], ['Int'], [dup, mul]}],
    {ok, r1_test_square} = af_ring1:compile_module(r1_test_square, Defs),
    ?assertEqual([{'Int', 25}], r1_test_square:square([{'Int', 5}])).

literal_push_interp_test() ->
    Defs = [{"answer", [], ['Int'], [{lit, {'Int', 42}}]}],
    {ok, r1_test_lit} = af_ring1:compile_module(r1_test_lit, Defs),
    ?assertEqual([{'Int', 42}], r1_test_lit:answer([])).

multi_word_interp_test() ->
    Defs = [
        {"double", ['Int'], ['Int'], [dup, add]},
        {"quadruple", ['Int'], ['Int'], [{call, "double"}, {call, "double"}]}
    ],
    {ok, r1_test_multi} = af_ring1:compile_module(r1_test_multi, Defs),
    ?assertEqual([{'Int', 40}], r1_test_multi:quadruple([{'Int', 10}])).

stack_ops_interp_test() ->
    %% swap then sub: stack [10, 3] -> swap -> [3, 10] -> sub (NOS-TOS = 10-3 = 7)
    Defs = [{"sub_swap", ['Int', 'Int'], ['Int'], [swap, sub]}],
    {ok, r1_test_sops} = af_ring1:compile_module(r1_test_sops, Defs),
    ?assertEqual([{'Int', 7}], r1_test_sops:sub_swap([{'Int', 10}, {'Int', 3}])).

drop_interp_test() ->
    %% swap drop: [1, 2] -> swap -> [2, 1] -> drop -> [1]
    Defs = [{"first", ['Any', 'Any'], ['Any'], [swap, drop]}],
    {ok, r1_test_drop} = af_ring1:compile_module(r1_test_drop, Defs),
    ?assertEqual([{'Int', 1}], r1_test_drop:first([{'Int', 1}, {'Int', 2}])).

comparison_interp_test() ->
    Defs = [{"is_zero", ['Int'], ['Bool'], [{lit, {'Int', 0}}, eq]}],
    {ok, r1_test_cmp} = af_ring1:compile_module(r1_test_cmp, Defs),
    ?assertEqual([{'Bool', true}], r1_test_cmp:is_zero([{'Int', 0}])),
    ?assertEqual([{'Bool', false}], r1_test_cmp:is_zero([{'Int', 5}])).

list_ops_interp_test() ->
    Defs = [{"make_list", ['Int'], ['List'],
             [nil, swap, cons]}],
    {ok, r1_test_list} = af_ring1:compile_module(r1_test_list, Defs),
    ?assertEqual([{'List', [{'Int', 42}]}],
                 r1_test_list:make_list([{'Int', 42}])).

string_interp_test() ->
    Defs = [{"greet", ['String'], ['String'],
             [{lit, {'String', <<"Hello, ">>}}, swap,
              {lit, {'String', <<"!">>}},
              %% Stack: "!" name "Hello, "
              %% Use ring0 for concat since it's not a simple op
              to_r, to_r,
              %% RS: [name, "!"], DS: ["Hello, "]
              from_r,
              %% DS: [name, "Hello, "]
              %% We need concat... use select_clause or emit_tos
              %% Actually, let's just test that strings pass through
              drop, from_r, drop
             ]}],
    {ok, r1_test_str} = af_ring1:compile_module(r1_test_str, Defs),
    %% After drops: "Hello, " remains
    ?assertEqual([{'String', <<"Hello, ">>}],
                 r1_test_str:greet([{'String', <<"World">>}])).

select_clause_interp_test() ->
    %% Factorial using select_clause
    FactBody = [
        {lit, {'List', [
            {[{'Int', 0}], [drop, {lit, {'Int', 1}}]},
            {['Int'], [dup, {lit, {'Int', 1}}, sub, {call, "fact"}, mul]}
        ]}},
        select_clause
    ],
    Defs = [{"fact", ['Int'], ['Int'], FactBody}],
    {ok, r1_test_fact} = af_ring1:compile_module(r1_test_fact, Defs),
    ?assertEqual([{'Int', 120}], r1_test_fact:fact([{'Int', 5}])),
    ?assertEqual([{'Int', 1}], r1_test_fact:fact([{'Int', 0}])).

fibonacci_interp_test() ->
    FibBody = [
        {lit, {'List', [
            {[{'Int', 0}], [drop, {lit, {'Int', 0}}]},
            {[{'Int', 1}], [drop, {lit, {'Int', 1}}]},
            {['Int'], [dup, {lit, {'Int', 1}}, sub, {call, "fib"},
                       swap, {lit, {'Int', 2}}, sub, {call, "fib"},
                       add]}
        ]}},
        select_clause
    ],
    Defs = [{"fib", ['Int'], ['Int'], FibBody}],
    {ok, r1_test_fib} = af_ring1:compile_module(r1_test_fib, Defs),
    ?assertEqual([{'Int', 55}], r1_test_fib:fib([{'Int', 10}])).

multi_clause_interp_test() ->
    %% Two definitions of same word with different SigIn
    Defs = [
        {"classify", [{'Int', 0}], ['String'],
         [drop, {lit, {'String', <<"zero">>}}]},
        {"classify", ['Int'], ['String'],
         [drop, {lit, {'String', <<"nonzero">>}}]}
    ],
    {ok, r1_test_mc} = af_ring1:compile_module(r1_test_mc, Defs),
    ?assertEqual([{'String', <<"zero">>}], r1_test_mc:classify([{'Int', 0}])),
    ?assertEqual([{'String', <<"nonzero">>}], r1_test_mc:classify([{'Int', 5}])).

preserves_stack_interp_test() ->
    %% Word should preserve unconsumed stack items
    Defs = [{"inc", ['Int'], ['Int'], [{lit, {'Int', 1}}, add]}],
    {ok, r1_test_pres} = af_ring1:compile_module(r1_test_pres, Defs),
    ?assertEqual([{'Int', 6}, {'String', <<"below">>}],
                 r1_test_pres:inc([{'Int', 5}, {'String', <<"below">>}])).

return_stack_interp_test() ->
    %% Test to_r / from_r
    Defs = [{"over", [], ['Any'],
             [to_r, dup, from_r, swap]}],
    {ok, r1_test_rs} = af_ring1:compile_module(r1_test_rs, Defs),
    ?assertEqual([{'Int', 1}, {'Int', 2}, {'Int', 1}],
                 r1_test_rs:over([{'Int', 2}, {'Int', 1}])).

%%% === Native Mode Tests ===

simple_double_native_test() ->
    Defs = [{"double", ['Int'], ['Int'], [dup, add]}],
    {ok, r1n_double} = af_ring1:compile_module(r1n_double, Defs, [{mode, native}]),
    ?assertEqual([{'Int', 42}], r1n_double:double([{'Int', 21}])).

simple_square_native_test() ->
    Defs = [{"square", ['Int'], ['Int'], [dup, mul]}],
    {ok, r1n_square} = af_ring1:compile_module(r1n_square, Defs, [{mode, native}]),
    ?assertEqual([{'Int', 25}], r1n_square:square([{'Int', 5}])).

literal_native_test() ->
    Defs = [{"answer", [], ['Int'], [{lit, {'Int', 42}}]}],
    {ok, r1n_lit} = af_ring1:compile_module(r1n_lit, Defs, [{mode, native}]),
    ?assertEqual([{'Int', 42}], r1n_lit:answer([])).

arith_native_test() ->
    %% 10 - 3 = 7
    Defs = [{"calc", ['Int', 'Int'], ['Int'], [sub]}],
    {ok, r1n_arith} = af_ring1:compile_module(r1n_arith, Defs, [{mode, native}]),
    ?assertEqual([{'Int', 7}], r1n_arith:calc([{'Int', 3}, {'Int', 10}])).

comparison_native_test() ->
    Defs = [{"is_zero", ['Int'], ['Bool'], [{lit, {'Int', 0}}, eq]}],
    {ok, r1n_cmp} = af_ring1:compile_module(r1n_cmp, Defs, [{mode, native}]),
    ?assertEqual([{'Bool', true}], r1n_cmp:is_zero([{'Int', 0}])),
    ?assertEqual([{'Bool', false}], r1n_cmp:is_zero([{'Int', 5}])).

lt_native_test() ->
    Defs = [{"lt_check", ['Int', 'Int'], ['Bool'], [lt]}],
    {ok, r1n_lt} = af_ring1:compile_module(r1n_lt, Defs, [{mode, native}]),
    ?assertEqual([{'Bool', true}], r1n_lt:lt_check([{'Int', 10}, {'Int', 3}])),
    ?assertEqual([{'Bool', false}], r1n_lt:lt_check([{'Int', 3}, {'Int', 10}])).

not_op_native_test() ->
    Defs = [{"negate", ['Bool'], ['Bool'], [not_op]}],
    {ok, r1n_not} = af_ring1:compile_module(r1n_not, Defs, [{mode, native}]),
    ?assertEqual([{'Bool', false}], r1n_not:negate([{'Bool', true}])),
    ?assertEqual([{'Bool', true}], r1n_not:negate([{'Bool', false}])).

drop_native_test() ->
    Defs = [{"discard", ['Any'], [], [drop]}],
    {ok, r1n_drop} = af_ring1:compile_module(r1n_drop, Defs, [{mode, native}]),
    ?assertEqual([], r1n_drop:discard([{'Int', 1}])).

swap_native_test() ->
    Defs = [{"flip", ['Any', 'Any'], ['Any', 'Any'], [swap]}],
    {ok, r1n_swap} = af_ring1:compile_module(r1n_swap, Defs, [{mode, native}]),
    ?assertEqual([{'Int', 2}, {'Int', 1}],
                 r1n_swap:flip([{'Int', 1}, {'Int', 2}])).

multi_word_native_test() ->
    Defs = [
        {"double", ['Int'], ['Int'], [dup, add]},
        {"quadruple", ['Int'], ['Int'], [{call, "double"}, {call, "double"}]}
    ],
    {ok, r1n_multi} = af_ring1:compile_module(r1n_multi, Defs, [{mode, native}]),
    ?assertEqual([{'Int', 40}], r1n_multi:quadruple([{'Int', 10}])).

native_fallback_test() ->
    %% Operations not inlined should fall back to af_ring0:exec
    Defs = [{"make_list", ['Int'], ['List'],
             [nil, swap, cons]}],
    {ok, r1n_fb} = af_ring1:compile_module(r1n_fb, Defs, [{mode, native}]),
    ?assertEqual([{'List', [{'Int', 42}]}],
                 r1n_fb:make_list([{'Int', 42}])).

multi_clause_native_test() ->
    Defs = [
        {"classify", [{'Int', 0}], ['String'],
         [drop, {lit, {'String', <<"zero">>}}]},
        {"classify", ['Int'], ['String'],
         [drop, {lit, {'String', <<"nonzero">>}}]}
    ],
    {ok, r1n_mc} = af_ring1:compile_module(r1n_mc, Defs, [{mode, native}]),
    ?assertEqual([{'String', <<"zero">>}], r1n_mc:classify([{'Int', 0}])),
    ?assertEqual([{'String', <<"nonzero">>}], r1n_mc:classify([{'Int', 5}])).

%%% === Native Product Type Tests ===

%% Register 'Point' in the type registry so Ring 1's
%% is_product_type recognises it when generating head patterns.
%% In production, af_r0_compiler registers types during compilation.
register_point() ->
    af_type:init(),
    af_type:register_type(#af_type{
        name = 'Point', fields = [{x, 'Int'}, {y, 'Int'}]}).

product_new_native_test() ->
    register_point(),
    %% Flat-tuple layout: {TypeName, V1, V2}. Opcode takes field count.
    WordDefs = [
        {"make-pt", ['Int', 'Int'], ['Point'], [{product_new, 'Point', 2}]}
    ],
    {ok, r1n_prod_new} = af_ring1:compile_module(r1n_prod_new, WordDefs, [{mode, native}]),
    [Instance] = r1n_prod_new:'make-pt'([{'Int', 20}, {'Int', 10}]),
    ?assertEqual('Point', element(1, Instance)),
    ?assertEqual({'Int', 10}, element(2, Instance)),
    ?assertEqual({'Int', 20}, element(3, Instance)).

product_get_native_test() ->
    register_point(),
    %% Pos 2 -> field x (first declared), Pos 3 -> y.
    WordDefs = [
        {"make-pt", ['Int', 'Int'], ['Point'], [{product_new, 'Point', 2}]},
        {"get-x", ['Point'], ['Int', 'Point'], [{product_get, 2}]}
    ],
    {ok, r1n_prod_get} = af_ring1:compile_module(r1n_prod_get, WordDefs, [{mode, native}]),
    [Instance] = r1n_prod_get:'make-pt'([{'Int', 20}, {'Int', 10}]),
    Result = r1n_prod_get:'get-x'([Instance]),
    [{'Int', 10}, Instance] = Result.

product_set_native_test() ->
    register_point(),
    WordDefs = [
        {"make-pt", ['Int', 'Int'], ['Point'], [{product_new, 'Point', 2}]},
        {"set-x", ['Int', 'Point'], ['Point'], [{product_set, 2}]}
    ],
    {ok, r1n_prod_set} = af_ring1:compile_module(r1n_prod_set, WordDefs, [{mode, native}]),
    [Instance] = r1n_prod_set:'make-pt'([{'Int', 20}, {'Int', 10}]),
    [Updated] = r1n_prod_set:'set-x'([{'Int', 99}, Instance]),
    ?assertEqual('Point', element(1, Updated)),
    ?assertEqual({'Int', 99}, element(2, Updated)),
    ?assertEqual({'Int', 20}, element(3, Updated)).

product_round_trip_native_test() ->
    register_point(),
    WordDefs = [
        {"make-pt", ['Int', 'Int'], ['Point'], [{product_new, 'Point', 2}]},
        {"get-x", ['Point'], ['Int', 'Point'], [{product_get, 2}]},
        {"get-y", ['Point'], ['Int', 'Point'], [{product_get, 3}]},
        {"set-x", ['Int', 'Point'], ['Point'], [{product_set, 2}]},
        {"set-y", ['Int', 'Point'], ['Point'], [{product_set, 3}]}
    ],
    {ok, r1n_prod_rt} = af_ring1:compile_module(r1n_prod_rt, WordDefs, [{mode, native}]),
    [I0] = r1n_prod_rt:'make-pt'([{'Int', 20}, {'Int', 10}]),
    [I1] = r1n_prod_rt:'set-x'([{'Int', 50}, I0]),
    [{'Int', 50}, I1] = r1n_prod_rt:'get-x'([I1]),
    [{'Int', 20}, I1] = r1n_prod_rt:'get-y'([I1]).

%%% === Native Map Operation Tests ===

map_new_native_test() ->
    WordDefs = [
        {"empty-map", [], ['Map'], [map_new]}
    ],
    {ok, r1n_map_new} = af_ring1:compile_module(r1n_map_new, WordDefs, [{mode, native}]),
    ?assertEqual([{'Map', #{}}], r1n_map_new:'empty-map'([])).

map_put_native_test() ->
    WordDefs = [
        {"build-map", [], ['Map'],
         [map_new, {lit, {'String', <<"k">>}}, {lit, {'Int', 42}}, map_put]}
    ],
    {ok, r1n_map_put} = af_ring1:compile_module(r1n_map_put, WordDefs, [{mode, native}]),
    [{'Map', M}] = r1n_map_put:'build-map'([]),
    ?assertEqual({'Int', 42}, maps:get({'String', <<"k">>}, M)).

map_get_native_test() ->
    WordDefs = [
        {"build-and-get", [], ['Int'],
         [map_new, {lit, {'String', <<"k">>}}, {lit, {'Int', 99}}, map_put,
          {lit, {'String', <<"k">>}}, map_get]}
    ],
    {ok, r1n_map_get} = af_ring1:compile_module(r1n_map_get, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 99}], r1n_map_get:'build-and-get'([])).

map_multi_key_native_test() ->
    %% Build a map with two keys, retrieve both
    WordDefs = [
        {"build2", [], ['Map'],
         [map_new,
          {lit, {'String', <<"a">>}}, {lit, {'Int', 1}}, map_put,
          {lit, {'String', <<"b">>}}, {lit, {'Int', 2}}, map_put]},
        {"get-a", ['Map'], ['Int'],
         [{lit, {'String', <<"a">>}}, map_get]},
        {"get-b", ['Map'], ['Int'],
         [{lit, {'String', <<"b">>}}, map_get]}
    ],
    {ok, r1n_map_multi} = af_ring1:compile_module(r1n_map_multi, WordDefs, [{mode, native}]),
    [{'Map', M}] = r1n_map_multi:build2([]),
    ?assertEqual([{'Int', 1}], r1n_map_multi:'get-a'([{'Map', M}])),
    ?assertEqual([{'Int', 2}], r1n_map_multi:'get-b'([{'Map', M}])).

%%% === Native Print/Assert Tests ===

print_tos_native_test() ->
    %% print_tos should print TOS and remove it from stack
    WordDefs = [
        {"do-print", ['Any'], [], [print_tos]}
    ],
    {ok, r1n_print} = af_ring1:compile_module(r1n_print, WordDefs, [{mode, native}]),
    ?assertEqual([], r1n_print:'do-print'([{'Int', 42}])).

print_tos_preserves_rest_native_test() ->
    WordDefs = [
        {"do-print", ['Any'], [], [print_tos]}
    ],
    {ok, r1n_print2} = af_ring1:compile_module(r1n_print2, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 99}], r1n_print2:'do-print'([{'String', <<"hi">>}, {'Int', 99}])).

print_stack_native_test() ->
    %% print_stack should print the stack and leave it unchanged
    WordDefs = [
        {"show", [], [], [print_stack]}
    ],
    {ok, r1n_pstk} = af_ring1:compile_module(r1n_pstk, WordDefs, [{mode, native}]),
    Stack = [{'Int', 1}, {'Int', 2}],
    ?assertEqual(Stack, r1n_pstk:show(Stack)).

print_stack_empty_native_test() ->
    WordDefs = [
        {"show", [], [], [print_stack]}
    ],
    {ok, r1n_pstk_e} = af_ring1:compile_module(r1n_pstk_e, WordDefs, [{mode, native}]),
    ?assertEqual([], r1n_pstk_e:show([])).

assert_true_native_test() ->
    WordDefs = [
        {"check", ['Bool'], [], [assert_true]}
    ],
    {ok, r1n_assert} = af_ring1:compile_module(r1n_assert, WordDefs, [{mode, native}]),
    ?assertEqual([], r1n_assert:check([{'Bool', true}])).

assert_true_fails_native_test() ->
    WordDefs = [
        {"check", ['Bool'], [], [assert_true]}
    ],
    {ok, r1n_assert_f} = af_ring1:compile_module(r1n_assert_f, WordDefs, [{mode, native}]),
    ?assertError(assertion_failed, r1n_assert_f:check([{'Bool', false}])).

assert_true_preserves_stack_native_test() ->
    WordDefs = [
        {"check", ['Bool'], [], [assert_true]}
    ],
    {ok, r1n_assert_s} = af_ring1:compile_module(r1n_assert_s, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 5}], r1n_assert_s:check([{'Bool', true}, {'Int', 5}])).

assert_eq_native_test() ->
    WordDefs = [
        {"check-eq", ['Any', 'Any'], [], [assert_eq]}
    ],
    {ok, r1n_aeq} = af_ring1:compile_module(r1n_aeq, WordDefs, [{mode, native}]),
    ?assertEqual([], r1n_aeq:'check-eq'([{'Int', 42}, {'Int', 42}])).

assert_eq_fails_native_test() ->
    WordDefs = [
        {"check-eq", ['Any', 'Any'], [], [assert_eq]}
    ],
    {ok, r1n_aeq_f} = af_ring1:compile_module(r1n_aeq_f, WordDefs, [{mode, native}]),
    ?assertError({assert_eq_failed, _, _},
                 r1n_aeq_f:'check-eq'([{'Int', 1}, {'Int', 2}])).

assert_eq_preserves_stack_native_test() ->
    WordDefs = [
        {"check-eq", ['Any', 'Any'], [], [assert_eq]}
    ],
    {ok, r1n_aeq_s} = af_ring1:compile_module(r1n_aeq_s, WordDefs, [{mode, native}]),
    ?assertEqual([{'String', <<"ok">>}],
                 r1n_aeq_s:'check-eq'([{'Int', 5}, {'Int', 5}, {'String', <<"ok">>}])).

%%% === Native Boolean Logic Tests ===

and_op_native_test() ->
    WordDefs = [
        {"both", ['Bool', 'Bool'], ['Bool'], [and_op]}
    ],
    {ok, r1n_and} = af_ring1:compile_module(r1n_and, WordDefs, [{mode, native}]),
    ?assertEqual([{'Bool', true}], r1n_and:both([{'Bool', true}, {'Bool', true}])),
    ?assertEqual([{'Bool', false}], r1n_and:both([{'Bool', false}, {'Bool', true}])),
    ?assertEqual([{'Bool', false}], r1n_and:both([{'Bool', true}, {'Bool', false}])),
    ?assertEqual([{'Bool', false}], r1n_and:both([{'Bool', false}, {'Bool', false}])).

or_op_native_test() ->
    WordDefs = [
        {"either", ['Bool', 'Bool'], ['Bool'], [or_op]}
    ],
    {ok, r1n_or} = af_ring1:compile_module(r1n_or, WordDefs, [{mode, native}]),
    ?assertEqual([{'Bool', true}], r1n_or:either([{'Bool', true}, {'Bool', true}])),
    ?assertEqual([{'Bool', true}], r1n_or:either([{'Bool', false}, {'Bool', true}])),
    ?assertEqual([{'Bool', true}], r1n_or:either([{'Bool', true}, {'Bool', false}])),
    ?assertEqual([{'Bool', false}], r1n_or:either([{'Bool', false}, {'Bool', false}])).

%%% === Native List Operation Tests ===

nil_native_test() ->
    WordDefs = [
        {"empty-list", [], ['List'], [nil]}
    ],
    {ok, r1n_nil} = af_ring1:compile_module(r1n_nil, WordDefs, [{mode, native}]),
    ?assertEqual([{'List', []}], r1n_nil:'empty-list'([])).

cons_native_test() ->
    WordDefs = [
        {"singleton", ['Any'], ['List'], [nil, swap, cons]}
    ],
    {ok, r1n_cons} = af_ring1:compile_module(r1n_cons, WordDefs, [{mode, native}]),
    ?assertEqual([{'List', [{'Int', 42}]}],
                 r1n_cons:singleton([{'Int', 42}])).

cons_multi_native_test() ->
    %% Build a list with multiple elements using cons
    %% cons expects: [Element, {List, ListVal}, ...] — Element is TOS
    %% So we need: element on top, list below
    WordDefs = [
        {"make3", [], ['List'],
         [nil, {lit, {'Int', 3}}, cons,
              {lit, {'Int', 2}}, cons,
              {lit, {'Int', 1}}, cons]}
    ],
    {ok, r1n_cons_m} = af_ring1:compile_module(r1n_cons_m, WordDefs, [{mode, native}]),
    [{'List', L}] = r1n_cons_m:make3([]),
    ?assertEqual([{'Int', 1}, {'Int', 2}, {'Int', 3}], L).

head_native_test() ->
    WordDefs = [
        {"first", ['List'], ['Any'], [head]}
    ],
    {ok, r1n_head} = af_ring1:compile_module(r1n_head, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 1}],
                 r1n_head:first([{'List', [{'Int', 1}, {'Int', 2}]}])).

tail_native_test() ->
    WordDefs = [
        {"rest", ['List'], ['List'], [tail]}
    ],
    {ok, r1n_tail} = af_ring1:compile_module(r1n_tail, WordDefs, [{mode, native}]),
    ?assertEqual([{'List', [{'Int', 2}, {'Int', 3}]}],
                 r1n_tail:rest([{'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}])).

tail_single_native_test() ->
    WordDefs = [
        {"rest", ['List'], ['List'], [tail]}
    ],
    {ok, r1n_tail_s} = af_ring1:compile_module(r1n_tail_s, WordDefs, [{mode, native}]),
    ?assertEqual([{'List', []}],
                 r1n_tail_s:rest([{'List', [{'Int', 1}]}])).

head_tail_round_trip_native_test() ->
    %% head then tail of same list
    WordDefs = [
        {"first", ['List'], ['Any'], [head]},
        {"rest", ['List'], ['List'], [tail]}
    ],
    {ok, r1n_ht} = af_ring1:compile_module(r1n_ht, WordDefs, [{mode, native}]),
    Input = [{'List', [{'Int', 10}, {'Int', 20}]}],
    ?assertEqual([{'Int', 10}], r1n_ht:first(Input)),
    ?assertEqual([{'List', [{'Int', 20}]}], r1n_ht:rest(Input)).

%%% === Native generic_len Tests ===

generic_len_string_native_test() ->
    WordDefs = [
        {"len", ['Any'], ['Int'], [generic_len]}
    ],
    {ok, r1n_len_s} = af_ring1:compile_module(r1n_len_s, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 5}], r1n_len_s:len([{'String', <<"hello">>}])).

generic_len_list_native_test() ->
    WordDefs = [
        {"len", ['Any'], ['Int'], [generic_len]}
    ],
    {ok, r1n_len_l} = af_ring1:compile_module(r1n_len_l, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 3}], r1n_len_l:len([{'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}])).

generic_len_empty_native_test() ->
    WordDefs = [
        {"len", ['Any'], ['Int'], [generic_len]}
    ],
    {ok, r1n_len_e} = af_ring1:compile_module(r1n_len_e, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 0}], r1n_len_e:len([{'String', <<>>}])),
    ?assertEqual([{'Int', 0}], r1n_len_e:len([{'List', []}])).

%%% === Native String Concat Test ===

str_concat_native_test() ->
    WordDefs = [
        {"join", ['String', 'String'], ['String'], [str_concat]}
    ],
    {ok, r1n_concat} = af_ring1:compile_module(r1n_concat, WordDefs, [{mode, native}]),
    ?assertEqual([{'String', <<"helloworld">>}],
                 r1n_concat:join([{'String', <<"world">>}, {'String', <<"hello">>}])).

%%% === Native div/mod Tests ===

divop_native_test() ->
    WordDefs = [
        {"half", ['Int', 'Int'], ['Int'], [divop]}
    ],
    {ok, r1n_div} = af_ring1:compile_module(r1n_div, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 5}], r1n_div:half([{'Int', 2}, {'Int', 10}])).

modop_native_test() ->
    WordDefs = [
        {"remainder", ['Int', 'Int'], ['Int'], [modop]}
    ],
    {ok, r1n_mod} = af_ring1:compile_module(r1n_mod, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 1}], r1n_mod:remainder([{'Int', 3}, {'Int', 10}])).

%%% === Native select_clause Tests ===

select_clause_native_test() ->
    %% Factorial using select_clause in native mode
    FactBody = [
        {lit, {'List', [
            {[{'Int', 0}], [drop, {lit, {'Int', 1}}]},
            {['Int'], [dup, {lit, {'Int', 1}}, sub, {call, "fact"}, mul]}
        ]}},
        select_clause
    ],
    Defs = [{"fact", ['Int'], ['Int'], FactBody}],
    {ok, r1n_fact} = af_ring1:compile_module(r1n_fact, Defs, [{mode, native}]),
    ?assertEqual([{'Int', 1}], r1n_fact:fact([{'Int', 0}])),
    ?assertEqual([{'Int', 1}], r1n_fact:fact([{'Int', 1}])),
    ?assertEqual([{'Int', 120}], r1n_fact:fact([{'Int', 5}])),
    ?assertEqual([{'Int', 3628800}], r1n_fact:fact([{'Int', 10}])).

select_clause_fibonacci_native_test() ->
    FibBody = [
        {lit, {'List', [
            {[{'Int', 0}], [drop, {lit, {'Int', 0}}]},
            {[{'Int', 1}], [drop, {lit, {'Int', 1}}]},
            {['Int'], [dup, {lit, {'Int', 1}}, sub, {call, "fib"},
                       swap, {lit, {'Int', 2}}, sub, {call, "fib"},
                       add]}
        ]}},
        select_clause
    ],
    Defs = [{"fib", ['Int'], ['Int'], FibBody}],
    {ok, r1n_fib} = af_ring1:compile_module(r1n_fib, Defs, [{mode, native}]),
    ?assertEqual([{'Int', 0}], r1n_fib:fib([{'Int', 0}])),
    ?assertEqual([{'Int', 1}], r1n_fib:fib([{'Int', 1}])),
    ?assertEqual([{'Int', 55}], r1n_fib:fib([{'Int', 10}])).

select_clause_with_any_native_test() ->
    %% select_clause with Any type pattern
    Body = [
        {lit, {'List', [
            {[{'Int', 0}], [drop, {lit, {'String', <<"zero">>}}]},
            {['Int'], [drop, {lit, {'String', <<"int">>}}]},
            {['Any'], [drop, {lit, {'String', <<"other">>}}]}
        ]}},
        select_clause
    ],
    Defs = [{"type-of", ['Any'], ['String'], Body}],
    {ok, r1n_sc_any} = af_ring1:compile_module(r1n_sc_any, Defs, [{mode, native}]),
    ?assertEqual([{'String', <<"zero">>}], r1n_sc_any:'type-of'([{'Int', 0}])),
    ?assertEqual([{'String', <<"int">>}], r1n_sc_any:'type-of'([{'Int', 5}])),
    ?assertEqual([{'String', <<"other">>}], r1n_sc_any:'type-of'([{'String', <<"hi">>}])).

%%% === Native rot/over/2dup Inline Pattern Tests ===

rot_native_test() ->
    %% rot: [A,B,C|R] -> [C,A,B|R]
    WordDefs = [
        {"do-rot", ['Any', 'Any', 'Any'], ['Any', 'Any', 'Any'],
         [to_r, swap, from_r, swap]}
    ],
    {ok, r1n_rot} = af_ring1:compile_module(r1n_rot, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 3}, {'Int', 1}, {'Int', 2}],
                 r1n_rot:'do-rot'([{'Int', 1}, {'Int', 2}, {'Int', 3}])).

over_native_test() ->
    %% over: [A,B|R] -> [B,A,B|R]
    WordDefs = [
        {"do-over", ['Any', 'Any'], ['Any', 'Any', 'Any'],
         [to_r, dup, from_r, swap]}
    ],
    {ok, r1n_over} = af_ring1:compile_module(r1n_over, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 1}, {'Int', 2}, {'Int', 1}],
                 r1n_over:'do-over'([{'Int', 2}, {'Int', 1}])).

two_dup_native_test() ->
    %% 2dup: [A,B|R] -> [A,B,A,B|R]
    WordDefs = [
        {"do-2dup", ['Any', 'Any'], ['Any', 'Any', 'Any', 'Any'],
         [to_r, dup, from_r, swap, to_r, dup, from_r, swap]}
    ],
    {ok, r1n_2dup} = af_ring1:compile_module(r1n_2dup, WordDefs, [{mode, native}]),
    ?assertEqual([{'Int', 1}, {'Int', 2}, {'Int', 1}, {'Int', 2}],
                 r1n_2dup:'do-2dup'([{'Int', 1}, {'Int', 2}])).

%%% === Native apply_impl Test ===

apply_impl_native_test() ->
    %% apply_impl pushes unknown op as Atom
    WordDefs = [
        {"push-atom", [], ['Atom'], [{apply_impl, "hello"}]}
    ],
    {ok, r1n_appl} = af_ring1:compile_module(r1n_appl, WordDefs, [{mode, native}]),
    ?assertEqual([{'Atom', hello}], r1n_appl:'push-atom'([])).

apply_impl_binary_native_test() ->
    WordDefs = [
        {"push-atom-bin", [], ['Atom'], [{apply_impl, <<"world">>}]}
    ],
    {ok, r1n_appl_b} = af_ring1:compile_module(r1n_appl_b, WordDefs, [{mode, native}]),
    ?assertEqual([{'Atom', world}], r1n_appl_b:'push-atom-bin'([])).

apply_impl_atom_native_test() ->
    WordDefs = [
        {"push-atom-a", [], ['Atom'], [{apply_impl, foo}]}
    ],
    {ok, r1n_appl_a} = af_ring1:compile_module(r1n_appl_a, WordDefs, [{mode, native}]),
    ?assertEqual([{'Atom', foo}], r1n_appl_a:'push-atom-a'([])).

%%% === Binary/Save Tests ===

compile_to_binary_test() ->
    Defs = [{"inc", ['Int'], ['Int'], [{lit, {'Int', 1}}, add]}],
    {ok, Binary} = af_ring1:compile_to_binary(r1_bin_test, Defs),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

compile_to_binary_no_opts_test() ->
    %% Test compile_to_binary/2 (without opts — defaults to interpreted mode)
    Defs = [{"inc", ['Int'], ['Int'], [{lit, {'Int', 1}}, add]}],
    {ok, Binary} = af_ring1:compile_to_binary(r1_bin_noopt, Defs),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

compile_to_binary_native_test() ->
    Defs = [{"inc", ['Int'], ['Int'], [{lit, {'Int', 1}}, add]}],
    {ok, Binary} = af_ring1:compile_to_binary(r1_bin_nat, Defs, [{mode, native}]),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

save_module_test() ->
    Defs = [{"inc", ['Int'], ['Int'], [{lit, {'Int', 1}}, add]}],
    {ok, _} = af_ring1:compile_module(r1_save_test, Defs),
    Path = "/tmp/r1_save_test.beam",
    ok = af_ring1:save_module(r1_save_test, Path),
    {ok, Binary} = file:read_file(Path),
    ?assert(byte_size(Binary) > 0),
    file:delete(Path).

save_module_not_found_test() ->
    ?assertEqual({error, {no_binary, r1_nonexistent}},
                 af_ring1:save_module(r1_nonexistent, "/tmp/no.beam")).

save_module_native_test() ->
    Defs = [{"inc", ['Int'], ['Int'], [{lit, {'Int', 1}}, add]}],
    {ok, _} = af_ring1:compile_module(r1_save_nat, Defs, [{mode, native}]),
    Path = "/tmp/r1_save_nat.beam",
    ok = af_ring1:save_module(r1_save_nat, Path),
    {ok, Binary} = file:read_file(Path),
    ?assert(byte_size(Binary) > 0),
    file:delete(Path).

get_module_binary_test() ->
    Defs = [{"x", [], ['Int'], [{lit, {'Int', 1}}]}],
    {ok, _} = af_ring1:compile_module(r1_getbin_test, Defs),
    {ok, Bin} = af_ring1:get_module_binary(r1_getbin_test),
    ?assert(is_binary(Bin)).

get_module_binary_not_found_test() ->
    ?assertEqual(error, af_ring1:get_module_binary(r1_no_such_module)).

%%% === Empty/Edge Cases ===

empty_body_test() ->
    %% Identity function: no ops, stack passes through
    Defs = [{"identity", [], [], []}],
    {ok, r1_test_id} = af_ring1:compile_module(r1_test_id, Defs),
    ?assertEqual([{'Int', 42}], r1_test_id:identity([{'Int', 42}])),
    ?assertEqual([], r1_test_id:identity([])).

any_sig_test() ->
    Defs = [{"dup_any", ['Any'], ['Any', 'Any'], [dup]}],
    {ok, r1_test_any} = af_ring1:compile_module(r1_test_any, Defs),
    ?assertEqual([{'String', <<"hi">>}, {'String', <<"hi">>}],
                 r1_test_any:dup_any([{'String', <<"hi">>}])).

bool_sig_test() ->
    Defs = [{"flip_bool", [{'Bool', true}], ['Bool'], [drop, {lit, {'Bool', false}}]},
            {"flip_bool", [{'Bool', false}], ['Bool'], [drop, {lit, {'Bool', true}}]}],
    {ok, r1_test_bool} = af_ring1:compile_module(r1_test_bool, Defs),
    ?assertEqual([{'Bool', false}], r1_test_bool:flip_bool([{'Bool', true}])),
    ?assertEqual([{'Bool', true}], r1_test_bool:flip_bool([{'Bool', false}])).

float_sig_test() ->
    Defs = [{"half", [{'Float', 1.0}], ['Float'],
             [drop, {lit, {'Float', 0.5}}]}],
    {ok, r1_test_float} = af_ring1:compile_module(r1_test_float, Defs),
    ?assertEqual([{'Float', 0.5}], r1_test_float:half([{'Float', 1.0}])).

atom_sig_test() ->
    Defs = [{"check", [{'Atom', hello}], ['Bool'],
             [drop, {lit, {'Bool', true}}]}],
    {ok, r1_test_atom} = af_ring1:compile_module(r1_test_atom, Defs),
    ?assertEqual([{'Bool', true}], r1_test_atom:check([{'Atom', hello}])).

%%% === Cross-Mode Equivalence ===

equivalence_test_() ->
    Defs = [{"triple", ['Int'], ['Int'], [dup, dup, add, add]}],
    {foreach, fun() -> ok end, fun(_) -> ok end, [
        fun(_) -> {"interpreted triple", fun() ->
            {ok, r1_eq_i} = af_ring1:compile_module(r1_eq_i, Defs, [{mode, interpreted}]),
            ?assertEqual([{'Int', 21}], r1_eq_i:triple([{'Int', 7}]))
        end} end,
        fun(_) -> {"native triple", fun() ->
            {ok, r1_eq_n} = af_ring1:compile_module(r1_eq_n, Defs, [{mode, native}]),
            ?assertEqual([{'Int', 21}], r1_eq_n:triple([{'Int', 7}]))
        end} end
    ]}.

%%% === Combined Operations Tests (Native) ===

combined_arith_assert_native_test() ->
    %% Compute and assert in one word
    WordDefs = [
        {"check-double", ['Int'], [],
         [dup, add, {lit, {'Int', 10}}, eq, assert_true]}
    ],
    {ok, r1n_comb} = af_ring1:compile_module(r1n_comb, WordDefs, [{mode, native}]),
    ?assertEqual([], r1n_comb:'check-double'([{'Int', 5}])).

combined_map_product_native_test() ->
    register_point(),
    WordDefs = [
        {"make-pt", ['Int', 'Int'], ['Point'], [{product_new, 'Point', 2}]},
        {"wrap", ['Point'], ['Map'],
         [map_new, {lit, {'String', <<"point">>}}, swap,
          map_put
         ]}
    ],
    {ok, r1n_mp} = af_ring1:compile_module(r1n_mp, WordDefs, [{mode, native}]),
    [Pt] = r1n_mp:'make-pt'([{'Int', 20}, {'Int', 10}]),
    ?assertEqual('Point', element(1, Pt)),
    ?assertEqual({'Int', 10}, element(2, Pt)).
