-module(af_ring1_tests).

-include_lib("eunit/include/eunit.hrl").

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

%%% === Binary/Save Tests ===

compile_to_binary_test() ->
    Defs = [{"inc", ['Int'], ['Int'], [{lit, {'Int', 1}}, add]}],
    {ok, Binary} = af_ring1:compile_to_binary(r1_bin_test, Defs),
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
