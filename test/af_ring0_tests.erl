-module(af_ring0_tests).

-include_lib("eunit/include/eunit.hrl").

%%% === Stack Primitives ===

dup_test() ->
    S0 = af_ring0:set_ds([{'Int', 42}], af_ring0:new()),
    S1 = af_ring0:exec(dup, S0),
    ?assertEqual([{'Int', 42}, {'Int', 42}], af_ring0:get_ds(S1)).

drop_test() ->
    S0 = af_ring0:set_ds([{'Int', 1}, {'Int', 2}], af_ring0:new()),
    S1 = af_ring0:exec(drop, S0),
    ?assertEqual([{'Int', 2}], af_ring0:get_ds(S1)).

swap_test() ->
    S0 = af_ring0:set_ds([{'Int', 1}, {'Int', 2}], af_ring0:new()),
    S1 = af_ring0:exec(swap, S0),
    ?assertEqual([{'Int', 2}, {'Int', 1}], af_ring0:get_ds(S1)).

to_r_test() ->
    S0 = af_ring0:set_ds([{'Int', 99}], af_ring0:new()),
    S1 = af_ring0:exec(to_r, S0),
    ?assertEqual([], af_ring0:get_ds(S1)),
    ?assertEqual([{'Int', 99}], af_ring0:get_rs(S1)).

from_r_test() ->
    S0 = af_ring0:set_ds([{'Int', 99}], af_ring0:new()),
    S1 = af_ring0:exec(to_r, S0),
    S2 = af_ring0:exec(from_r, S1),
    ?assertEqual([{'Int', 99}], af_ring0:get_ds(S2)),
    ?assertEqual([], af_ring0:get_rs(S2)).

%%% === Data Primitives ===

lit_test() ->
    S0 = af_ring0:new(),
    S1 = af_ring0:exec({lit, {'Int', 42}}, S0),
    ?assertEqual([{'Int', 42}], af_ring0:get_ds(S1)).

lit_string_test() ->
    S0 = af_ring0:new(),
    S1 = af_ring0:exec({lit, {'String', <<"hello">>}}, S0),
    ?assertEqual([{'String', <<"hello">>}], af_ring0:get_ds(S1)).

tuple_new_test() ->
    S0 = af_ring0:set_ds([{'Int', 1}, {'Int', 2}, {'Int', 3}], af_ring0:new()),
    S1 = af_ring0:exec({tuple_new, 3}, S0),
    ?assertEqual([{{'Int', 3}, {'Int', 2}, {'Int', 1}}], af_ring0:get_ds(S1)).

tuple_new_2_test() ->
    S0 = af_ring0:set_ds([{'Atom', point}, {'Int', 10}], af_ring0:new()),
    S1 = af_ring0:exec({tuple_new, 2}, S0),
    ?assertEqual([{{'Int', 10}, {'Atom', point}}], af_ring0:get_ds(S1)).

tuple_get_test() ->
    Tuple = {a, b, c},
    S0 = af_ring0:set_ds([Tuple], af_ring0:new()),
    S1 = af_ring0:exec({tuple_get, 2}, S0),
    ?assertEqual([b], af_ring0:get_ds(S1)).

tuple_put_test() ->
    Tuple = {a, b, c},
    S0 = af_ring0:set_ds([new_val, Tuple], af_ring0:new()),
    S1 = af_ring0:exec({tuple_put, 2}, S0),
    ?assertEqual([{a, new_val, c}], af_ring0:get_ds(S1)).

%%% === List Primitives ===

nil_test() ->
    S0 = af_ring0:new(),
    S1 = af_ring0:exec(nil, S0),
    ?assertEqual([{'List', []}], af_ring0:get_ds(S1)).

cons_test() ->
    S0 = af_ring0:set_ds([{'Int', 1}, {'List', []}], af_ring0:new()),
    S1 = af_ring0:exec(cons, S0),
    ?assertEqual([{'List', [{'Int', 1}]}], af_ring0:get_ds(S1)).

cons_multiple_test() ->
    %% cons expects: Elem on TOS, List below -> build [1,2,3]
    S0 = af_ring0:run([
        nil,
        {lit, {'Int', 3}}, cons,
        {lit, {'Int', 2}}, cons,
        {lit, {'Int', 1}}, cons
    ], af_ring0:new()),
    ?assertEqual([{'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}],
                 af_ring0:get_ds(S0)).

head_test() ->
    S0 = af_ring0:set_ds([{'List', [{'Int', 1}, {'Int', 2}]}], af_ring0:new()),
    S1 = af_ring0:exec(head, S0),
    ?assertEqual([{'Int', 1}], af_ring0:get_ds(S1)).

tail_test() ->
    S0 = af_ring0:set_ds([{'List', [{'Int', 1}, {'Int', 2}]}], af_ring0:new()),
    S1 = af_ring0:exec(tail, S0),
    ?assertEqual([{'List', [{'Int', 2}]}], af_ring0:get_ds(S1)).

%%% === Arithmetic ===

add_test() ->
    S0 = af_ring0:set_ds([{'Int', 3}, {'Int', 5}], af_ring0:new()),
    S1 = af_ring0:exec(add, S0),
    ?assertEqual([{'Int', 8}], af_ring0:get_ds(S1)).

add_float_test() ->
    S0 = af_ring0:set_ds([{'Float', 1.5}, {'Int', 2}], af_ring0:new()),
    S1 = af_ring0:exec(add, S0),
    ?assertEqual([{'Float', 3.5}], af_ring0:get_ds(S1)).

sub_test() ->
    S0 = af_ring0:set_ds([{'Int', 3}, {'Int', 10}], af_ring0:new()),
    S1 = af_ring0:exec(sub, S0),
    ?assertEqual([{'Int', 7}], af_ring0:get_ds(S1)).

mul_test() ->
    S0 = af_ring0:set_ds([{'Int', 4}, {'Int', 7}], af_ring0:new()),
    S1 = af_ring0:exec(mul, S0),
    ?assertEqual([{'Int', 28}], af_ring0:get_ds(S1)).

divop_int_test() ->
    S0 = af_ring0:set_ds([{'Int', 3}, {'Int', 10}], af_ring0:new()),
    S1 = af_ring0:exec(divop, S0),
    ?assertEqual([{'Int', 3}], af_ring0:get_ds(S1)).

divop_float_test() ->
    S0 = af_ring0:set_ds([{'Float', 2.0}, {'Int', 5}], af_ring0:new()),
    S1 = af_ring0:exec(divop, S0),
    ?assertEqual([{'Float', 2.5}], af_ring0:get_ds(S1)).

modop_test() ->
    S0 = af_ring0:set_ds([{'Int', 3}, {'Int', 10}], af_ring0:new()),
    S1 = af_ring0:exec(modop, S0),
    ?assertEqual([{'Int', 1}], af_ring0:get_ds(S1)).

%%% === Comparison + Logic ===

eq_true_test() ->
    S0 = af_ring0:set_ds([{'Int', 5}, {'Int', 5}], af_ring0:new()),
    S1 = af_ring0:exec(eq, S0),
    ?assertEqual([{'Bool', true}], af_ring0:get_ds(S1)).

eq_false_test() ->
    S0 = af_ring0:set_ds([{'Int', 5}, {'Int', 3}], af_ring0:new()),
    S1 = af_ring0:exec(eq, S0),
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

eq_cross_type_test() ->
    S0 = af_ring0:set_ds([{'Int', 5}, {'Float', 5.0}], af_ring0:new()),
    S1 = af_ring0:exec(eq, S0),
    %% =:= is strict: Int 5 =/= Float 5.0
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

lt_true_test() ->
    S0 = af_ring0:set_ds([{'Int', 10}, {'Int', 3}], af_ring0:new()),
    S1 = af_ring0:exec(lt, S0),
    ?assertEqual([{'Bool', true}], af_ring0:get_ds(S1)).

lt_false_test() ->
    S0 = af_ring0:set_ds([{'Int', 3}, {'Int', 10}], af_ring0:new()),
    S1 = af_ring0:exec(lt, S0),
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

not_op_test() ->
    S0 = af_ring0:set_ds([{'Bool', true}], af_ring0:new()),
    S1 = af_ring0:exec(not_op, S0),
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

not_op_false_test() ->
    S0 = af_ring0:set_ds([{'Bool', false}], af_ring0:new()),
    S1 = af_ring0:exec(not_op, S0),
    ?assertEqual([{'Bool', true}], af_ring0:get_ds(S1)).

%%% === Control Flow ===

call_test() ->
    %% Define a word "double" that does dup + add
    S0 = af_ring0:run([
        {def_word, "double", [dup, add]},
        {lit, {'Int', 21}},
        {call, "double"}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 42}], af_ring0:get_ds(S0)).

branch_if_true_test() ->
    %% If true, branch to instruction 4 (skip the lit 99)
    S0 = af_ring0:run([
        {lit, {'Bool', true}},
        {branch_if, 4},
        {lit, {'Int', 99}},
        {lit, {'Int', 1}}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 1}], af_ring0:get_ds(S0)).

branch_if_false_test() ->
    %% If false, continue normally — executes lit 99 then lit 1
    S0 = af_ring0:run([
        {lit, {'Bool', false}},
        {branch_if, 4},
        {lit, {'Int', 99}},
        {lit, {'Int', 1}}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 1}, {'Int', 99}], af_ring0:get_ds(S0)).

branch_unconditional_test() ->
    S0 = af_ring0:run([
        {branch, 3},
        {lit, {'Int', 99}},
        {lit, {'Int', 1}}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 1}], af_ring0:get_ds(S0)).

return_test() ->
    S0 = af_ring0:run([
        {lit, {'Int', 1}},
        return,
        {lit, {'Int', 2}}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 1}], af_ring0:get_ds(S0)).

nested_call_test() ->
    S0 = af_ring0:run([
        {def_word, "inc", [{lit, {'Int', 1}}, add]},
        {def_word, "inc2", [{call, "inc"}, {call, "inc"}]},
        {lit, {'Int', 10}},
        {call, "inc2"}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 12}], af_ring0:get_ds(S0)).

%%% === Type System ===

type_new_test() ->
    S0 = af_ring0:run([
        {lit, {'Atom', 'MyType'}},
        type_new
    ], af_ring0:new()),
    Types = af_ring0:get_types(S0),
    ?assert(maps:is_key('MyType', Types)),
    ?assertEqual(#{ops => #{}, handler => undefined}, maps:get('MyType', Types)).

type_add_op_test() ->
    S0 = af_ring0:run([
        {lit, {'Atom', 'Counter'}},
        type_new,
        {lit, {'Atom', 'Counter'}},
        {lit, {'String', <<"inc">>}},
        {lit, {'List', ['Counter']}},
        {lit, {'List', ['Counter']}},
        {lit, {'Code', [{lit, {'Int', 1}}, add]}},
        type_add_op
    ], af_ring0:new()),
    Types = af_ring0:get_types(S0),
    CounterType = maps:get('Counter', Types),
    Ops = maps:get(ops, CounterType),
    ?assert(maps:is_key(<<"inc">>, Ops)),
    [OpDef] = maps:get(<<"inc">>, Ops),
    ?assertEqual(['Counter'], maps:get(sig_in, OpDef)),
    ?assertEqual(['Counter'], maps:get(sig_out, OpDef)).

type_add_op_multi_clause_test() ->
    %% Add two clauses for same op name
    S0 = af_ring0:run([
        {lit, {'Atom', 'Num'}},
        type_new,
        %% First clause: factorial 0
        {lit, {'Atom', 'Num'}},
        {lit, {'String', <<"factorial">>}},
        {lit, {'List', [{'Int', 0}]}},
        {lit, {'List', ['Int']}},
        {lit, {'Code', [drop, {lit, {'Int', 1}}]}},
        type_add_op,
        %% Second clause: factorial N
        {lit, {'Atom', 'Num'}},
        {lit, {'String', <<"factorial">>}},
        {lit, {'List', ['Int']}},
        {lit, {'List', ['Int']}},
        {lit, {'Code', [dup, {lit, {'Int', 1}}, sub]}},
        type_add_op
    ], af_ring0:new()),
    Types = af_ring0:get_types(S0),
    NumType = maps:get('Num', Types),
    Ops = maps:get(ops, NumType),
    OpDefs = maps:get(<<"factorial">>, Ops),
    ?assertEqual(2, length(OpDefs)).

type_get_op_found_test() ->
    S0 = af_ring0:run([
        {lit, {'Atom', 'T'}},
        type_new,
        {lit, {'Atom', 'T'}},
        {lit, {'String', <<"foo">>}},
        {lit, {'List', []}},
        {lit, {'List', []}},
        {lit, {'Code', []}},
        type_add_op,
        {lit, {'Atom', 'T'}},
        {lit, {'String', <<"foo">>}},
        type_get_op
    ], af_ring0:new()),
    [{'List', OpList}] = af_ring0:get_ds(S0),
    ?assertEqual(1, length(OpList)).

type_get_op_not_found_test() ->
    S0 = af_ring0:run([
        {lit, {'Atom', 'T'}},
        type_new,
        {lit, {'Atom', 'T'}},
        {lit, {'String', <<"bar">>}},
        type_get_op
    ], af_ring0:new()),
    ?assertEqual([{'Atom', not_found}], af_ring0:get_ds(S0)).

type_get_op_unknown_type_test() ->
    S0 = af_ring0:run([
        {lit, {'Atom', 'Unknown'}},
        {lit, {'String', <<"bar">>}},
        type_get_op
    ], af_ring0:new()),
    ?assertEqual([{'Atom', not_found}], af_ring0:get_ds(S0)).

type_get_test() ->
    S0 = af_ring0:run([
        {lit, {'Atom', 'X'}},
        type_new,
        {lit, {'Atom', 'X'}},
        type_get
    ], af_ring0:new()),
    [{'Map', TypeDef}] = af_ring0:get_ds(S0),
    ?assertEqual(#{ops => #{}, handler => undefined}, TypeDef).

type_get_not_found_test() ->
    S0 = af_ring0:run([
        {lit, {'Atom', 'Z'}},
        type_get
    ], af_ring0:new()),
    ?assertEqual([{'Atom', not_found}], af_ring0:get_ds(S0)).

type_set_test() ->
    %% Create a type, then replace its definition entirely
    S0 = af_ring0:run([
        {lit, {'Atom', 'X'}},
        type_new,
        {lit, {'Atom', 'X'}},
        {lit, {'Map', #{ops => #{<<"custom">> => [custom_op]}, handler => my_handler}}},
        type_set,
        {lit, {'Atom', 'X'}},
        type_get
    ], af_ring0:new()),
    [{'Map', TypeDef}] = af_ring0:get_ds(S0),
    ?assertEqual(#{ops => #{<<"custom">> => [custom_op]}, handler => my_handler}, TypeDef).

%%% === Pattern Matching ===

match_sig_type_test() ->
    S0 = af_ring0:set_ds([{'List', ['Int']}, {'Int', 42}], af_ring0:new()),
    S1 = af_ring0:exec(match_sig, S0),
    [{'Bool', Result} | _] = af_ring0:get_ds(S1),
    ?assert(Result).

match_sig_type_mismatch_test() ->
    S0 = af_ring0:set_ds([{'List', ['String']}, {'Int', 42}], af_ring0:new()),
    S1 = af_ring0:exec(match_sig, S0),
    [{'Bool', Result} | _] = af_ring0:get_ds(S1),
    ?assertNot(Result).

match_sig_value_constraint_test() ->
    S0 = af_ring0:set_ds([{'List', [{'Int', 0}]}, {'Int', 0}], af_ring0:new()),
    S1 = af_ring0:exec(match_sig, S0),
    [{'Bool', Result} | _] = af_ring0:get_ds(S1),
    ?assert(Result).

match_sig_value_constraint_mismatch_test() ->
    S0 = af_ring0:set_ds([{'List', [{'Int', 0}]}, {'Int', 5}], af_ring0:new()),
    S1 = af_ring0:exec(match_sig, S0),
    [{'Bool', Result} | _] = af_ring0:get_ds(S1),
    ?assertNot(Result).

match_sig_any_test() ->
    S0 = af_ring0:set_ds([{'List', ['Any']}, {'String', <<"hi">>}], af_ring0:new()),
    S1 = af_ring0:exec(match_sig, S0),
    [{'Bool', Result} | _] = af_ring0:get_ds(S1),
    ?assert(Result).

match_sig_multi_test() ->
    S0 = af_ring0:set_ds([{'List', ['Int', 'String']},
                           {'Int', 1}, {'String', <<"x">>}], af_ring0:new()),
    S1 = af_ring0:exec(match_sig, S0),
    [{'Bool', Result} | _] = af_ring0:get_ds(S1),
    ?assert(Result).

match_sig_empty_stack_test() ->
    S0 = af_ring0:set_ds([{'List', ['Int']}], af_ring0:new()),
    S1 = af_ring0:exec(match_sig, S0),
    [{'Bool', Result} | _] = af_ring0:get_ds(S1),
    ?assertNot(Result).

match_sig_empty_sig_test() ->
    S0 = af_ring0:set_ds([{'List', []}, {'Int', 1}], af_ring0:new()),
    S1 = af_ring0:exec(match_sig, S0),
    [{'Bool', Result} | _] = af_ring0:get_ds(S1),
    ?assert(Result).

match_value_test() ->
    S0 = af_ring0:set_ds([{'Int', 42}], af_ring0:new()),
    S1 = af_ring0:exec({match_value, {'Int', 42}}, S0),
    [{'Bool', Result}, {'Int', 42}] = af_ring0:get_ds(S1),
    ?assert(Result).

match_value_mismatch_test() ->
    S0 = af_ring0:set_ds([{'Int', 42}], af_ring0:new()),
    S1 = af_ring0:exec({match_value, {'Int', 99}}, S0),
    [{'Bool', Result}, {'Int', 42}] = af_ring0:get_ds(S1),
    ?assertNot(Result).

select_clause_test() ->
    Clauses = [
        {[{'Int', 0}], [{lit, {'String', <<"zero">>}}]},
        {['Int'],      [{lit, {'String', <<"nonzero">>}}]}
    ],
    %% Match first clause
    S0 = af_ring0:set_ds([{'List', Clauses}, {'Int', 0}], af_ring0:new()),
    S1 = af_ring0:exec(select_clause, S0),
    [{'String', <<"zero">>} | _] = af_ring0:get_ds(S1).

select_clause_second_test() ->
    Clauses = [
        {[{'Int', 0}], [{lit, {'String', <<"zero">>}}]},
        {['Int'],      [{lit, {'String', <<"nonzero">>}}]}
    ],
    S0 = af_ring0:set_ds([{'List', Clauses}, {'Int', 5}], af_ring0:new()),
    S1 = af_ring0:exec(select_clause, S0),
    [{'String', <<"nonzero">>} | _] = af_ring0:get_ds(S1).

select_clause_no_match_test() ->
    Clauses = [
        {['String'], [{lit, {'Int', 1}}]}
    ],
    S0 = af_ring0:set_ds([{'List', Clauses}, {'Int', 5}], af_ring0:new()),
    ?assertError({no_matching_clause, _}, af_ring0:exec(select_clause, S0)).

%%% === Actors ===

actor_spawn_send_receive_test() ->
    %% Spawn an actor that receives a message and sends it back doubled
    Parent = self(),
    WordBody = [
        receive_msg,                    %% receive Int
        dup, add,                       %% double it
        {lit, {'Actor', Parent}},       %% push parent pid
        swap,                           %% Actor Value -> Value Actor
        send_msg                        %% send back
    ],
    S0 = af_ring0:run([
        {lit, {'Code', WordBody}},
        spawn_actor,                     %% spawn the actor, stack: [Actor]
        {lit, {'Int', 21}},              %% stack: [Int, Actor]
        send_msg                         %% send 21 to actor (Value on TOS, Actor below)
    ], af_ring0:new()),
    %% Wait for response
    receive
        {r0_msg, {'Int', 42}} -> ok
    after 1000 ->
        ?assert(false)
    end,
    ?assertEqual([], af_ring0:get_ds(S0)).

receive_timeout_success_test() ->
    Self = self(),
    erlang:spawn(fun() ->
        timer:sleep(10),
        Self ! {r0_msg, {'Int', 99}}
    end),
    S0 = af_ring0:set_ds([{'Int', 500}], af_ring0:new()),
    S1 = af_ring0:exec(receive_timeout, S0),
    ?assertEqual([{'Bool', true}, {'Int', 99}], af_ring0:get_ds(S1)).

receive_timeout_expired_test() ->
    S0 = af_ring0:set_ds([{'Int', 1}], af_ring0:new()),
    S1 = af_ring0:exec(receive_timeout, S0),
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

%%% === I/O ===

emit_test() ->
    S0 = af_ring0:exec({emit, <<"hello">>}, af_ring0:new()),
    ?assertEqual(<<"hello">>, af_ring0:get_output(S0)).

emit_append_test() ->
    S0 = af_ring0:run([
        {emit, <<"hello">>},
        {emit, <<" world">>}
    ], af_ring0:new()),
    ?assertEqual(<<"hello world">>, af_ring0:get_output(S0)).

emit_tos_test() ->
    S0 = af_ring0:set_ds([{'String', <<"out">>}], af_ring0:new()),
    S1 = af_ring0:exec(emit_tos, S0),
    ?assertEqual(<<"out">>, af_ring0:get_output(S1)),
    ?assertEqual([], af_ring0:get_ds(S1)).

read_n_test() ->
    S0 = af_ring0:set_input(<<"abcdef">>, af_ring0:new()),
    S1 = af_ring0:exec({read_n, 3}, S0),
    ?assertEqual([{'String', <<"abc">>}], af_ring0:get_ds(S1)).

read_n_eof_test() ->
    S0 = af_ring0:set_input(<<"ab">>, af_ring0:new()),
    S1 = af_ring0:exec({read_n, 5}, S0),
    ?assertEqual([{'Atom', eof}], af_ring0:get_ds(S1)).

%%% === BIF Call ===

bif_add_test() ->
    S0 = af_ring0:set_ds([{'Int', 2}, {'Int', 3}], af_ring0:new()),
    S1 = af_ring0:exec({bif, erlang, '+', 2}, S0),
    ?assertEqual([{'Int', 5}], af_ring0:get_ds(S1)).

bif_atom_to_list_test() ->
    S0 = af_ring0:set_ds([{'Atom', hello}], af_ring0:new()),
    S1 = af_ring0:exec({bif, erlang, atom_to_list, 1}, S0),
    %% atom_to_list returns a string (list), auto_tag wraps as List
    [{'List', _}] = af_ring0:get_ds(S1).

bif_byte_size_test() ->
    S0 = af_ring0:set_ds([{'String', <<"hello">>}], af_ring0:new()),
    S1 = af_ring0:exec({bif, erlang, byte_size, 1}, S0),
    ?assertEqual([{'Int', 5}], af_ring0:get_ds(S1)).

bif_is_integer_test() ->
    S0 = af_ring0:set_ds([{'Int', 42}], af_ring0:new()),
    S1 = af_ring0:exec({bif, erlang, is_integer, 1}, S0),
    ?assertEqual([{'Bool', true}], af_ring0:get_ds(S1)).

%%% === Map Primitives ===

map_new_test() ->
    S0 = af_ring0:exec(map_new, af_ring0:new()),
    ?assertEqual([{'Map', #{}}], af_ring0:get_ds(S0)).

map_put_test() ->
    S0 = af_ring0:set_ds([{'Int', 42}, {'String', <<"key">>}, {'Map', #{}}], af_ring0:new()),
    S1 = af_ring0:exec(map_put, S0),
    [{'Map', M}] = af_ring0:get_ds(S1),
    ?assertEqual(#{ {'String', <<"key">>} => {'Int', 42} }, M).

map_get_found_test() ->
    M = #{ {'String', <<"x">>} => {'Int', 10} },
    S0 = af_ring0:set_ds([{'String', <<"x">>}, {'Map', M}], af_ring0:new()),
    S1 = af_ring0:exec(map_get, S0),
    ?assertEqual([{'Int', 10}], af_ring0:get_ds(S1)).

map_get_not_found_test() ->
    S0 = af_ring0:set_ds([{'String', <<"nope">>}, {'Map', #{}}], af_ring0:new()),
    S1 = af_ring0:exec(map_get, S0),
    ?assertEqual([{'Atom', not_found}], af_ring0:get_ds(S1)).

map_delete_test() ->
    M = #{ {'String', <<"a">>} => {'Int', 1} },
    S0 = af_ring0:set_ds([{'String', <<"a">>}, {'Map', M}], af_ring0:new()),
    S1 = af_ring0:exec(map_delete, S0),
    ?assertEqual([{'Map', #{}}], af_ring0:get_ds(S1)).

map_keys_test() ->
    M = #{ {'String', <<"a">>} => {'Int', 1} },
    S0 = af_ring0:set_ds([{'Map', M}], af_ring0:new()),
    S1 = af_ring0:exec(map_keys, S0),
    ?assertEqual([{'List', [{'String', <<"a">>}]}], af_ring0:get_ds(S1)).

map_has_true_test() ->
    M = #{ {'String', <<"x">>} => {'Int', 1} },
    S0 = af_ring0:set_ds([{'String', <<"x">>}, {'Map', M}], af_ring0:new()),
    S1 = af_ring0:exec(map_has, S0),
    ?assertEqual([{'Bool', true}], af_ring0:get_ds(S1)).

map_has_false_test() ->
    S0 = af_ring0:set_ds([{'String', <<"y">>}, {'Map', #{}}], af_ring0:new()),
    S1 = af_ring0:exec(map_has, S0),
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

map_roundtrip_test() ->
    %% Build map with put, then get
    %% map_put wants: Value Key Map (TOS first)
    S0 = af_ring0:run([
        map_new,                            %% DS: [Map]
        {lit, {'String', <<"name">>}},      %% DS: ["name", Map]
        {lit, {'String', <<"Alice">>}},      %% DS: ["Alice", "name", Map]
        map_put,                             %% DS: [Map']
        {lit, {'String', <<"name">>}},       %% DS: ["name", Map']
        map_get                              %% DS: ["Alice"]
    ], af_ring0:new()),
    ?assertEqual([{'String', <<"Alice">>}], af_ring0:get_ds(S0)).

%%% === String Primitives ===

str_len_test() ->
    S0 = af_ring0:set_ds([{'String', <<"hello">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_len, S0),
    ?assertEqual([{'Int', 5}], af_ring0:get_ds(S1)).

str_nth_test() ->
    S0 = af_ring0:set_ds([{'Int', 1}, {'String', <<"abc">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_nth, S0),
    ?assertEqual([{'String', <<"b">>}], af_ring0:get_ds(S1)).

str_slice_test() ->
    S0 = af_ring0:set_ds([{'Int', 3}, {'Int', 1}, {'String', <<"hello">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_slice, S0),
    ?assertEqual([{'String', <<"ell">>}], af_ring0:get_ds(S1)).

str_concat_test() ->
    S0 = af_ring0:set_ds([{'String', <<" world">>}, {'String', <<"hello">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_concat, S0),
    ?assertEqual([{'String', <<"hello world">>}], af_ring0:get_ds(S1)).

str_eq_true_test() ->
    S0 = af_ring0:set_ds([{'String', <<"a">>}, {'String', <<"a">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_eq, S0),
    ?assertEqual([{'Bool', true}], af_ring0:get_ds(S1)).

str_eq_false_test() ->
    S0 = af_ring0:set_ds([{'String', <<"b">>}, {'String', <<"a">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_eq, S0),
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

str_to_int_success_test() ->
    S0 = af_ring0:set_ds([{'String', <<"42">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_to_int, S0),
    ?assertEqual([{'Bool', true}, {'Int', 42}], af_ring0:get_ds(S1)).

str_to_int_fail_test() ->
    S0 = af_ring0:set_ds([{'String', <<"abc">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_to_int, S0),
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

str_to_float_success_test() ->
    S0 = af_ring0:set_ds([{'String', <<"3.14">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_to_float, S0),
    ?assertEqual([{'Bool', true}, {'Float', 3.14}], af_ring0:get_ds(S1)).

str_to_float_fail_test() ->
    S0 = af_ring0:set_ds([{'String', <<"abc">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_to_float, S0),
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

str_find_found_test() ->
    S0 = af_ring0:set_ds([{'String', <<"ll">>}, {'String', <<"hello">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_find, S0),
    ?assertEqual([{'Bool', true}, {'Int', 2}], af_ring0:get_ds(S1)).

str_find_not_found_test() ->
    S0 = af_ring0:set_ds([{'String', <<"xyz">>}, {'String', <<"hello">>}], af_ring0:new()),
    S1 = af_ring0:exec(str_find, S0),
    ?assertEqual([{'Bool', false}], af_ring0:get_ds(S1)).

%%% === Word Definition ===

def_word_instruction_test() ->
    S0 = af_ring0:run([
        {def_word, "square", [dup, mul]},
        {lit, {'Int', 7}},
        {call, "square"}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 49}], af_ring0:get_ds(S0)).

def_word_stack_test() ->
    S0 = af_ring0:run([
        {lit, {'String', <<"cube">>}},
        {lit, {'Code', [dup, dup, mul, mul]}},
        def_word,
        {lit, {'Int', 3}},
        {call, "cube"}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 27}], af_ring0:get_ds(S0)).

%%% === Integration Tests ===

%% Derive 'over' from primitives: to_r dup from_r swap
over_derived_test() ->
    S0 = af_ring0:run([
        {def_word, "over", [to_r, dup, from_r, swap]},
        {lit, {'Int', 1}},
        {lit, {'Int', 2}},
        {call, "over"}
    ], af_ring0:new()),
    %% [2, 1] -> over -> [1, 2, 1]
    ?assertEqual([{'Int', 1}, {'Int', 2}, {'Int', 1}], af_ring0:get_ds(S0)).

%% Derive 'rot' from primitives: to_r swap from_r swap
rot_derived_test() ->
    S0 = af_ring0:run([
        {def_word, "rot", [to_r, swap, from_r, swap]},
        {lit, {'Int', 1}},
        {lit, {'Int', 2}},
        {lit, {'Int', 3}},
        {call, "rot"}
    ], af_ring0:new()),
    %% [3, 2, 1] -> rot -> [1, 3, 2]
    ?assertEqual([{'Int', 1}, {'Int', 3}, {'Int', 2}], af_ring0:get_ds(S0)).

%% Factorial using select_clause for pattern matching
factorial_test() ->
    FactBody = [
        {lit, {'List', [
            {[{'Int', 0}], [drop, {lit, {'Int', 1}}]},
            {['Int'],      [dup, {lit, {'Int', 1}}, sub, {call, "fact"}, mul]}
        ]}},
        select_clause
    ],
    S0 = af_ring0:run([
        {def_word, "fact", FactBody},
        {lit, {'Int', 5}},
        {call, "fact"}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 120}], af_ring0:get_ds(S0)).

%% Fibonacci using select_clause
fibonacci_test() ->
    FibBody = [
        {lit, {'List', [
            {[{'Int', 0}], [drop, {lit, {'Int', 0}}]},
            {[{'Int', 1}], [drop, {lit, {'Int', 1}}]},
            {['Int'],      [dup, {lit, {'Int', 1}}, sub, {call, "fib"},
                            swap, {lit, {'Int', 2}}, sub, {call, "fib"},
                            add]}
        ]}},
        select_clause
    ],
    S0 = af_ring0:run([
        {def_word, "fib", FibBody},
        {lit, {'Int', 10}},
        {call, "fib"}
    ], af_ring0:new()),
    ?assertEqual([{'Int', 55}], af_ring0:get_ds(S0)).

%% Type-driven dispatch: define ops on types, look them up
type_dispatch_test() ->
    S0 = af_ring0:run([
        {lit, {'Atom', 'Dog'}}, type_new,
        {lit, {'Atom', 'Dog'}},
        {lit, {'String', <<"speak">>}},
        {lit, {'List', ['Dog']}},
        {lit, {'List', ['String']}},
        {lit, {'Code', [drop, {lit, {'String', <<"woof">>}}]}},
        type_add_op,
        %% Look up and execute
        {lit, {'Atom', 'Dog'}},
        {lit, {'String', <<"speak">>}},
        type_get_op
    ], af_ring0:new()),
    [{'List', [OpDef]}] = af_ring0:get_ds(S0),
    %% Now manually run the body
    {'Code', Body} = maps:get(body, OpDef),
    S1 = af_ring0:set_ds([{'Atom', fido}], af_ring0:new()),
    S2 = af_ring0:run(Body, S1),
    ?assertEqual([{'String', <<"woof">>}], af_ring0:get_ds(S2)).

%% Program that builds and inspects a type
type_introspection_test() ->
    S0 = af_ring0:run([
        {lit, {'Atom', 'Vec2'}}, type_new,
        {lit, {'Atom', 'Vec2'}},
        {lit, {'String', <<"x">>}},
        {lit, {'List', ['Vec2']}},
        {lit, {'List', ['Int']}},
        {lit, {'Code', []}},
        type_add_op,
        %% Inspect the full type
        {lit, {'Atom', 'Vec2'}},
        type_get
    ], af_ring0:new()),
    [{'Map', TypeDef}] = af_ring0:get_ds(S0),
    Ops = maps:get(ops, TypeDef),
    ?assert(maps:is_key(<<"x">>, Ops)).

%% Test run with empty program
empty_program_test() ->
    S0 = af_ring0:run([], af_ring0:new()),
    ?assertEqual([], af_ring0:get_ds(S0)).

%% Test new state
new_state_test() ->
    S = af_ring0:new(),
    ?assertEqual([], af_ring0:get_ds(S)),
    ?assertEqual([], af_ring0:get_rs(S)),
    ?assertEqual(#{}, af_ring0:get_types(S)),
    ?assertEqual(#{}, af_ring0:get_words(S)),
    ?assertEqual(<<>>, af_ring0:get_output(S)).

%% Combined I/O test
io_roundtrip_test() ->
    S0 = af_ring0:set_input(<<"hello world">>, af_ring0:new()),
    S1 = af_ring0:run([
        {read_n, 5},
        emit_tos,
        {emit, <<"!">>}
    ], S0),
    ?assertEqual(<<"hello!">>, af_ring0:get_output(S1)).
