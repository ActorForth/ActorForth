-module(af_type_tests).

-include_lib("eunit/include/eunit.hrl").
-include("operation.hrl").
-include("af_type.hrl").

%% Helpers
setup() -> af_type:reset().

%% --- init tests ---

init_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"registers Any and Atom types", fun() ->
            ?assertMatch({ok, #af_type{name = 'Any'}}, af_type:get_type('Any')),
            ?assertMatch({ok, #af_type{name = 'Atom'}}, af_type:get_type('Atom'))
        end} end
    ]}.

%% --- register_type tests ---

register_type_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"register and retrieve a type", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            ?assertMatch({ok, #af_type{name = 'Int'}}, af_type:get_type('Int'))
        end} end,
        fun(_) -> {"unknown type returns not_found", fun() ->
            ?assertEqual(not_found, af_type:get_type('Bogus'))
        end} end
    ]}.

%% --- add_op tests ---

add_op_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"add operation to a type", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            Op = #operation{name = "+", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                           impl = fun(C) -> C end},
            ?assertEqual(ok, af_type:add_op('Int', Op))
        end} end,
        fun(_) -> {"add op to unknown type returns error", fun() ->
            Op = #operation{name = "x", sig_in = [], sig_out = [],
                           impl = fun(C) -> C end},
            ?assertMatch({error, {unknown_type, _}}, af_type:add_op('Bogus', Op))
        end} end,
        fun(_) -> {"multiple ops with same name accumulate", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            Op1 = #operation{name = "f", sig_in = [{'Int', 0}], sig_out = ['Int'],
                            impl = fun(C) -> C end},
            Op2 = #operation{name = "f", sig_in = ['Int'], sig_out = ['Int'],
                            impl = fun(C) -> C end},
            af_type:add_op('Int', Op1),
            af_type:add_op('Int', Op2),
            {ok, #af_type{ops = Ops}} = af_type:get_type('Int'),
            ?assertEqual(2, length(maps:get("f", Ops)))
        end} end
    ]}.

%% --- find_op tests ---

find_op_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"find op in TOS type dictionary", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            Op = #operation{name = "+", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                           impl = fun(C) -> C end},
            af_type:add_op('Int', Op),
            Stack = [{'Int', 5}, {'Int', 3}],
            ?assertMatch({ok, #operation{name = "+"}}, af_type:find_op("+", Stack))
        end} end,
        fun(_) -> {"fall back to Any dictionary", fun() ->
            Op = #operation{name = "dup", sig_in = ['Any'], sig_out = ['Any', 'Any'],
                           impl = fun(C) -> C end},
            af_type:add_op('Any', Op),
            af_type:register_type(#af_type{name = 'Int'}),
            Stack = [{'Int', 5}],
            ?assertMatch({ok, #operation{name = "dup"}}, af_type:find_op("dup", Stack))
        end} end,
        fun(_) -> {"not_found when no match", fun() ->
            Stack = [{'Atom', "hello"}],
            ?assertEqual(not_found, af_type:find_op("nonexistent", Stack))
        end} end,
        fun(_) -> {"not_found when sig doesn't match", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            Op = #operation{name = "+", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                           impl = fun(C) -> C end},
            af_type:add_op('Int', Op),
            Stack = [{'Int', 5}],  %% only one Int, need two
            ?assertEqual(not_found, af_type:find_op("+", Stack))
        end} end,
        fun(_) -> {"find op on empty stack checks Any only", fun() ->
            Op = #operation{name = "nop", sig_in = [], sig_out = [],
                           impl = fun(C) -> C end},
            af_type:add_op('Any', Op),
            ?assertMatch({ok, #operation{name = "nop"}}, af_type:find_op("nop", []))
        end} end,
        fun(_) -> {"TOS type dictionary takes priority over Any", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            AnyOp = #operation{name = "show", sig_in = ['Any'], sig_out = [],
                              impl = fun(C) -> C end},
            IntOp = #operation{name = "show", sig_in = ['Int'], sig_out = [],
                              impl = fun(C) -> C end},
            af_type:add_op('Any', AnyOp),
            af_type:add_op('Int', IntOp),
            Stack = [{'Int', 5}],
            {ok, Found} = af_type:find_op("show", Stack),
            %% Should find the Int version, not the Any version
            ?assertEqual(['Int'], Found#operation.sig_in)
        end} end,
        fun(_) -> {"constructor in Any dict found via fallback", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            %% int constructor: Atom -> Int, registered in Any dict
            Op = #operation{name = "int", sig_in = ['Atom'], sig_out = ['Int'],
                           impl = fun(C) -> C end},
            af_type:add_op('Any', Op),
            Stack = [{'Atom', "42"}],
            ?assertMatch({ok, #operation{name = "int"}}, af_type:find_op("int", Stack))
        end} end
    ]}.

%% --- match_sig tests ---

match_sig_test_() ->
    [
        {"empty sig matches anything", fun() ->
            ?assert(af_type:match_sig([], [])),
            ?assert(af_type:match_sig([], [{'Int', 5}]))
        end},
        {"Any matches any type", fun() ->
            ?assert(af_type:match_sig(['Any'], [{'Int', 5}])),
            ?assert(af_type:match_sig(['Any'], [{'Atom', "x"}]))
        end},
        {"exact type match", fun() ->
            ?assert(af_type:match_sig(['Int'], [{'Int', 5}])),
            ?assertNot(af_type:match_sig(['Int'], [{'Atom', "5"}]))
        end},
        {"value constraint match", fun() ->
            ?assert(af_type:match_sig([{'Int', 0}], [{'Int', 0}])),
            ?assertNot(af_type:match_sig([{'Int', 0}], [{'Int', 5}]))
        end},
        {"sig longer than stack fails", fun() ->
            ?assertNot(af_type:match_sig(['Int', 'Int'], [{'Int', 5}]))
        end},
        {"multi-element sig", fun() ->
            ?assert(af_type:match_sig(['Int', 'Int'], [{'Int', 5}, {'Int', 3}])),
            ?assertNot(af_type:match_sig(['Int', 'Int'], [{'Int', 5}, {'Atom', "3"}]))
        end},
        {"Any in multi-element sig", fun() ->
            ?assert(af_type:match_sig(['Any', 'Int'], [{'Atom', "x"}, {'Int', 3}]))
        end}
    ].

%% --- find_op_by_name tests ---

find_op_by_name_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"find_op_by_name finds registered op", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            Op = #operation{name = "+", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                           impl = fun(C) -> C end},
            af_type:add_op('Int', Op),
            ?assertMatch({ok, #operation{name = "+"}}, af_type:find_op_by_name("+", 'Int'))
        end} end,
        fun(_) -> {"find_op_by_name returns not_found for missing op", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            ?assertEqual(not_found, af_type:find_op_by_name("nonexistent", 'Int'))
        end} end,
        fun(_) -> {"find_op_by_name returns not_found for missing type", fun() ->
            ?assertEqual(not_found, af_type:find_op_by_name("x", 'Bogus'))
        end} end
    ]}.

%% --- all_types test ---

all_types_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"all_types returns all registered types", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            af_type:register_type(#af_type{name = 'Bool'}),
            Types = af_type:all_types(),
            Names = [T#af_type.name || T <- Types],
            ?assert(lists:member('Any', Names)),
            ?assert(lists:member('Atom', Names)),
            ?assert(lists:member('Int', Names)),
            ?assert(lists:member('Bool', Names))
        end} end
    ]}.

%% --- init idempotency ---

init_idempotent_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"calling init twice doesn't crash", fun() ->
            ?assertEqual(ok, af_type:init()),
            ?assertMatch({ok, #af_type{name = 'Any'}}, af_type:get_type('Any'))
        end} end
    ]}.

%% --- find_op_in_type with unknown type ---

find_op_edge_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"find_op_in_tos with unknown TOS type returns not_found", fun() ->
            Stack = [{'UnknownType', foo}],
            ?assertEqual(not_found, af_type:find_op_in_tos("anything", Stack))
        end} end,
        fun(_) -> {"find_op_in_tos with empty stack returns not_found", fun() ->
            ?assertEqual(not_found, af_type:find_op_in_tos("anything", []))
        end} end,
        fun(_) -> {"find_op_in_any with empty stack", fun() ->
            Op = #operation{name = "nop", sig_in = [], sig_out = [],
                           impl = fun(C) -> C end},
            af_type:add_op('Any', Op),
            ?assertMatch({ok, _}, af_type:find_op_in_any("nop", []))
        end} end,
        fun(_) -> {"find_op_in_any returns not_found when not present", fun() ->
            ?assertEqual(not_found, af_type:find_op_in_any("bogus", []))
        end} end
    ]}.

%% --- replace_ops tests ---

replace_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"replace_ops replaces all ops for a name", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            Op1 = #operation{name = "f", sig_in = ['Int'], sig_out = ['Int'],
                            impl = fun(C) -> C end},
            Op2 = #operation{name = "f", sig_in = [{'Int', 0}], sig_out = ['Int'],
                            impl = fun(C) -> C end},
            af_type:add_op('Int', Op1),
            af_type:add_op('Int', Op2),
            %% Now replace with a single op
            NewOp = #operation{name = "f", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                              impl = fun(C) -> C end},
            ?assertEqual(ok, af_type:replace_ops('Int', "f", [NewOp])),
            {ok, #af_type{ops = Ops}} = af_type:get_type('Int'),
            ?assertEqual(1, length(maps:get("f", Ops))),
            [Found] = maps:get("f", Ops),
            ?assertEqual(['Int', 'Int'], Found#operation.sig_in)
        end} end,
        fun(_) -> {"replace_ops on unknown type returns error", fun() ->
            NewOp = #operation{name = "f", sig_in = [], sig_out = [],
                              impl = fun(C) -> C end},
            ?assertMatch({error, {unknown_type, 'Bogus'}},
                         af_type:replace_ops('Bogus', "f", [NewOp]))
        end} end
    ]}.

%% --- match_sig wildcard and type variable tests ---

match_sig_advanced_test_() ->
    [
        {"underscore wildcard matches any type", fun() ->
            ?assert(af_type:match_sig(['_'], [{'Int', 5}])),
            ?assert(af_type:match_sig(['_'], [{'Atom', "x"}])),
            ?assert(af_type:match_sig(['_', '_'], [{'Int', 5}, {'Bool', true}]))
        end},
        {"type variable _a matches any type", fun() ->
            ?assert(af_type:match_sig(['_a'], [{'Int', 5}])),
            ?assert(af_type:match_sig(['_a'], [{'Atom', "x"}]))
        end},
        {"type variable matches even when name coincidentally equals type atom", fun() ->
            %% If the type variable name happens to equal the stack type atom
            %% (unlikely in practice but tests line 106-112)
            ?assert(af_type:match_sig(['_a'], [{'_a', something}]))
        end},
        {"type variable matches when type does not match name", fun() ->
            %% Tests line 116-121: type atom is a type var but doesn't match stack type
            ?assert(af_type:match_sig(['_b'], [{'Int', 5}]))
        end},
        {"non-type-variable atom that doesn't match stack type fails", fun() ->
            %% Tests line 116-121 false branch
            ?assertNot(af_type:match_sig(['Bool'], [{'Int', 5}]))
        end},
        {"value constraint with wrong type fails", fun() ->
            %% {Int, 0} in sig vs {Bool, 0} on stack — type mismatch
            ?assertNot(af_type:match_sig([{'Int', 0}], [{'Bool', 0}]))
        end},
        {"value constraint with wrong value fails", fun() ->
            ?assertNot(af_type:match_sig([{'Int', 0}], [{'Int', 1}]))
        end},
        {"catch-all returns false for non-standard sig elements", fun() ->
            %% A tuple with 3 elements in sig — not {Type, Value}, not atom
            ?assertNot(af_type:match_sig([{a, b, c}], [{'Int', 5}]))
        end}
    ].

%% --- snapshot tests ---

snapshot_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"snapshot returns map of all types", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            Snap = af_type:snapshot(),
            ?assert(is_map(Snap)),
            ?assertMatch(#af_type{name = 'Any'}, maps:get('Any', Snap)),
            ?assertMatch(#af_type{name = 'Int'}, maps:get('Int', Snap)),
            ?assertMatch(#af_type{name = 'Atom'}, maps:get('Atom', Snap))
        end} end
    ]}.

%% --- dict_* function tests ---

dict_functions_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"dict_register_type and dict_get_type", fun() ->
            Dict0 = #{},
            Type = #af_type{name = 'TestType'},
            Dict1 = af_type:dict_register_type(Type, Dict0),
            ?assertMatch({ok, #af_type{name = 'TestType'}},
                         af_type:dict_get_type('TestType', Dict1)),
            ?assertEqual(not_found, af_type:dict_get_type('Missing', Dict1))
        end} end,
        fun(_) -> {"dict_all_types returns all values", fun() ->
            Dict = #{
                'A' => #af_type{name = 'A'},
                'B' => #af_type{name = 'B'}
            },
            Types = af_type:dict_all_types(Dict),
            ?assertEqual(2, length(Types)),
            Names = [T#af_type.name || T <- Types],
            ?assert(lists:member('A', Names)),
            ?assert(lists:member('B', Names))
        end} end,
        fun(_) -> {"dict_add_op adds to existing type", fun() ->
            Type = #af_type{name = 'Int'},
            Dict0 = #{int => Type, 'Int' => Type},
            Op = #operation{name = "+", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                           impl = fun(C) -> C end},
            Dict1 = af_type:dict_add_op('Int', Op, Dict0),
            {ok, #af_type{ops = Ops}} = af_type:dict_get_type('Int', Dict1),
            ?assertEqual(1, length(maps:get("+", Ops)))
        end} end,
        fun(_) -> {"dict_add_op creates type if not present", fun() ->
            Dict0 = #{},
            Op = #operation{name = "x", sig_in = [], sig_out = [],
                           impl = fun(C) -> C end},
            Dict1 = af_type:dict_add_op('NewType', Op, Dict0),
            {ok, #af_type{name = 'NewType', ops = Ops}} =
                af_type:dict_get_type('NewType', Dict1),
            ?assertEqual(1, length(maps:get("x", Ops)))
        end} end,
        fun(_) -> {"dict_add_op accumulates multiple ops with same name", fun() ->
            Dict0 = #{},
            Op1 = #operation{name = "f", sig_in = ['Int'], sig_out = ['Int'],
                            impl = fun(C) -> C end},
            Op2 = #operation{name = "f", sig_in = [{'Int', 0}], sig_out = ['Int'],
                            impl = fun(C) -> C end},
            Dict1 = af_type:dict_add_op('Int', Op1, Dict0),
            Dict2 = af_type:dict_add_op('Int', Op2, Dict1),
            {ok, #af_type{ops = Ops}} = af_type:dict_get_type('Int', Dict2),
            ?assertEqual(2, length(maps:get("f", Ops)))
        end} end,
        fun(_) -> {"dict_replace_ops replaces ops in dict", fun() ->
            Op1 = #operation{name = "f", sig_in = ['Int'], sig_out = ['Int'],
                            impl = fun(C) -> C end},
            Dict0 = af_type:dict_add_op('Int', Op1, #{}),
            NewOp = #operation{name = "f", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                              impl = fun(C) -> C end},
            Dict1 = af_type:dict_replace_ops('Int', "f", [NewOp], Dict0),
            {ok, #af_type{ops = Ops}} = af_type:dict_get_type('Int', Dict1),
            ?assertEqual(1, length(maps:get("f", Ops))),
            [Found] = maps:get("f", Ops),
            ?assertEqual(['Int', 'Int'], Found#operation.sig_in)
        end} end,
        fun(_) -> {"dict_replace_ops on missing type returns dict unchanged", fun() ->
            Dict0 = #{},
            NewOp = #operation{name = "f", sig_in = [], sig_out = [],
                              impl = fun(C) -> C end},
            Dict1 = af_type:dict_replace_ops('Missing', "f", [NewOp], Dict0),
            ?assertEqual(Dict0, Dict1)
        end} end,
        fun(_) -> {"dict_find_op_in_tos finds op in TOS type", fun() ->
            Op = #operation{name = "+", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                           impl = fun(C) -> C end},
            Dict = af_type:dict_add_op('Int', Op, #{}),
            Stack = [{'Int', 5}, {'Int', 3}],
            ?assertMatch({ok, #operation{name = "+"}},
                         af_type:dict_find_op_in_tos("+", Stack, Dict))
        end} end,
        fun(_) -> {"dict_find_op_in_tos with empty stack returns not_found", fun() ->
            ?assertEqual(not_found, af_type:dict_find_op_in_tos("x", [], #{}))
        end} end,
        fun(_) -> {"dict_find_op_in_tos with missing type returns not_found", fun() ->
            Stack = [{'Unknown', 1}],
            ?assertEqual(not_found, af_type:dict_find_op_in_tos("x", Stack, #{}))
        end} end,
        fun(_) -> {"dict_find_op_in_any finds op in Any dict", fun() ->
            Op = #operation{name = "nop", sig_in = [], sig_out = [],
                           impl = fun(C) -> C end},
            Dict = af_type:dict_add_op('Any', Op, #{}),
            ?assertMatch({ok, #operation{name = "nop"}},
                         af_type:dict_find_op_in_any("nop", [], Dict))
        end} end,
        fun(_) -> {"dict_find_op_in_any returns not_found when missing", fun() ->
            ?assertEqual(not_found, af_type:dict_find_op_in_any("bogus", [], #{}))
        end} end,
        fun(_) -> {"dict_find_op_in_tos with non-matching sig returns not_found", fun() ->
            Op = #operation{name = "+", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                           impl = fun(C) -> C end},
            Dict = af_type:dict_add_op('Int', Op, #{}),
            Stack = [{'Int', 5}],  %% only one Int, need two
            ?assertEqual(not_found, af_type:dict_find_op_in_tos("+", Stack, Dict))
        end} end
    ]}.
