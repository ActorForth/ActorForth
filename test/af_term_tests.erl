-module(af_term_tests).

-include_lib("eunit/include/eunit.hrl").
-include("af_type.hrl").

%% --- to_stack_item ---

to_stack_item_test_() ->
    [
        {"integer", fun() ->
            ?assertEqual({'Int', 42}, af_term:to_stack_item(42))
        end},
        {"negative integer", fun() ->
            ?assertEqual({'Int', -7}, af_term:to_stack_item(-7))
        end},
        {"true", fun() ->
            ?assertEqual({'Bool', true}, af_term:to_stack_item(true))
        end},
        {"false", fun() ->
            ?assertEqual({'Bool', false}, af_term:to_stack_item(false))
        end},
        {"binary", fun() ->
            ?assertEqual({'String', <<"hello">>}, af_term:to_stack_item(<<"hello">>))
        end},
        {"empty binary", fun() ->
            ?assertEqual({'String', <<>>}, af_term:to_stack_item(<<>>))
        end},
        {"atom", fun() ->
            ?assertEqual({'Atom', "foo"}, af_term:to_stack_item(foo))
        end},
        {"list of integers", fun() ->
            ?assertEqual({'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]},
                         af_term:to_stack_item([1, 2, 3]))
        end},
        {"empty list", fun() ->
            ?assertEqual({'List', []}, af_term:to_stack_item([]))
        end},
        {"map", fun() ->
            Result = af_term:to_stack_item(#{<<"key">> => 42}),
            {'Map', M} = Result,
            ?assertEqual({'Int', 42}, maps:get({'String', <<"key">>}, M))
        end},
        {"empty map", fun() ->
            ?assertEqual({'Map', #{}}, af_term:to_stack_item(#{}))
        end},
        {"tuple becomes Tuple", fun() ->
            ?assertEqual({'Tuple', {1, 2}},
                         af_term:to_stack_item({1, 2}))
        end},
        {"pid becomes Actor", fun() ->
            {'Actor', #{pid := P}} = af_term:to_stack_item(self()),
            ?assertEqual(self(), P)
        end},
        {"nested: map of lists", fun() ->
            Result = af_term:to_stack_item(#{<<"nums">> => [1, 2]}),
            {'Map', M} = Result,
            ?assertEqual({'List', [{'Int', 1}, {'Int', 2}]},
                         maps:get({'String', <<"nums">>}, M))
        end}
    ].

%% --- from_stack_item ---

from_stack_item_test_() ->
    [
        {"integer", fun() ->
            ?assertEqual(42, af_term:from_stack_item({'Int', 42}))
        end},
        {"bool", fun() ->
            ?assertEqual(true, af_term:from_stack_item({'Bool', true}))
        end},
        {"string", fun() ->
            ?assertEqual(<<"hello">>, af_term:from_stack_item({'String', <<"hello">>}))
        end},
        {"atom", fun() ->
            ?assertEqual(foo, af_term:from_stack_item({'Atom', "foo"}))
        end},
        {"list", fun() ->
            ?assertEqual([1, 2, 3],
                         af_term:from_stack_item({'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}))
        end},
        {"map", fun() ->
            M = #{ {'String', <<"k">>} => {'Int', 1} },
            ?assertEqual(#{<<"k">> => 1}, af_term:from_stack_item({'Map', M}))
        end},
        {"actor returns pid", fun() ->
            ?assertEqual(self(),
                         af_term:from_stack_item({'Actor', #{pid => self(), type_name => t, vocab => #{}}}))
        end},
        {"product type becomes map with type key", fun() ->
            %% Register the type so from_stack_item can find its field layout.
            af_type:reset(),
            af_type:register_type(#af_type{name = 'Point',
                                            fields = [{x, 'Int'}, {y, 'Int'}]}),
            %% New storage: positional tuple with raw values.
            Instance = {'Point', 10, 20},
            Result = af_term:from_stack_item(Instance),
            ?assertEqual(#{type => 'Point', x => 10, y => 20}, Result)
        end}
    ].

%% --- roundtrip ---

roundtrip_test_() ->
    [
        {"integer roundtrip", fun() ->
            ?assertEqual(42, af_term:from_stack_item(af_term:to_stack_item(42)))
        end},
        {"bool roundtrip", fun() ->
            ?assertEqual(true, af_term:from_stack_item(af_term:to_stack_item(true)))
        end},
        {"binary roundtrip", fun() ->
            ?assertEqual(<<"hi">>, af_term:from_stack_item(af_term:to_stack_item(<<"hi">>)))
        end},
        {"list roundtrip", fun() ->
            ?assertEqual([1, 2, 3], af_term:from_stack_item(af_term:to_stack_item([1, 2, 3])))
        end},
        {"map roundtrip", fun() ->
            M = #{<<"a">> => 1, <<"b">> => 2},
            ?assertEqual(M, af_term:from_stack_item(af_term:to_stack_item(M)))
        end},
        {"empty structures roundtrip", fun() ->
            ?assertEqual([], af_term:from_stack_item(af_term:to_stack_item([]))),
            ?assertEqual(#{}, af_term:from_stack_item(af_term:to_stack_item(#{})))
        end}
    ].

%% --- batch ---

batch_test_() ->
    [
        {"to_stack_items", fun() ->
            ?assertEqual([{'Int', 1}, {'String', <<"hi">>}],
                         af_term:to_stack_items([1, <<"hi">>]))
        end},
        {"from_stack_items", fun() ->
            ?assertEqual([1, <<"hi">>],
                         af_term:from_stack_items([{'Int', 1}, {'String', <<"hi">>}]))
        end}
    ].
