-module(af_core_target_tests).

-include_lib("eunit/include/eunit.hrl").
-include("operation.hrl").

%% Build a minimal #operation{} that references a primitive by name. The
%% Core Erlang target does not need an impl — it works off the name alone.
op(Name) ->
    #operation{name = Name, sig_in = [], sig_out = [],
               impl = undefined, source = undefined}.

setup() ->
    af_type:reset().

%%====================================================================
%% supported_primitives/0
%%====================================================================

supported_primitives_test_() ->
    Sup = af_core_target:supported_primitives(),
    [
        ?_assert(lists:member("dup", Sup)),
        ?_assert(lists:member("drop", Sup)),
        ?_assert(lists:member("swap", Sup)),
        ?_assert(lists:member("+", Sup)),
        ?_assert(lists:member("-", Sup)),
        ?_assert(lists:member("*", Sup)),
        ?_assert(lists:member("/", Sup))
    ].

%%====================================================================
%% Validation rejects unsupported primitives
%%====================================================================

validation_test_() ->
    [
        {"compile_word accepts supported body", fun() ->
            Body = [op("dup"), op("+")],
            R = af_core_target:compile_word("double", ['Int'], ['Int'], Body),
            ?assertMatch({ok, _}, R)
        end},

        {"compile_word rejects unsupported primitive", fun() ->
            Body = [op("map-new")],
            R = af_core_target:compile_word("bad", [], ['Map'], Body),
            ?assertMatch({error, {unsupported_primitive, "map-new"}}, R)
        end},

        {"empty body is accepted", fun() ->
            R = af_core_target:compile_word("noop", [], [], []),
            ?assertMatch({ok, _}, R)
        end}
    ].

%%====================================================================
%% End-to-end: compile and run
%%====================================================================

compile_and_run_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"double via Core Erlang", fun() ->
            Body = [op("dup"), op("+")],
            ModName = af_core_test_double,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [{"double", ['Int'], ['Int'], Body}]
            ),
            %% Invoke the generated function
            Result = ModName:double([{'Int', 5}]),
            ?assertEqual([{'Int', 10}], Result)
        end} end,

        fun(_) -> {"double preserves rest of stack", fun() ->
            Body = [op("dup"), op("+")],
            ModName = af_core_test_double_rest,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [{"double", ['Int'], ['Int'], Body}]
            ),
            Result = ModName:double([{'Int', 3}, {'Int', 99}, {'Bool', true}]),
            ?assertEqual([{'Int', 6}, {'Int', 99}, {'Bool', true}], Result)
        end} end,

        fun(_) -> {"subtract via Core Erlang", fun() ->
            Body = [op("-")],
            ModName = af_core_test_sub,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [{"sub", ['Int', 'Int'], ['Int'], Body}]
            ),
            %% Stack: [a, b] (TOS-first means b is top). For `a b -` we want a - b.
            %% Our caller passes TOS-first: [TOP, UNDER, ...].
            %% The compiler destructures TOS=V0, under=V1. For "-" we compute
            %% V1 - V0 (under minus top), matching Forth convention.
            Result = ModName:sub([{'Int', 3}, {'Int', 10}]),
            ?assertEqual([{'Int', 7}], Result)
        end} end,

        fun(_) -> {"multiply via Core Erlang", fun() ->
            Body = [op("*")],
            ModName = af_core_test_mul,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [{"mul", ['Int', 'Int'], ['Int'], Body}]
            ),
            Result = ModName:mul([{'Int', 7}, {'Int', 6}]),
            ?assertEqual([{'Int', 42}], Result)
        end} end,

        fun(_) -> {"integer division via Core Erlang", fun() ->
            Body = [op("/")],
            ModName = af_core_test_div,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [{"divide", ['Int', 'Int'], ['Int'], Body}]
            ),
            Result = ModName:divide([{'Int', 3}, {'Int', 15}]),
            ?assertEqual([{'Int', 5}], Result)
        end} end,

        fun(_) -> {"drop via Core Erlang", fun() ->
            Body = [op("drop")],
            ModName = af_core_test_drop,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [{"dropit", ['Int'], [], Body}]
            ),
            Result = ModName:dropit([{'Int', 99}, {'Int', 7}]),
            ?assertEqual([{'Int', 7}], Result)
        end} end,

        fun(_) -> {"swap via Core Erlang", fun() ->
            Body = [op("swap")],
            ModName = af_core_test_swap,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [{"swp", ['Int', 'Int'], ['Int', 'Int'], Body}]
            ),
            Result = ModName:swp([{'Int', 1}, {'Int', 2}]),
            ?assertEqual([{'Int', 2}, {'Int', 1}], Result)
        end} end,

        fun(_) -> {"int literal push via Core Erlang", fun() ->
            %% Body with literal "42" op pushes {Int, 42}
            Body = [op("42")],
            ModName = af_core_test_lit,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [{"push42", [], ['Int'], Body}]
            ),
            Result = ModName:push42([]),
            ?assertEqual([{'Int', 42}], Result)
        end} end,

        fun(_) -> {"literal plus add", fun() ->
            Body = [op("dup"), op("1"), op("+")],
            ModName = af_core_test_addone,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [{"incdup", ['Int'], ['Int', 'Int'], Body}]
            ),
            Result = ModName:incdup([{'Int', 5}]),
            %% dup 5 -> [5, 5]
            %% 1 -> [1, 5, 5]
            %% + -> [6, 5]  (top+under)
            ?assertEqual([{'Int', 6}, {'Int', 5}], Result)
        end} end,

        fun(_) -> {"compile_words_to_binary returns a loadable binary", fun() ->
            Body = [op("dup"), op("+")],
            ModName = af_core_test_bin,
            {ok, ModName, Bin} = af_core_target:compile_words_to_binary(
                ModName,
                [{"double", ['Int'], ['Int'], Body}]
            ),
            ?assert(is_binary(Bin)),
            %% Load it manually
            {module, ModName} = code:load_binary(
                ModName, atom_to_list(ModName) ++ ".beam", Bin),
            Result = ModName:double([{'Int', 4}]),
            ?assertEqual([{'Int', 8}], Result)
        end} end,

        fun(_) -> {"multiple words in one module", fun() ->
            DoubleBody = [op("dup"), op("+")],
            SquareBody = [op("dup"), op("*")],
            ModName = af_core_test_multi,
            {ok, ModName} = af_core_target:compile_words_to_module(
                ModName,
                [
                    {"double", ['Int'], ['Int'], DoubleBody},
                    {"square", ['Int'], ['Int'], SquareBody}
                ]
            ),
            ?assertEqual([{'Int', 10}], ModName:double([{'Int', 5}])),
            ?assertEqual([{'Int', 25}], ModName:square([{'Int', 5}]))
        end} end
    ]}.

%%====================================================================
%% Error cases
%%====================================================================

error_paths_test_() ->
    [
        {"unsupported primitive surfaces in compile_words_to_binary", fun() ->
            Body = [op("map-new")],
            R = af_core_target:compile_words_to_binary(
                af_core_test_err,
                [{"bad", [], [], Body}]
            ),
            ?assertMatch({error, {compile_error, "bad",
                                   {unsupported_primitive, _}}}, R)
        end},

        {"compile_words_to_module propagates binary errors", fun() ->
            Body = [op("map-new")],
            R = af_core_target:compile_words_to_module(
                af_core_test_module_err,
                [{"bad", [], [], Body}]
            ),
            ?assertMatch({error, _}, R)
        end},

        {"compile_word catches unexpected exceptions", fun() ->
            %% Pass a non-list body. validate_body/1's pattern match on
            %% [] | [_|_] will fail (function_clause) so the generic
            %% catch in compile_word must handle it.
            R = af_core_target:compile_word("boom", [], [], not_a_list),
            ?assertMatch({error, _}, R)
        end}
    ].
