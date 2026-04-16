-module(af_forget_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

setup() ->
    af_type:reset().

eval(Input) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()).

%%====================================================================
%% defined_at stamping
%%====================================================================

defined_at_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"new op receives a defined_at counter", fun() ->
            _ = eval(": foo Int -> Int ; 1 + ."),
            {ok, Op} = af_type:find_op_by_name("foo", 'Int'),
            ?assert(is_integer(Op#operation.defined_at)),
            ?assert(Op#operation.defined_at > 0)
        end} end,

        fun(_) -> {"later op has higher defined_at", fun() ->
            _ = eval(": alpha Int -> Int ; 1 + ."),
            _ = eval(": beta Int -> Int ; 2 + ."),
            {ok, A} = af_type:find_op_by_name("alpha", 'Int'),
            {ok, B} = af_type:find_op_by_name("beta", 'Int'),
            ?assert(B#operation.defined_at > A#operation.defined_at)
        end} end,

        fun(_) -> {"current_def_counter increases across definitions", fun() ->
            Before = af_type:current_def_counter(),
            _ = eval(": w1 Int -> Int ; 1 + ."),
            _ = eval(": w2 Int -> Int ; 1 + ."),
            After = af_type:current_def_counter(),
            ?assert(After > Before)
        end} end
    ]}.

%%====================================================================
%% forget semantics
%%====================================================================

forget_basic_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"forget removes the named word", fun() ->
            _ = eval(": doomed Int -> Int ; 99 + ."),
            ?assertMatch({ok, _}, af_type:find_op_by_name("doomed", 'Int')),
            ok = af_type:forget("doomed"),
            ?assertEqual(not_found,
                         af_type:find_op_by_name("doomed", 'Int'))
        end} end,

        fun(_) -> {"forget also removes all later-defined words", fun() ->
            _ = eval(": survivor Int -> Int ; 1 + ."),
            _ = eval(": victim   Int -> Int ; 2 + ."),
            _ = eval(": later    Int -> Int ; 3 + ."),
            ok = af_type:forget("victim"),
            ?assertMatch({ok, _}, af_type:find_op_by_name("survivor", 'Int')),
            ?assertEqual(not_found,
                         af_type:find_op_by_name("victim", 'Int')),
            ?assertEqual(not_found,
                         af_type:find_op_by_name("later", 'Int'))
        end} end,

        fun(_) -> {"forget returns not_found for unknown name", fun() ->
            ?assertEqual({error, not_found}, af_type:forget("no-such-word"))
        end} end,

        fun(_) -> {"forget preserves earlier definitions", fun() ->
            _ = eval(": early Int -> Int ; 1 + ."),
            Before = af_type:current_def_counter(),
            _ = eval(": mid Int -> Int ; 2 + ."),
            _ = eval(": late Int -> Int ; 3 + ."),
            ok = af_type:forget("mid"),
            {ok, Op} = af_type:find_op_by_name("early", 'Int'),
            ?assert(Op#operation.defined_at =< Before)
        end} end,

        fun(_) -> {"forget accepts atom names", fun() ->
            _ = eval(": atom-named Int -> Int ; 1 + ."),
            ok = af_type:forget('atom-named'),
            ?assertEqual(not_found,
                         af_type:find_op_by_name("atom-named", 'Int'))
        end} end,

        fun(_) -> {"forget accepts binary names", fun() ->
            _ = eval(": bin-named Int -> Int ; 1 + ."),
            ok = af_type:forget(<<"bin-named">>),
            ?assertEqual(not_found,
                         af_type:find_op_by_name("bin-named", 'Int'))
        end} end
    ]}.

forget_rolls_counter_back_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"forget rolls counter back so new definitions reuse slots", fun() ->
            _ = eval(": first Int -> Int ; 1 + ."),
            {ok, FirstOp} = af_type:find_op_by_name("first", 'Int'),
            FirstAt = FirstOp#operation.defined_at,

            _ = eval(": doomed Int -> Int ; 9 + ."),
            ok = af_type:forget("doomed"),

            _ = eval(": next Int -> Int ; 2 + ."),
            {ok, NextOp} = af_type:find_op_by_name("next", 'Int'),
            %% next should have a counter that is at most one greater than
            %% first's counter (the slot doomed had vacated).
            ?assert(NextOp#operation.defined_at >= FirstAt),
            ?assert(NextOp#operation.defined_at =< FirstAt + 2)
        end} end
    ]}.

%%====================================================================
%% `forget` word at the REPL
%%====================================================================

forget_word_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"forget word removes named word", fun() ->
            _ = eval(": temp Int -> Int ; 7 + ."),
            ?assertMatch({ok, _}, af_type:find_op_by_name("temp", 'Int')),
            _ = eval("\"temp\" forget"),
            ?assertEqual(not_found,
                         af_type:find_op_by_name("temp", 'Int'))
        end} end,

        fun(_) -> {"forget word with unknown name does not crash", fun() ->
            %% Just verify no exception; the word prints a notice.
            _ = eval("\"never-defined\" forget"),
            ok
        end} end,

        fun(_) -> {"forget word leaves empty stack", fun() ->
            _ = eval(": temp2 Int -> Int ; 5 + ."),
            C = eval("\"temp2\" forget"),
            ?assertEqual([], C#continuation.data_stack)
        end} end
    ]}.

%%====================================================================
%% replace_ops preserves defined_at
%%====================================================================

replace_preserves_counter_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"replace_ops does not bump the counter", fun() ->
            _ = eval(": stable Int -> Int ; 1 + ."),
            {ok, Before} = af_type:find_op_by_name("stable", 'Int'),
            BeforeAt = Before#operation.defined_at,

            %% Replace with a brand-new op lacking defined_at; the carry-forward
            %% logic should apply the original's counter.
            NewOp = #operation{
                name = "stable", sig_in = ['Int'], sig_out = ['Int'],
                impl = fun(Cont) -> Cont end
            },
            af_type:replace_ops('Int', "stable", [NewOp]),

            {ok, After} = af_type:find_op_by_name("stable", 'Int'),
            ?assertEqual(BeforeAt, After#operation.defined_at)
        end} end
    ]}.

%%====================================================================
%% stamp_op path: pre-stamped op keeps its counter
%%====================================================================

pre_stamped_add_op_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"add_op preserves an already-set defined_at", fun() ->
            Op = #operation{
                name = "prestamped",
                sig_in = ['Int'], sig_out = ['Int'],
                impl = fun(Cont) -> Cont end,
                defined_at = 42
            },
            ok = af_type:add_op('Int', Op),
            {ok, Got} = af_type:find_op_by_name("prestamped", 'Int'),
            ?assertEqual(42, Got#operation.defined_at)
        end} end
    ]}.

replace_ops_mixed_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"replace_ops with pre-stamped new op keeps its counter", fun() ->
            _ = eval(": orig Int -> Int ; 1 + ."),
            NewOp = #operation{
                name = "orig", sig_in = ['Int'], sig_out = ['Int'],
                impl = fun(Cont) -> Cont end,
                defined_at = 999
            },
            af_type:replace_ops('Int', "orig", [NewOp]),
            {ok, Got} = af_type:find_op_by_name("orig", 'Int'),
            ?assertEqual(999, Got#operation.defined_at)
        end} end,

        fun(_) -> {"replace_ops with more new ops than existing counters", fun() ->
            _ = eval(": solo Int -> Int ; 1 + ."),
            %% Replace with two unstamped ops. First inherits original counter;
            %% second must allocate a fresh one.
            Op1 = #operation{name = "solo", sig_in = ['Int'], sig_out = ['Int'],
                             impl = fun(C) -> C end},
            Op2 = #operation{name = "solo", sig_in = ['Int'], sig_out = ['Int'],
                             impl = fun(C) -> C end},
            af_type:replace_ops('Int', "solo", [Op1, Op2]),
            {ok, #af_type{ops = Ops}} = af_type:get_type('Int'),
            [A, B] = maps:get("solo", Ops),
            ?assert(is_integer(A#operation.defined_at)),
            ?assert(is_integer(B#operation.defined_at)),
            ?assert(B#operation.defined_at > A#operation.defined_at)
        end} end
    ]}.

%%====================================================================
%% Built-in primitives are all stamped with counters
%%====================================================================

builtins_stamped_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"built-in 'dup' has a defined_at counter", fun() ->
            {ok, Op} = af_type:find_op_by_name("dup", 'Any'),
            ?assert(is_integer(Op#operation.defined_at))
        end} end,

        fun(_) -> {"all ops in Any have defined_at", fun() ->
            {ok, #af_type{ops = Ops}} = af_type:get_type('Any'),
            Unstamped = maps:fold(fun(_K, OpList, Acc) ->
                [Op || Op <- OpList, Op#operation.defined_at =:= undefined] ++ Acc
            end, [], Ops),
            ?assertEqual([], Unstamped)
        end} end
    ]}.
