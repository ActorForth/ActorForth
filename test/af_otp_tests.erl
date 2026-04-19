-module(af_otp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

%% --- gen-server-module ---

gen_server_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile gen_server from product type", fun() ->
            %% Define a Counter product type with words
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": increment Counter -> Counter ; value 1 + value! .", C1),
            C3 = eval(": count Counter -> Counter Int ; value .", C2),
            %% Compile to gen_server module
            C4 = eval("af_gs_counter Counter gen-server-module", C3),
            [{'Atom', "af_gs_counter"}] = C4#continuation.data_stack,
            %% Start with a tagged product type instance as state
            InitState = {'Counter', 0},
            {ok, Pid} = gen_server:start_link(af_gs_counter, InitState, []),
            %% Cast: increment (async)
            ok = gen_server:cast(Pid, increment),
            ok = gen_server:cast(Pid, increment),
            ok = gen_server:cast(Pid, increment),
            timer:sleep(50),
            %% Call: count (sync)
            {ok, Count} = gen_server:call(Pid, count),
            ?assertEqual(3, Count),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"gen_server handles unknown calls gracefully", fun() ->
            C1 = eval("type Simple value Int .", af_interpreter:new_continuation()),
            C2 = eval(": get Simple -> Simple Int ; value .", C1),
            C3 = eval("af_gs_simple Simple gen-server-module", C2),
            [{'Atom', "af_gs_simple"}] = C3#continuation.data_stack,
            InitState = {'Simple', 42},
            {ok, Pid} = gen_server:start_link(af_gs_simple, InitState, []),
            ?assertEqual({error, unknown_call}, gen_server:call(Pid, nonexistent)),
            gen_server:stop(Pid)
        end} end
    ]}.

%% --- af_otp_dispatch ---

dispatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"dispatch call_word returns values", fun() ->
            C1 = eval("type Box value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": get-value Box -> Box Int ; value .", C1),
            State = {'Box', 99},
            {ReturnValues, _NewState} = af_otp_dispatch:call_word("get-value", [], State),
            ?assertEqual([99], ReturnValues)
        end} end,
        fun(_) -> {"dispatch cast_word modifies state", fun() ->
            C1 = eval("type Box value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": set-42 Box -> Box ; 42 value! .", C1),
            State = {'Box', 0},
            NewState = af_otp_dispatch:cast_word("set-42", [], State),
            ?assertEqual('Box', element(1, NewState)),
            ?assertEqual(42, element(2, NewState))
        end} end
    ]}.

%% --- Error paths ---

error_path_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile_gen_server unknown type returns error", fun() ->
            Cont = af_interpreter:new_continuation(),
            Result = af_type_otp:compile_gen_server(af_gs_nonexistent, 'Nonexistent', Cont),
            ?assertEqual({error, {unknown_type, 'Nonexistent'}}, Result)
        end} end,

        fun(_) -> {"compile_gen_server type with no compiled words returns error", fun() ->
            C1 = eval("type Empty value Int .", af_interpreter:new_continuation()),
            Result = af_type_otp:compile_gen_server(af_gs_empty, 'Empty', C1),
            ?assertEqual({error, {no_words_for_type, 'Empty'}}, Result)
        end} end,

        fun(_) -> {"gen_server with cast-only words (no return values)", fun() ->
            C1 = eval("type Acc value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": reset Acc -> Acc ; 0 value! .", C1),
            C3 = eval("af_gs_acc Acc gen-server-module", _C2),
            [{'Atom', "af_gs_acc"}] = C3#continuation.data_stack,
            InitState = {'Acc', 10},
            {ok, Pid} = gen_server:start_link(af_gs_acc, InitState, []),
            ok = gen_server:cast(Pid, reset),
            timer:sleep(50),
            %% Unknown call should return error
            ?assertEqual({error, unknown_call}, gen_server:call(Pid, get_value)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"gen_server with multi-return call", fun() ->
            C1 = eval("type Pair a Int b Int .", af_interpreter:new_continuation()),
            %% Auto-field-binding: `a` and `b` push copies of the Pair's
            %% fields on top of the stack (the Pair stays below). Two
            %% reads produce [Pair, Int_a, Int_b] (TOS-first order).
            _C2 = eval(": get-both Pair -> Pair Int Int ; a b .", C1),
            C3 = eval("af_gs_pair Pair gen-server-module", _C2),
            [{'Atom', "af_gs_pair"}] = C3#continuation.data_stack,
            InitState = {'Pair', 1, 2},
            {ok, Pid} = gen_server:start_link(af_gs_pair, InitState, []),
            {ok, Result} = gen_server:call(Pid, 'get-both'),
            %% Multi-return: result is list of raw values
            ?assert(is_list(Result)),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"compile_gen_server directly", fun() ->
            C1 = eval("type Calc total Int .", af_interpreter:new_continuation()),
            _C2 = eval(": get-total Calc -> Calc Int ; total .", C1),
            Cont = af_interpreter:new_continuation(),
            Result = af_type_otp:compile_gen_server(af_gs_calc, 'Calc', Cont),
            ?assertMatch({ok, af_gs_calc}, Result)
        end} end,

        fun(_) -> {"gen_server with native compiled words", fun() ->
            C1 = eval("type NCounter val Int .", af_interpreter:new_continuation()),
            C2 = eval(": get-val NCounter -> NCounter Int ; val .", C1),
            %% Compile to native BEAM first
            C3 = eval("\"get-val\" compile", C2),
            C4 = eval("af_gs_ncounter NCounter gen-server-module", C3),
            [{'Atom', "af_gs_ncounter"}] = C4#continuation.data_stack,
            InitState = {'NCounter', 77},
            {ok, Pid} = gen_server:start_link(af_gs_ncounter, InitState, []),
            {ok, Val} = gen_server:call(Pid, 'get-val'),
            ?assertEqual(77, Val),
            gen_server:stop(Pid)
        end} end
    ]}.
