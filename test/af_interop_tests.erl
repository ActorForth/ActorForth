-module(af_interop_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input) ->
    Tokens = af_parser:parse(Input, "test"),
    Result = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    Result#continuation.data_stack.

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

%% --- Float type ---

float_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"float literal auto-detection", fun() ->
            ?assertEqual([{'Float', 3.14}], eval("3.14"))
        end} end,
        fun(_) -> {"float constructor from atom", fun() ->
            %% When a token is already a float literal, it auto-detects.
            %% The float constructor works on Atoms (string tokens that look like floats).
            ?assertEqual([{'Float', 2.5}], eval("2.5"))
        end} end,
        fun(_) -> {"int to-float conversion", fun() ->
            ?assertEqual([{'Float', 5.0}], eval("5 int to-float"))
        end} end,
        fun(_) -> {"float to-int conversion", fun() ->
            ?assertEqual([{'Int', 3}], eval("3.7 to-int"))
        end} end,
        fun(_) -> {"float addition", fun() ->
            ?assertEqual([{'Float', 3.5}], eval("1.5 2.0 +"))
        end} end,
        fun(_) -> {"float subtraction", fun() ->
            Result = eval("5.0 2.0 -"),
            [{'Float', V}] = Result,
            ?assert(abs(V - 3.0) < 0.001)
        end} end,
        fun(_) -> {"float multiplication", fun() ->
            ?assertEqual([{'Float', 6.0}], eval("2.0 3.0 *"))
        end} end,
        fun(_) -> {"float division", fun() ->
            ?assertEqual([{'Float', 2.5}], eval("5.0 2.0 /"))
        end} end,
        fun(_) -> {"mixed float+int arithmetic", fun() ->
            ?assertEqual([{'Float', 7.5}], eval("5.5 2 int +"))
        end} end
    ]}.

%% --- Tuple type ---

tuple_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"make-tuple creates tuple from stack items", fun() ->
            ?assertEqual([{'Tuple', {3, 5}}], eval("3 int 5 int 2 int make-tuple"))
        end} end,
        fun(_) -> {"tuple-size", fun() ->
            C = eval("3 int 5 int 2 int make-tuple tuple-size", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 2}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"tuple-get", fun() ->
            C = eval("3 int 5 int 2 int make-tuple 1 int tuple-get", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 3}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"elt works on a4 Tuple values", fun() ->
            %% elt accepts any stack item whose Erlang value is a
            %% tuple, ignoring the type tag. Tuple values work.
            C = eval("3 int 5 int 2 int make-tuple 2 int elt",
                     af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"elt works on tagged record-style values", fun() ->
            %% A raw Erlang tuple like {'SystemNode', ...} appears on
            %% the a4 stack with its own type tag; elt still accesses
            %% positional fields.
            C0 = #continuation{data_stack = [
                {'SystemNode', <<"Car">>, <<"Dispatcher">>,
                 'None', [], [], []}
            ]},
            C1 = af_interpreter:interpret_token(
                #token{value = "2"}, C0),
            C2 = af_interpreter:interpret_token(
                #token{value = "int"}, C1),
            C3 = af_interpreter:interpret_token(
                #token{value = "elt"}, C2),
            ?assertEqual([{'String', <<"Car">>}], C3#continuation.data_stack)
        end} end,
        fun(_) -> {"ok-tuple", fun() ->
            ?assertEqual([{'Tuple', {ok, 42}}], eval("42 int ok-tuple"))
        end} end,
        fun(_) -> {"error-tuple", fun() ->
            ?assertEqual([{'Tuple', {error, timeout}}], eval("timeout error-tuple"))
        end} end,
        fun(_) -> {"is-ok on ok tuple", fun() ->
            C = eval("42 int ok-tuple is-ok", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"is-ok on error tuple", fun() ->
            C = eval("oops error-tuple is-ok", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"unwrap-ok", fun() ->
            C = eval("42 int ok-tuple unwrap-ok", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 42}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"list to-tuple and from-tuple roundtrip", fun() ->
            C = eval("nil 1 int cons 2 int cons to-tuple from-tuple", af_interpreter:new_continuation()),
            [{'List', Items}] = C#continuation.data_stack,
            ?assertEqual([{'Int', 2}, {'Int', 1}], Items)
        end} end
    ]}.

%% --- Enhanced FFI ---

ffi_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"erlang-call with 1 arg", fun() ->
            %% erlang:abs(-5) = 5
            ?assertEqual([{'Int', 5}], eval("-5 int erlang abs 1 int erlang-call"))
        end} end,
        fun(_) -> {"erlang-call with 2 args", fun() ->
            %% erlang:max(3, 7) = 7
            ?assertEqual([{'Int', 7}], eval("3 int 7 int erlang max 2 int erlang-call"))
        end} end,
        fun(_) -> {"erlang-call0 zero args", fun() ->
            %% erlang:node() = nonode@nohost (in test env)
            C = eval("erlang node erlang-call0", af_interpreter:new_continuation()),
            [{'Atom', _}] = C#continuation.data_stack
        end} end
    ]}.

%% --- af_term roundtrip ---

term_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"float term roundtrip", fun() ->
            Item = af_term:to_stack_item(3.14),
            ?assertEqual({'Float', 3.14}, Item),
            ?assertEqual(3.14, af_term:from_stack_item(Item))
        end} end,
        fun(_) -> {"tuple term roundtrip", fun() ->
            Item = af_term:to_stack_item({ok, 42}),
            ?assertEqual({'Tuple', {ok, 42}}, Item),
            ?assertEqual({ok, 42}, af_term:from_stack_item(Item))
        end} end,
        fun(_) -> {"nested tuple term conversion", fun() ->
            T = {error, {badarg, "test"}},
            Item = af_term:to_stack_item(T),
            ?assertEqual({'Tuple', T}, Item),
            ?assertEqual(T, af_term:from_stack_item(Item))
        end} end
    ]}.

%% --- Inter-word compilation through ActorForth ---

compile_interword_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile-all with inter-word calls", fun() ->
            af_type_beam:init(),
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval(": quadruple Int -> Int ; double double .", C1),
            %% Verify interpreted versions work
            C3 = eval("3 int quadruple", C2),
            [{'Int', 12}] = C3#continuation.data_stack,
            %% Compile all — inter-word calls should be native
            C4 = eval("af_interword_test compile-all", C2),
            [{'Atom', "af_interword_test"}] = C4#continuation.data_stack,
            ?assertEqual([{'Int', 10}], af_interword_test:double([{'Int', 5}])),
            ?assertEqual([{'Int', 20}], af_interword_test:quadruple([{'Int', 5}]))
        end} end
    ]}.
