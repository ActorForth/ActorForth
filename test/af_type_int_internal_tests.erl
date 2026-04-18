-module(af_type_int_internal_tests).

%% Tests that exercise af_type_int's op_int Erlang function directly by
%% manually placing an {'Atom', Str} on the stack. The a4 literal
%% handler auto-converts numeric strings to Int, so these paths aren't
%% reachable from normal .a4 source — they must be tested here.
%%
%% Behavioral int tests live in lib/testing/int_ops.test.a4.

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

setup() ->
    af_type:reset().

op_int_path_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"int constructor from numeric string atom (op_int path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "99"}]},
            Tokens = af_parser:parse("int", "test"),
            C = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Int', 99}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"int passthrough on already-Int value directly", fun() ->
            Cont = #continuation{data_stack = [{'Int', 42}]},
            Tokens = af_parser:parse("int", "test"),
            C = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Int', 42}], C#continuation.data_stack)
        end} end
    ]}.
