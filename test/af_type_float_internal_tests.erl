-module(af_type_float_internal_tests).

%% Tests exercising af_type_float's op_float directly.
%% Behavioral float tests live in lib/testing/float_ops.test.a4.

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

setup() ->
    af_type:reset().

op_float_path_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"float constructor from Atom directly (op_float path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "3.14"}]},
            C = af_interpreter:interpret_tokens(af_parser:parse("float", "test"),
                                                  Cont),
            ?assertMatch([{'Float', _}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"float constructor with invalid value errors", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "notanumber"}]},
            ?assertError({type_error, {cannot_convert_to_float, _}},
                af_interpreter:interpret_tokens(af_parser:parse("float", "test"),
                                                  Cont))
        end} end
    ]}.
