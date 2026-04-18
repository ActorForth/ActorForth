-module(af_type_bool_internal_tests).

%% Tests exercising af_type_bool's op_bool function directly by
%% manually placing {'Atom', "true"|"True"|"false"|"False"} on the
%% stack. The a4 literal handler auto-converts these tokens to Bool
%% before the constructor fires, so these paths aren't reachable
%% from normal .a4 source.
%%
%% Behavioral bool tests live in lib/testing/bool_ops.test.a4.

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

setup() ->
    af_type:reset().

op_bool_path_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"bool constructor from Atom 'true' (op_bool path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "true"}]},
            C = af_interpreter:interpret_tokens(af_parser:parse("bool", "test"), Cont),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"bool constructor from Atom 'True' (op_bool path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "True"}]},
            C = af_interpreter:interpret_tokens(af_parser:parse("bool", "test"), Cont),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"bool constructor from Atom 'false' (op_bool path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "false"}]},
            C = af_interpreter:interpret_tokens(af_parser:parse("bool", "test"), Cont),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"bool constructor from Atom 'False' (op_bool path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "False"}]},
            C = af_interpreter:interpret_tokens(af_parser:parse("bool", "test"), Cont),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end
    ]}.
