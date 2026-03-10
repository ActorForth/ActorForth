-module(af_type_float_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

setup() ->
    af_type:reset().

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

%% --- Float constructor (explicit) ---

float_constructor_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"float literal auto-detection", fun() ->
            C = eval("3.14", af_interpreter:new_continuation()),
            ?assertMatch([{'Float', _}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"float constructor from atom - use op directly", fun() ->
            %% Build a continuation with an Atom that looks like a float
            Cont = af_interpreter:new_continuation(),
            Cont1 = Cont#continuation{data_stack = [{'Atom', "3.14"}]},
            %% Call float constructor via eval
            C = eval("float", Cont1),
            ?assertMatch([{'Float', _}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"float constructor with invalid value errors", fun() ->
            Cont = af_interpreter:new_continuation(),
            Cont1 = Cont#continuation{data_stack = [{'Atom', "notanumber"}]},
            ?assertError({type_error, {cannot_convert_to_float, _}},
                eval("float", Cont1))
        end} end
    ]}.

%% --- Mixed arithmetic: Int on TOS, Float below ---
%% These test the Any-dict ops with sig_in ['Int', 'Float']

mixed_int_float_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"float + int (Int on TOS)", fun() ->
            C = eval("2.5 3 +", af_interpreter:new_continuation()),
            [{'Float', R}] = C#continuation.data_stack,
            ?assertEqual(5.5, R)
        end} end,
        fun(_) -> {"float * int (Int on TOS)", fun() ->
            C = eval("2.5 3 *", af_interpreter:new_continuation()),
            [{'Float', R}] = C#continuation.data_stack,
            ?assertEqual(7.5, R)
        end} end,
        fun(_) -> {"float - int (Int on TOS)", fun() ->
            C = eval("10.0 3 -", af_interpreter:new_continuation()),
            [{'Float', R}] = C#continuation.data_stack,
            ?assertEqual(7.0, R)
        end} end,
        fun(_) -> {"float / int (Int on TOS)", fun() ->
            C = eval("10.0 2 /", af_interpreter:new_continuation()),
            [{'Float', R}] = C#continuation.data_stack,
            ?assertEqual(5.0, R)
        end} end
    ]}.

%% --- Mixed arithmetic: Float on TOS, Int below ---
%% These test the Float-dict ops with sig_in ['Float', 'Int']

mixed_float_int_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"int + float (Float on TOS)", fun() ->
            C = eval("3 2.5 +", af_interpreter:new_continuation()),
            [{'Float', R}] = C#continuation.data_stack,
            ?assertEqual(5.5, R)
        end} end,
        fun(_) -> {"int * float (Float on TOS)", fun() ->
            C = eval("3 2.5 *", af_interpreter:new_continuation()),
            [{'Float', R}] = C#continuation.data_stack,
            ?assertEqual(7.5, R)
        end} end,
        fun(_) -> {"int - float (Float on TOS)", fun() ->
            C = eval("10 3.0 -", af_interpreter:new_continuation()),
            [{'Float', R}] = C#continuation.data_stack,
            ?assertEqual(7.0, R)
        end} end,
        fun(_) -> {"int / float (Float on TOS)", fun() ->
            C = eval("10 2.0 /", af_interpreter:new_continuation()),
            [{'Float', R}] = C#continuation.data_stack,
            ?assertEqual(5.0, R)
        end} end
    ]}.

%% --- to-string ---

to_string_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"float to-string", fun() ->
            C = eval("3.14 to-string", af_interpreter:new_continuation()),
            [{'String', S}] = C#continuation.data_stack,
            ?assert(is_binary(S))
        end} end
    ]}.
