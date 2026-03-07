-module(af_ffi_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_string:init(),
    af_type_map:init(),
    af_type_list:init(),
    af_type_actor:init(),
    af_type_ffi:init().

%% --- erlang-apply0 (zero-arg calls) ---

apply0_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"call erlang:node/0", fun() ->
            C = eval("node erlang erlang-apply0", af_interpreter:new_continuation()),
            [{'Atom', NodeStr}] = C#continuation.data_stack,
            ?assertEqual(atom_to_list(node()), NodeStr)
        end} end,
        fun(_) -> {"call erlang:system_time/0", fun() ->
            C = eval("system_time erlang erlang-apply0", af_interpreter:new_continuation()),
            [{'Int', T}] = C#continuation.data_stack,
            ?assert(T > 0)
        end} end
    ]}.

%% --- erlang-apply (with args) ---

apply_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"call erlang:abs/1", fun() ->
            C = eval("nil -5 cons abs erlang erlang-apply", af_interpreter:new_continuation()),
            [{'Int', 5}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"call erlang:max/2", fun() ->
            C = eval("nil 10 cons 20 cons max erlang erlang-apply", af_interpreter:new_continuation()),
            [{'Int', 20}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"call lists:reverse/1", fun() ->
            C = eval("nil nil 1 cons 2 cons 3 cons cons reverse lists erlang-apply", af_interpreter:new_continuation()),
            [{'List', Reversed}] = C#continuation.data_stack,
            ?assertEqual([{'Int', 1}, {'Int', 2}, {'Int', 3}], Reversed)
        end} end,
        fun(_) -> {"call erlang:min/2", fun() ->
            C = eval("nil 100 cons 3 cons min erlang erlang-apply", af_interpreter:new_continuation()),
            [{'Int', 3}] = C#continuation.data_stack
        end} end
    ]}.

%% --- error handling ---

error_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"calling nonexistent function raises ffi_error", fun() ->
            ?assertError({af_error, ffi_error, _, _, _, _},
                eval("no_such_func no_such_mod erlang-apply0", af_interpreter:new_continuation()))
        end} end
    ]}.
