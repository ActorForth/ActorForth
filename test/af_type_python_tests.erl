-module(af_type_python_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_float:init(),
    af_type_string:init(),
    af_type_list:init(),
    af_type_atom:init(),
    af_type_tuple:init(),
    af_type_compiler:init(),
    af_type_python:init(),
    application:ensure_all_started(erlang_python),
    ok.

%% --- py-start ---

py_start_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-start succeeds", fun() ->
            C = eval("py-start", af_interpreter:new_continuation()),
            ?assertEqual([], C#continuation.data_stack)
        end} end
    ]}.

%% --- py-eval (covers py_to_stack_item for all types) ---

py_eval_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-eval integer", fun() ->
            C = eval("\"2 + 3\" py-eval", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-eval float", fun() ->
            C = eval("\"1.5 + 2.5\" py-eval", af_interpreter:new_continuation()),
            ?assertEqual([{'Float', 4.0}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-eval string", fun() ->
            C = eval("\"'hello'\" py-eval", af_interpreter:new_continuation()),
            ?assertEqual([{'String', "hello"}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-eval True", fun() ->
            C = eval("\"True\" py-eval", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-eval False", fun() ->
            C = eval("\"False\" py-eval", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-eval None", fun() ->
            C = eval("\"None\" py-eval", af_interpreter:new_continuation()),
            ?assertEqual([{'Atom', "none"}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-eval list", fun() ->
            C = eval("\"[1, 2, 3]\" py-eval", af_interpreter:new_continuation()),
            [{'List', Items}] = C#continuation.data_stack,
            ?assertEqual([{'Int', 1}, {'Int', 2}, {'Int', 3}], Items)
        end} end,
        fun(_) -> {"py-eval dict", fun() ->
            C = eval("\"{'a': 1}\" py-eval", af_interpreter:new_continuation()),
            [{'List', Items}] = C#continuation.data_stack,
            ?assertEqual(1, length(Items)),
            {'Tuple', {{'String', "a"}, {'Int', 1}}} = hd(Items)
        end} end,
        fun(_) -> {"py-eval tuple", fun() ->
            C = eval("\"(1, 2, 3)\" py-eval", af_interpreter:new_continuation()),
            [{'Tuple', Val}] = C#continuation.data_stack,
            ?assert(is_tuple(Val))
        end} end,
        fun(_) -> {"py-eval error raises", fun() ->
            ?assertError({af_error, python_error, _, _, _, _},
                eval("\"1/0\" py-eval", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- py-exec ---

py_exec_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-exec runs statement without error", fun() ->
            C = eval("\"x = 42\" py-exec", af_interpreter:new_continuation()),
            ?assertEqual([], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-exec error raises", fun() ->
            ?assertError({af_error, python_error, _, _, _, _},
                eval("\"raise ValueError('test')\" py-exec", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- py-call ---

py_call_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-call with 1 int arg (builtins.abs)", fun() ->
            C = eval("-42 int builtins abs 1 int py-call", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 42}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-call with 2 args (builtins.max)", fun() ->
            C = eval("3 int 7 int builtins max 2 int py-call", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 7}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-call with string arg (builtins.len)", fun() ->
            C = eval("\"hello\" builtins len 1 int py-call", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-call stack underflow raises error", fun() ->
            ?assertError({af_error, stack_underflow, _, _, _, _},
                eval("builtins abs 3 int py-call", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"py-call error raises python_error", fun() ->
            ?assertError({af_error, python_error, _, _, _, _},
                eval("42 int builtins nonexistent_fn_xyz 1 int py-call", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- py-call with various arg types via direct stack construction ---

py_call_arg_types_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-call with float arg", fun() ->
            %% builtins.float(3.5) -> 3.5 (Float passes through)
            Stack = [{'Int', 1}, {'Atom', "float"}, {'Atom', "builtins"}, {'Float', 3.5}],
            C = #continuation{data_stack = Stack},
            Token = #token{value = "py-call"},
            C2 = af_interpreter:interpret_token(Token, C),
            [{'Float', V}] = C2#continuation.data_stack,
            ?assert(abs(V - 3.5) < 0.001)
        end} end,
        fun(_) -> {"py-call with bool arg", fun() ->
            %% builtins.int(True) -> 1
            Stack = [{'Int', 1}, {'Atom', "int"}, {'Atom', "builtins"}, {'Bool', true}],
            C = #continuation{data_stack = Stack},
            Token = #token{value = "py-call"},
            C2 = af_interpreter:interpret_token(Token, C),
            ?assertEqual([{'Int', 1}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"py-call with list arg (builtins.len)", fun() ->
            Stack = [{'Int', 1}, {'Atom', "len"}, {'Atom', "builtins"},
                     {'List', [{'Int', 1}, {'Int', 2}, {'Int', 3}]}],
            C = #continuation{data_stack = Stack},
            Token = #token{value = "py-call"},
            C2 = af_interpreter:interpret_token(Token, C),
            ?assertEqual([{'Int', 3}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"py-call with Atom arg (builtins.len on string)", fun() ->
            Stack = [{'Int', 1}, {'Atom', "len"}, {'Atom', "builtins"}, {'Atom', "test"}],
            C = #continuation{data_stack = Stack},
            Token = #token{value = "py-call"},
            C2 = af_interpreter:interpret_token(Token, C),
            ?assertEqual([{'Int', 4}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"py-call with Tuple arg (builtins.len on tuple)", fun() ->
            Stack = [{'Int', 1}, {'Atom', "len"}, {'Atom', "builtins"}, {'Tuple', {1, 2, 3}}],
            C = #continuation{data_stack = Stack},
            Token = #token{value = "py-call"},
            C2 = af_interpreter:interpret_token(Token, C),
            ?assertEqual([{'Int', 3}], C2#continuation.data_stack)
        end} end,
        fun(_) -> {"py-call with custom type arg", fun() ->
            %% Custom type — af_to_python extracts just the value
            Stack = [{'Int', 1}, {'Atom', "type"}, {'Atom', "builtins"}, {'Custom', 42}],
            C = #continuation{data_stack = Stack},
            Token = #token{value = "py-call"},
            C2 = af_interpreter:interpret_token(Token, C),
            %% builtins.type(42) returns <class 'int'> which comes back as a string
            ?assertEqual(1, length(C2#continuation.data_stack))
        end} end
    ]}.

%% --- py-call0 ---

py_call0_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-call0 calls zero-arg function", fun() ->
            %% os.getcwd() returns a string
            Stack = [{'Atom', "getcwd"}, {'Atom', "os"}],
            C = #continuation{data_stack = Stack},
            Token = #token{value = "py-call0"},
            C2 = af_interpreter:interpret_token(Token, C),
            [{'String', Cwd}] = C2#continuation.data_stack,
            ?assert(is_list(Cwd))
        end} end,
        fun(_) -> {"py-call0 error raises python_error", fun() ->
            ?assertError({af_error, python_error, _, _, _, _},
                eval("nonexistent_module nonexistent_func py-call0", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- py-import ---

py_import_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-import adds path to sys.path", fun() ->
            C = eval("\"/tmp\" py-import", af_interpreter:new_continuation()),
            ?assertEqual([], C#continuation.data_stack)
        end} end,
        fun(_) -> {"py-import with list string (covers to_list/1 list clause)", fun() ->
            %% Construct stack with String value as list (not binary)
            Stack = [{'String', "/tmp"}],
            C = #continuation{data_stack = Stack},
            Token = #token{value = "py-import"},
            C2 = af_interpreter:interpret_token(Token, C),
            ?assertEqual([], C2#continuation.data_stack)
        end} end
    ]}.

%% --- py-venv ---

py_venv_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-venv with nonexistent path errors", fun() ->
            ?assertError(_,
                eval("\"/nonexistent/venv\" py-venv", af_interpreter:new_continuation()))
        end} end
    ]}.

%% --- py-register ---

py_register_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-register with interpreted word and call back", fun() ->
            C1 = eval(": triple Int -> Int ; dup dup + + .", af_interpreter:new_continuation()),
            C2 = eval("triple py-register", C1),
            ?assertEqual([{'Atom', "triple"}], C2#continuation.data_stack),
            %% Call the registered word back from Python (covers line 197)
            {ok, Result} = py:call(<<"erlang">>, triple, [5]),
            ?assertEqual(15, Result)
        end} end,
        fun(_) -> {"py-register not-found word raises error", fun() ->
            ?assertError(_,
                eval("nonexistent_word_xyz py-register", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"py-register with compiled native word and call back", fun() ->
            af_type_beam:init(),
            C1 = eval(": dbl Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval("\"dbl\" compile", C1),
            ?assertEqual([], C2#continuation.data_stack),
            C3 = eval("dbl py-register", C2),
            ?assertEqual([{'Atom', "dbl"}], C3#continuation.data_stack),
            %% Call the native word back from Python (covers lines 183-186)
            {ok, Result} = py:call(<<"erlang">>, dbl, [7]),
            ?assertEqual(14, Result)
        end} end,
        fun(_) -> {"py-register native word returning empty stack", fun() ->
            af_type_beam:init(),
            C1 = eval(": noop Int -> ; drop .", af_interpreter:new_continuation()),
            C2 = eval("\"noop\" compile", C1),
            C3 = eval("noop py-register", C2),
            ?assertEqual([{'Atom', "noop"}], C3#continuation.data_stack),
            %% Call back — empty result returns ok (covers line 187)
            %% Python may convert the atom 'ok' to binary <<"ok">>
            {ok, Result} = py:call(<<"erlang">>, noop, [42]),
            ?assert(Result =:= ok orelse Result =:= <<"ok">>)
        end} end
    ]}.

%% --- ensure_started ---

ensure_started_test_() ->
    {setup, fun setup/0, [
        {"ensure_started succeeds when python available", fun() ->
            ?assertEqual(ok, af_type_python:ensure_started())
        end}
    ]}.

%% --- py_to_stack_item direct tests ---

py_to_stack_item_test_() ->
    {setup, fun setup/0, [
        {"py_to_stack_item integer", fun() ->
            ?assertEqual({'Int', 42}, af_type_python:py_to_stack_item(42))
        end},
        {"py_to_stack_item float", fun() ->
            ?assertEqual({'Float', 3.14}, af_type_python:py_to_stack_item(3.14))
        end},
        {"py_to_stack_item true", fun() ->
            ?assertEqual({'Bool', true}, af_type_python:py_to_stack_item(true))
        end},
        {"py_to_stack_item false", fun() ->
            ?assertEqual({'Bool', false}, af_type_python:py_to_stack_item(false))
        end},
        {"py_to_stack_item none", fun() ->
            ?assertEqual({'Atom', "none"}, af_type_python:py_to_stack_item(none))
        end},
        {"py_to_stack_item binary", fun() ->
            ?assertEqual({'String', "hello"}, af_type_python:py_to_stack_item(<<"hello">>))
        end},
        {"py_to_stack_item list", fun() ->
            ?assertEqual({'List', [{'Int', 1}, {'Int', 2}]},
                         af_type_python:py_to_stack_item([1, 2]))
        end},
        {"py_to_stack_item map (dict)", fun() ->
            Result = af_type_python:py_to_stack_item(#{<<"k">> => 1}),
            ?assertMatch({'List', [{'Tuple', {{'String', "k"}, {'Int', 1}}}]}, Result)
        end},
        {"py_to_stack_item tuple", fun() ->
            ?assertEqual({'Tuple', {1, 2}}, af_type_python:py_to_stack_item({1, 2}))
        end},
        {"py_to_stack_item unknown (atom)", fun() ->
            Result = af_type_python:py_to_stack_item(some_weird_atom),
            ?assertMatch({'Atom', _}, Result)
        end}
    ]}.

%% --- run_word_with_args ---

run_word_with_args_test_() ->
    {setup, fun setup/0, [
        {"run_word_with_args returns result", fun() ->
            %% dup: 42 -> 42, 42.  Returns first item (42).
            Result = af_type_python:run_word_with_args("dup", [42]),
            ?assertEqual(42, Result)
        end},
        {"run_word_with_args empty result returns none", fun() ->
            Result = af_type_python:run_word_with_args("drop", [42]),
            ?assertEqual(none, Result)
        end}
    ]}.
