-module(af_python_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_repl:init_types(),
    af_type_python:ensure_started().

%% --- Basic Python operations ---

py_eval_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-eval evaluates Python expression", fun() ->
            C1 = eval("\"2 + 2\" py-eval", af_interpreter:new_continuation()),
            [{'Int', 4}] = C1#continuation.data_stack
        end} end,

        fun(_) -> {"py-eval with string result", fun() ->
            C1 = eval("\"'hello' + ' ' + 'world'\" py-eval", af_interpreter:new_continuation()),
            [{'String', "hello world"}] = C1#continuation.data_stack
        end} end,

        fun(_) -> {"py-eval with float result", fun() ->
            C1 = eval("\"3.14 * 2\" py-eval", af_interpreter:new_continuation()),
            [{'Float', Val}] = C1#continuation.data_stack,
            ?assert(abs(Val - 6.28) < 0.001)
        end} end,

        fun(_) -> {"py-eval with boolean result", fun() ->
            C1 = eval("\"10 > 5\" py-eval", af_interpreter:new_continuation()),
            [{'Bool', true}] = C1#continuation.data_stack
        end} end,

        fun(_) -> {"py-eval with list result", fun() ->
            C1 = eval("\"list(range(4))\" py-eval", af_interpreter:new_continuation()),
            [{'List', Items}] = C1#continuation.data_stack,
            ?assertEqual(4, length(Items))
        end} end
    ]}.

%% --- py-call ---

py_call_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-call calls function with args", fun() ->
            C1 = eval("16 math sqrt 1 py-call", af_interpreter:new_continuation()),
            [{'Float', 4.0}] = C1#continuation.data_stack
        end} end,

        fun(_) -> {"py-call with multiple args", fun() ->
            C1 = eval("2 8 math pow 2 py-call", af_interpreter:new_continuation()),
            [{'Float', 256.0}] = C1#continuation.data_stack
        end} end,

        fun(_) -> {"py-call0 calls zero-arg function", fun() ->
            C1 = eval("random random py-call0", af_interpreter:new_continuation()),
            [{'Float', Val}] = C1#continuation.data_stack,
            ?assert(Val >= 0.0 andalso Val < 1.0)
        end} end,

        fun(_) -> {"py-eval with math.pi", fun() ->
            C1 = eval("\"__import__('math').pi\" py-eval", af_interpreter:new_continuation()),
            [{'Float', Pi}] = C1#continuation.data_stack,
            ?assert(abs(Pi - 3.14159) < 0.001)
        end} end
    ]}.

%% --- py-exec ---

py_exec_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-exec runs Python statements", fun() ->
            C1 = eval("\"import sys\" py-exec", af_interpreter:new_continuation()),
            [] = C1#continuation.data_stack
        end} end
    ]}.

%% --- py-import and custom modules ---

py_import_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-import adds to sys.path and enables module calls", fun() ->
            C1 = eval("\"samples/python\" py-import", af_interpreter:new_continuation()),
            [] = C1#continuation.data_stack,
            C2 = eval("\"hello world\" text_tools word_count 1 py-call", C1),
            [{'Int', 2}] = C2#continuation.data_stack
        end} end,

        fun(_) -> {"call Python text_tools.slugify", fun() ->
            C1 = eval("\"samples/python\" py-import", af_interpreter:new_continuation()),
            C2 = eval("\"Hello World!\" text_tools slugify 1 py-call", C1),
            [{'String', "hello-world"}] = C2#continuation.data_stack
        end} end,

        fun(_) -> {"call Python text_tools.reverse_words", fun() ->
            C1 = eval("\"samples/python\" py-import", af_interpreter:new_continuation()),
            C2 = eval("\"a b c\" text_tools reverse_words 1 py-call", C1),
            [{'String', "c b a"}] = C2#continuation.data_stack
        end} end,

        fun(_) -> {"call Python text_tools.levenshtein", fun() ->
            C1 = eval("\"samples/python\" py-import", af_interpreter:new_continuation()),
            C2 = eval("\"kitten\" \"sitting\" text_tools levenshtein 2 py-call", C1),
            [{'Int', 3}] = C2#continuation.data_stack
        end} end
    ]}.

%% --- AI stub module ---

ai_stub_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"ai_stub.embed_text returns list", fun() ->
            C1 = eval("\"samples/python\" py-import", af_interpreter:new_continuation()),
            C2 = eval("\"hello\" ai_stub embed_text 1 py-call", C1),
            [{'List', Embedding}] = C2#continuation.data_stack,
            ?assertEqual(8, length(Embedding))
        end} end,

        fun(_) -> {"ai_stub.from_json parses JSON", fun() ->
            C1 = eval("\"samples/python\" py-import", af_interpreter:new_continuation()),
            C2 = eval("\"[1,2,3]\" ai_stub from_json 1 py-call", C1),
            [{'List', Items}] = C2#continuation.data_stack,
            ?assertEqual(3, length(Items))
        end} end,

        fun(_) -> {"ai_stub.cosine_similarity computes similarity", fun() ->
            C1 = eval("\"samples/python\" py-import", af_interpreter:new_continuation()),
            %% Same text should have similarity 1.0
            C2 = eval("\"hello\" ai_stub embed_text 1 py-call", C1),
            C3 = eval("\"hello\" ai_stub embed_text 1 py-call", C2),
            %% Two embeddings on stack — call cosine_similarity via eval
            %% since we need to pass lists from the AF stack to Python
            %% Use py:eval directly from Erlang for this
            {ok, Sim} = py:eval(<<"__import__('ai_stub').cosine_similarity([1,0,0],[1,0,0])">>),
            ?assertEqual(1.0, Sim)
        end} end
    ]}.

%% --- py-register (expose ActorForth words to Python) ---

py_register_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"py-register exposes compiled word to Python", fun() ->
            %% Define and compile a word
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval("double af_py_test_dbl compile-to-beam", C1),
            [{'Atom', "af_py_test_dbl"}] = C2#continuation.data_stack,
            %% Register it for Python
            C3 = eval("double py-register", C2),
            %% Call from Python via py:eval directly
            {ok, Result} = py:eval(<<"erlang.double(21)">>),
            ?assertEqual(42, Result),
            %% Also test via ActorForth py-eval
            C4 = eval("\"erlang.double(7)\" py-eval", C3),
            [{'Int', 14} | _] = C4#continuation.data_stack
        end} end
    ]}.

%% --- Actor bridge pattern (module-level state) ---

actor_bridge_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"Python worker module: store and fetch via py-call", fun() ->
            C0 = af_interpreter:new_continuation(),
            %% Import the worker module
            C1 = eval("\"samples/python\" py-import", C0),
            %% Clear any prior state
            C2 = eval("worker clear 0 py-call drop", C1),
            %% Store a value
            C3 = eval("\"name\" \"ActorForth\" worker store 2 py-call drop", C2),
            %% Fetch it back
            C4 = eval("\"name\" worker fetch 1 py-call", C3),
            [{'String', "ActorForth"}] = C4#continuation.data_stack,
            %% Check count
            C5 = eval("drop worker count 0 py-call", C4),
            [{'Int', 1}] = C5#continuation.data_stack,
            %% Clean up
            eval("drop worker clear 0 py-call drop", C5)
        end} end,

        fun(_) -> {"Python worker with py-register round-trip", fun() ->
            %% Define and compile an AF word, register it for Python
            C1 = eval(": triple Int -> Int ; dup dup + + .", af_interpreter:new_continuation()),
            C2 = eval("triple af_py_trip compile-to-beam", C1),
            C3 = eval("triple py-register", C2),
            %% Register a Python function that uses our AF word
            py:register_function(py_use_triple, fun([X]) ->
                {ok, R} = py:eval(iolist_to_binary(
                    io_lib:format("erlang.triple(~p)", [X]))),
                R
            end),
            %% Call from AF -> Python -> AF word -> result
            C4 = eval("\"erlang.py_use_triple(5)\" py-eval", C3),
            [{'Int', 15} | _] = C4#continuation.data_stack,
            py:unregister_function(py_use_triple)
        end} end
    ]}.
