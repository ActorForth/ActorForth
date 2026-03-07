# Python Integration: Tech Stack and Mechanisms

*How ActorForth embeds Python in the BEAM VM for AI/ML interop*

---

## Motivation

Python dominates the AI/ML ecosystem. LLM clients (OpenAI, Anthropic), embedding models, vector databases, data science libraries — nearly all ship Python SDKs first. Rather than waiting for Erlang/Elixir equivalents to mature, ActorForth embeds a Python interpreter directly inside the BEAM VM, allowing ActorForth programs to call any Python library as naturally as calling native words.

The goal is not to replace ActorForth's actor model with Python. It is to let ActorForth actors use Python as a library — calling out for AI inference, text processing, or any computation where Python's ecosystem is stronger, while keeping all concurrency, state management, and orchestration in ActorForth on the BEAM.

---

## The Tech Stack

### Layer 1: erlang_python (Hex Package)

The foundation is `erlang_python` version 1.8.1, a hex package that embeds CPython inside the BEAM via dirty NIFs.

```erlang
%% rebar.config
{deps, [
    {erlang_python, "1.8.1"}
]}.
```

**How it works internally:**

- Links against `libpython3.12.so` at compile time
- Runs Python code on BEAM dirty schedulers (dirty NIF threads), so Python's GIL does not block normal BEAM schedulers
- Provides an OTP application (`erlang_python`) with a supervision tree managing Python worker pools
- Uses the Python C API to marshal data between Erlang terms and Python objects

**Core API:**

| Function | Purpose |
|----------|---------|
| `py:call(Module, Function, Args)` | Call a Python function by module and name |
| `py:eval(BinaryExpression)` | Evaluate a Python expression, return result |
| `py:exec(BinaryCode)` | Execute Python statements (no return) |
| `py:register_function(Name, Fun)` | Register an Erlang fun as callable from Python |
| `py:activate_venv(Path)` | Activate a Python virtual environment |

All arguments and return values are Erlang terms. The library handles marshalling: integers, floats, binaries (strings), lists, maps, booleans, and `none` all convert automatically between Python and Erlang representations.

**Requirements:**

- Python 3.12+ built with `--enable-shared` (shared library required for NIF linking)
- OTP 27+
- `LD_LIBRARY_PATH` must include the Python shared lib directory

### Layer 2: af_type_python (ActorForth Bridge)

The bridge between ActorForth's stack-based world and the `erlang_python` API is `src/af_type_python.erl`. It registers eight words in the Any dictionary, making them available regardless of what type is on the stack.

```
py-start         Ensure Python runtime is running
py-call          Call module.function with N args from stack
py-call0         Call module.function() with zero args
py-eval          Evaluate a Python expression string
py-exec          Execute Python statements
py-import        Add directory to Python sys.path
py-venv          Activate a Python virtual environment
py-register      Expose a compiled ActorForth word to Python
```

### Layer 3: Python Modules

User-written Python modules in `samples/python/` provide the actual functionality. These are ordinary Python files with module-level functions and state.

### Layer 4: .env Configuration

The REPL loads `.env` on startup via `af_repl:load_env/0`, setting environment variables like `OPENAI_API_KEY` before Python code runs. Python modules read these via `os.environ`.

---

## How Data Crosses the Boundary

### ActorForth to Python (af_to_python)

When an ActorForth word calls Python, stack items must be unwrapped from their `{Type, Value}` tuples into plain Erlang values that `erlang_python` can marshal to Python objects:

```erlang
af_to_python({'Int', V})    -> V;           %% 42 -> 42
af_to_python({'Float', V})  -> V;           %% 3.14 -> 3.14
af_to_python({'Bool', V})   -> V;           %% true -> True
af_to_python({'String', V}) -> to_bin(V);   %% "hello" -> b"hello" -> str
af_to_python({'Atom', V})   -> to_bin(V);   %% atom -> str
af_to_python({'List', Items}) ->
    [af_to_python(I) || I <- Items];        %% recursive
af_to_python({_Type, V})   -> V.            %% fallback: raw value
```

The `to_bin/1` helper handles the fact that ActorForth strings may be Erlang lists or binaries — `erlang_python` expects binaries for Python strings.

### Python to ActorForth (py_to_stack_item)

Return values from Python must be re-wrapped into typed stack items:

```erlang
py_to_stack_item(V) when is_integer(V) -> {'Int', V};
py_to_stack_item(V) when is_float(V)   -> {'Float', V};
py_to_stack_item(true)                  -> {'Bool', true};
py_to_stack_item(false)                 -> {'Bool', false};
py_to_stack_item(none)                  -> {'Atom', "none"};
py_to_stack_item(V) when is_binary(V)  -> {'String', binary_to_list(V)};
py_to_stack_item(V) when is_list(V)    ->
    {'List', [py_to_stack_item(E) || E <- V]};
py_to_stack_item(V) when is_map(V)     ->
    Items = maps:fold(fun(K, Val, Acc) ->
        [{'Tuple', {py_to_stack_item(K), py_to_stack_item(Val)}} | Acc]
    end, [], V),
    {'List', Items};
py_to_stack_item(V) when is_tuple(V)   -> {'Tuple', V};
py_to_stack_item(V) ->
    {'Atom', lists:flatten(io_lib:format("~p", [V]))}.
```

Python dicts become ActorForth Lists of Tuples (key-value pairs). Python `None` becomes `{Atom, "none"}`. Unknown types are formatted as Atom strings.

### The Complete Round-Trip

```
ActorForth Stack          Erlang Term           Python Object
{Int, 42}           ->   42              ->    42
{String, "hello"}   ->   <<"hello">>     ->    "hello"
{List, [{Int,1},    ->   [1, 2, 3]      ->    [1, 2, 3]
        {Int,2},
        {Int,3}]}

Python Object             Erlang Term           ActorForth Stack
42                  ->    42              ->    {Int, 42}
"hello"             ->    <<"hello">>     ->    {String, "hello"}
[1, 2, 3]          ->    [1, 2, 3]      ->    {List, [{Int,1},{Int,2},{Int,3}]}
{"a": 1}            ->    #{<<"a">> => 1} ->    {List, [{Tuple, {{String,"a"},{Int,1}}}]}
```

---

## The py-call Mechanism

The `py-call` word is the primary mechanism for calling Python functions. Its stack signature is:

```
arg1 ... argN module function arity py-call -> result
```

**Concrete example — calling `math.sqrt(16)`:**

```
ok: 16 math sqrt 1 py-call
```

Step by step:

1. `16` is pushed as `{Int, 16}` via literal handler
2. `math` is pushed as `{Atom, "math"}` (unrecognized token)
3. `sqrt` is pushed as `{Atom, "sqrt"}`
4. `1` is pushed as `{Int, 1}` (the arity)
5. `py-call` pops arity (1), function ("sqrt"), module ("math")
6. Pops 1 argument from the stack: `{Int, 16}`
7. Converts: `af_to_python({Int, 16})` -> `16`
8. Calls: `py:call(<<"math">>, sqrt, [16])`
9. Python returns `4.0`
10. Converts: `py_to_stack_item(4.0)` -> `{Float, 4.0}`
11. Pushes result onto stack

**Multi-argument call — `math.pow(2, 8)`:**

```
ok: 2 8 math pow 2 py-call
```

Arguments are collected from the stack in reverse order (deepest first = first argument), matching the natural left-to-right reading of `2 8` as "first arg 2, second arg 8."

---

## Scope Isolation: A Critical Discovery

A key finding during development: `py:exec` and `py:eval` run in **separate Python scopes**. Functions defined in `py:exec` are NOT visible to subsequent `py:eval` calls:

```
# This does NOT work:
"def add(a, b): return a + b" py-exec
"add(2, 3)" py-eval    # NameError: 'add' is not defined
```

This is a fundamental property of how `erlang_python` manages Python execution contexts. Each `py:exec` and `py:eval` call gets its own scope.

**The solution: module-level state.** Instead of defining functions inline, write Python modules with module-level functions and state. Import the module's directory, then call functions via `py:call`:

```
# This DOES work:
"samples/python" py-import
"hello world" text_tools word_count 1 py-call    # -> {Int, 2}
```

This discovery shaped the entire integration architecture. All Python functionality is delivered through importable modules, not inline code.

---

## The Module-Level State Pattern

Since `py:exec` scopes are isolated, persistent Python-side state must live at the module level. This pattern emerges naturally:

```python
# worker.py
_data = {}

def store(key, value):
    _data[key] = value
    return "ok"

def fetch(key):
    return _data.get(key, "not_found")

def count():
    return len(_data)

def clear():
    _data.clear()
    return "ok"
```

From ActorForth:

```
"samples/python" py-import
"name" "ActorForth" worker store 2 py-call drop
"name" worker fetch 1 py-call                     # -> String("ActorForth")
worker count 0 py-call                             # -> Int(1)
```

Module-level variables (`_data`) persist across `py:call` invocations because `erlang_python` maintains the imported module in memory. This gives each Python module actor-like state without any special framework — just Python's natural module system.

The `actor_bridge.py` module demonstrates a richer version of this pattern with operation history tracking:

```python
# actor_bridge.py
_data = {}
_history = []

def store(key, value):
    _data[key] = value
    _history.append(("store", key, value))
    return "ok"

def history():
    return _history
```

---

## Bidirectional Calls: py-register

The `py-register` word enables the reverse direction — Python code calling ActorForth words. This creates a full bidirectional bridge.

### How It Works

1. Define an ActorForth word: `: double Int -> Int ; dup + .`
2. Compile it to native BEAM: `double af_math compile-to-beam`
3. Register it for Python: `double py-register`
4. Python can now call it: `erlang.double(21)` returns `42`

**Implementation in af_type_python.erl:**

```erlang
op_py_register(Cont) ->
    [{'Atom', WordName} | Rest] = Cont#continuation.data_stack,
    WordAtom = list_to_atom(WordName),
    case af_word_compiler:find_native_word(WordName) of
        {ok, NativeMod, _Arity, _NumOut} ->
            WrapperFun = fun(Args) ->
                erlang:apply(NativeMod, WordAtom, Args)
            end,
            py:register_function(WordAtom, WrapperFun),
            ...
    end.
```

For compiled words, the wrapper directly calls the native BEAM function. For interpreted words, it creates a temporary continuation, pushes the converted arguments, and dispatches through the interpreter:

```erlang
run_word_with_args(WordName, Args) ->
    StackItems = [py_to_stack_item(A) || A <- Args],
    Cont0 = af_interpreter:new_continuation(),
    Cont1 = Cont0#continuation{data_stack = lists:reverse(StackItems)},
    Token = #token{value = WordName, line = 0, column = 0, file = "python"},
    Cont2 = af_interpreter:interpret_token(Token, Cont1),
    case Cont2#continuation.data_stack of
        [Item | _] -> af_term:from_stack_item(Item);
        [] -> none
    end.
```

### The Full Round-Trip

This enables chains like ActorForth -> Python -> ActorForth:

```
# Define and compile a word
: triple Int -> Int ; dup dup + + .
triple af_py_trip compile-to-beam
triple py-register

# Python calls our word, we call Python calling our word
"erlang.triple(5)" py-eval    # -> {Int, 15}
```

The test suite (`af_python_tests.erl`) verifies this with a `py_use_triple` function registered in Python that calls `erlang.triple()`:

```erlang
py:register_function(py_use_triple, fun([X]) ->
    {ok, R} = py:eval(iolist_to_binary(
        io_lib:format("erlang.triple(~p)", [X]))),
    R
end),
C4 = eval("\"erlang.py_use_triple(5)\" py-eval", C3),
[{'Int', 15} | _] = C4#continuation.data_stack
```

---

## Virtual Environment and Dependency Management

### Python Virtual Environment

Python 3.12 was built from source with `--enable-shared` (required for `erlang_python`'s NIF linking). A local virtual environment manages Python packages:

```bash
/home/scherrey/python312/bin/python3.12 -m venv .venv
source .venv/bin/activate
pip install openai
```

The `.venv/bin/activate` script was modified to set `LD_LIBRARY_PATH` for the shared library:

```bash
LD_LIBRARY_PATH="/home/scherrey/python312/lib:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH
```

From ActorForth, the venv is activated with:

```
".venv" py-venv
```

### requirements.txt

Python dependencies are tracked in `requirements.txt`:

```
openai>=2.0
```

### .env for API Keys

The REPL loads `.env` on startup, setting environment variables before any Python code runs. Python modules read API keys via `os.environ`:

```python
# llm_client.py
import os
_api_key = os.environ.get("OPENAI_API_KEY")
```

The `.env` file is gitignored. Format:

```
# API Keys
OPENAI_API_KEY=sk-proj-xxx
```

---

## Python Module Catalog

### text_tools.py — Text Processing

Pure functions for string manipulation. No state, no external dependencies.

| Function | Args | Returns | Description |
|----------|------|---------|-------------|
| `word_count` | str | int | Count words |
| `reverse_words` | str | str | Reverse word order |
| `capitalize_words` | str | str | Title-case each word |
| `slugify` | str | str | URL-friendly slug |
| `extract_numbers` | str | list | Parse numbers from text |
| `levenshtein` | str, str | int | Edit distance |

### ai_stub.py — AI Stubs (No API Key Required)

Deterministic stubs for testing AI workflows without external APIs.

| Function | Args | Returns | Description |
|----------|------|---------|-------------|
| `embed_text` | str | list[float] | Fake 8-dim embedding |
| `cosine_similarity` | list, list | float | Cosine similarity |
| `chat_complete` | str | str | Canned response |
| `from_json` | str | any | Parse JSON string |
| `to_json` | any | str | Serialize to JSON |

### llm_client.py — Real LLM Access

OpenAI and Anthropic client supporting multiple models. Requires API key in `.env`.

| Function | Args | Returns | Description |
|----------|------|---------|-------------|
| `set_api_key` | str | str | Set API key |
| `set_model` | str | str | Set model (gpt-4o, etc.) |
| `set_provider` | str | str | "openai" or "anthropic" |
| `chat` | str | str | Simple chat completion |
| `chat_with_system` | str, str | str | Chat with system prompt |
| `chat_json` | str | dict | Structured JSON response |
| `embed` | str | list[float] | Text embedding vector |
| `embed_batch` | list[str] | list[list[float]] | Batch embeddings |
| `similarity` | str, str | float | Cosine similarity via embeddings |

### http_client.py — REST Client

HTTP client with auth headers, rate limiting, and request statistics.

### worker.py — Stateful Key-Value Store

Module-level state pattern. Simple key-value store demonstrating persistent state across calls.

### actor_bridge.py — Actor-Like Bridge

Extended module-level state with operation history. Demonstrates how Python modules can behave as actors when called from ActorForth.

---

## Architecture Diagram

```
+-----------------------------------------------------------+
|                    ActorForth REPL / Script                 |
|                                                            |
|   ok: "hello" text_tools word_count 1 py-call             |
+-----------------------------------------------------------+
        |                                           ^
        | af_to_python({String, "hello"})           | py_to_stack_item(2)
        v                                           |
+-----------------------------------------------------------+
|                 af_type_python.erl                          |
|                                                            |
|   op_py_call:                                              |
|     1. Pop arity, function, module from stack              |
|     2. Pop N args, convert with af_to_python               |
|     3. Call py:call(Module, Function, Args)                |
|     4. Convert result with py_to_stack_item                |
|     5. Push result onto data_stack                         |
+-----------------------------------------------------------+
        |                                           ^
        | py:call(<<"text_tools">>, word_count,     | {ok, 2}
        |         [<<"hello">>])                    |
        v                                           |
+-----------------------------------------------------------+
|              erlang_python (Hex package)                    |
|                                                            |
|   - Dirty NIF threads (don't block BEAM schedulers)       |
|   - Python C API marshalling                              |
|   - OTP application with worker pool supervision          |
+-----------------------------------------------------------+
        |                                           ^
        | CPython C API                             | PyObject -> Erlang term
        v                                           |
+-----------------------------------------------------------+
|              CPython 3.12 (libpython3.12.so)               |
|                                                            |
|   text_tools.word_count("hello")  ->  1                   |
|                                                            |
|   Module-level state persists across calls                |
|   Virtual environment for pip packages                    |
+-----------------------------------------------------------+
```

---

## Error Handling

All Python operations are wrapped in try/catch blocks. Errors are converted to structured ActorForth errors with the `python_error` type:

```erlang
try
    {ok, Result} = py:call(Module, Function, ErlArgs),
    ...
catch
    _:Reason ->
        Msg = lists:flatten(io_lib:format(
            "py-call ~s:~s/~p failed: ~p",
            [ModStr, FunStr, Arity, Reason])),
        af_error:raise(python_error, Msg, Cont)
end.
```

This produces errors like:

```
Error: python_error at stdin:1:15
  py-call text_tools:nonexistent/1 failed: {error,{python,...}}
  Stack(1): "hello":String
  Word trace: [...]
```

Stack underflow (not enough arguments for the declared arity) is caught before the Python call:

```erlang
case length(Rest) >= Arity of
    false ->
        af_error:raise(stack_underflow,
            io_lib:format("py-call needs ~p args on stack", [Arity]),
            Cont)
end.
```

---

## Testing

The test suite (`test/af_python_tests.erl`) covers 20 test cases across five groups:

| Group | Tests | What's Verified |
|-------|-------|-----------------|
| `py_eval_test_` | 5 | Expression evaluation: int, string, float, bool, list results |
| `py_call_test_` | 4 | Function calls: single arg, multi arg, zero arg, constants |
| `py_exec_test_` | 1 | Statement execution without return |
| `py_import_test_` | 4 | Custom module import: word_count, slugify, reverse_words, levenshtein |
| `ai_stub_test_` | 3 | AI stubs: embeddings, JSON parsing, cosine similarity |
| `py_register_test_` | 1 | Bidirectional: compile word, register for Python, call from Python |
| `actor_bridge_test_` | 2 | Module-level state: store/fetch/count, round-trip AF->Py->AF |

All tests run via `rebar3 eunit` and are included in CI. The Python runtime starts automatically via `af_type_python:ensure_started()` in the test setup.

---

## Lessons Learned

1. **Scope isolation is the biggest gotcha.** `py:exec` and `py:eval` do not share scope. Everything must go through imported modules. This was discovered empirically and shaped the entire architecture.

2. **Module-level state is the actor pattern.** Python modules with `_data = {}` at module level naturally behave like actors — they maintain state between calls, accessed only through their exported functions.

3. **String representation matters.** ActorForth strings can be Erlang lists or binaries depending on how they were created. The `to_bin/1` helper in `af_type_python.erl` normalizes this before passing to Python, preventing `badarg` errors.

4. **The dirty NIF model works.** Python's GIL runs on BEAM dirty scheduler threads, so Python computation does not block BEAM schedulers. This means ActorForth actors can call Python without impacting the responsiveness of other BEAM processes.

5. **py-register enables true bidirectional interop.** An ActorForth word compiled to native BEAM can be registered as a Python-callable function, then called from Python code that was itself called from ActorForth. This creates a full AF -> Python -> AF round-trip, verified in the test suite.

6. **Virtual environment activation requires LD_LIBRARY_PATH.** When Python is built from source with `--enable-shared`, the shared library location must be in `LD_LIBRARY_PATH` or the NIF fails to load.

---

## Future Directions

- **Async Python calls.** The `erlang_python` library supports `py_async_pool` and `py_async_worker` modules. Exposing these as ActorForth words would allow non-blocking Python calls from actors.

- **Python actors as BEAM processes.** Wrapping a Python module's state in a gen_server that dispatches messages to Python functions, creating true Python-backed actors supervisable by OTP.

- **Type-safe Python interfaces.** Generating ActorForth type signatures from Python type hints, enabling compile-time validation of Python calls.

- **Streaming LLM responses.** Using Python's async streaming APIs for LLM responses, with each token sent as a message to an ActorForth actor.
