# Debounce demo

Six actors pipelined: a `Store` collects values, a `Tally` counts them,
and an `Emitter` fires after a quiet period. The demo exercises the
actor model under event bursts — values stream in, the pipeline
aggregates, and the emitter fires exactly once per burst after the
debounce window expires.

## Run the tests across all languages

```
./run_all.sh
./run_all.sh --bench 10     # also benchmark each implementation in-process
```

## Code-size comparison

Non-blank, non-comment lines of the implementation:

| Language     | Impl lines | Notes |
|--------------|-----------:|-------|
| **a4**       | **52**     | Six typed actors; `<< >>` dispatch, `receive-timeout` for the window |
| Elixir       | 63         | GenServer per actor + `Process.send_after` for the window |
| Erlang       | 105        | gen_server per actor + explicit state records |
| TypeScript   | 108        | Custom Actor base class with mailbox + async dispatch loop |
| Python       | 177        | asyncio.Queue + per-actor tasks + cancellable timers |
| C++20        | 226        | std::thread per actor + std::mutex/condition_variable/chrono |

## Why a4 is short here

- **Actors compose without ceremony.** Six actors in 52 lines means each
  is ~8 lines. In Erlang/Elixir you pay `init/1` + `handle_call` +
  `handle_info` per actor; in C++ you pay the thread lifecycle per
  actor. a4 just declares the type, names the ops, and says `server`.

- **`receive-timeout` is the debounce.** The emitter's "fire after
  quiet" logic is one `receive-match-timeout` call. Python/C++ build
  cancellable timer objects that outlive their callers.

## Files in this directory

- `debounce_demo.a4` — ActorForth implementation (52 lines)
- `debounce_elixir_equivalent.exs` — Elixir reference (63 lines)
- `debounce_erlang_equivalent.erl` — Erlang reference (105 lines)
- `debounce_typescript_equivalent.ts` — TypeScript reference (108 lines)
- `debounce_python_equivalent.py` — Python reference (177 lines)
- `debounce_cpp20_equivalent.cpp` — C++20 reference (226 lines)
- `run_all.sh` — runs and optionally benchmarks every implementation
