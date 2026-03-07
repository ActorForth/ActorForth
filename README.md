actorforth
=====

ActorForth is Open Source software covered under BSD license (see
LICENSE.txt).  If you contribute to this project then you guarantee
that you have the right to contribute your content & IP, will abide by
said license and do assign an unlimited, universal, non-exclusive
license to the copyright holder and anyone else who agrees to abide by
the license.

ActorForth is a homoiconic, strongly-typed, concatenative, stack-based
language with actor concurrency as a primitive, targeting the Erlang BEAM VM.

Prior implementations exist in Python 3.x (most complete — type system,
compiler, product types) and C++ (partial — type system, basic interpreter)
in earlier git history. The current Erlang implementation is the real one:
an Erlang-hosted interpreter with native BEAM compilation, OTP integration,
actor primitives, and Python interop.

See [ActorForthDefinition](docs/ActorForthDefinition.md) for a quick
overview of how the language works.

See [Introduction to ActorForth](docs/IntroToActorForth.md) for a
comprehensive tutorial in the style of Leo Brodie's "Starting Forth."

See [Implementation Plan](docs/IMPLEMENTATION_PLAN.md) for the full
development roadmap.


Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 eunit --cover    # Run all tests with coverage
    $ rebar3 ct               # Common Test suites

REPL
-----

    $ rebar3 shell
    1> af_repl:start().
    ActorForth REPL. ^C to exit.
    ok: 42 dup + print
    84

Run a script:

    1> af_repl:run_file("samples/fib.a4").

Python Interop
-----

Requires Python 3.12+ and the `erlang_python` hex package (included as dependency).

    ok: py-start
    ok: "2 + 2" py-eval
    ok: print
    4

See `samples/python/` for example Python modules and `samples/py_*.a4` for
ActorForth demo scripts.
