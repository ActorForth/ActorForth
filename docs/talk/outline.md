# Talk Outline — ActorForth (2026-04-30)

Scaffolding only. The narration around each slide is yours.

**Estimated length:** 35–40 minutes plus Q&A.
**Audience assumption:** BEAM-literate, at least loosely familiar with concatenative languages.

---

## Act 1 — What ActorForth is (~8 min)

### Slide 1 — Title
- ActorForth
- *Homoiconic, concatenative, strongly-typed, actor-concurrent*
- Your name / affiliation / date

### Slide 2 — The one-sentence pitch
- "A Forth with types and actors that targets the BEAM and lets you build a DSL for your problem in minutes, not months."

### Slide 3 — Why this language exists
- Lift verbatim from the Preface in `IntroToActorForth.md`:
  - Language design as intellectual pastime
  - DSL-first workflow
  - Forth's answer, plus the two holes (types + concurrency)
- Anchor quote at the end of the slide:
  > *"The best ActorForth programs rarely use ActorForth primitives."*

### Slide 4 — The giants
- Moore (Forth), Hewitt (Actors), Kay (the original OO —
  *messaging, hidden state-process, extreme late-binding*),
  Reenskaug & Coplien (DCI — data vs role vs context),
  Steele (Growing a Language), Stroustrup (zero-overhead abstractions),
  Czaplicki (friendly small languages),
  Hamilton / Dijkstra (proof at scale — where it buys you something,
  and where it stops).
- One sentence each. Don't linger; this is context for what follows.
- When you hit Kay, pause: "Kay's definition of OO is the actor
  model, and he's said many times it was never the thing Java
  inherited. Hold that thought for slide 7."

---

## Act 2 — The machinery (~10 min)

### Slide 5 — Everything is a stack
- `42 3 +` with the stack visualization
- No variables, no syntax errors — only type errors
- Tokens that don't match an op fall through to `Atom`

### Slide 6 — Types own dispatch
- Every value is `{Type, Value}`
- Words register on the TOS type's dict
- Dispatch order: TOS dict → TOS handler → Any dict → literal → Atom
- *The compiler is just four types with handlers.* (This is the homoiconic punchline.)

### Slide 7 — Actors as a primitive
- `server`, `<<`, `>>`, `spawn`, `send`, `receive`
- Unbounded non-determinism as the default, not a library pattern
- BEAM pid = a4 Actor. One-to-one.
- Call back to Kay: every `server` is a process; every `<<` crosses
  a real process boundary; dispatch is resolved at message receipt
  through the type's dictionary. *Messaging, hidden state-process,
  extreme late-binding* — the three clauses of Kay's definition,
  implemented literally. "By Kay's own words, a4 is more
  object-oriented than the languages that inherited the label."
- This is the anchor moment of the talk's intellectual claim.
  Don't rush past it. If you get one big Q&A question out of the
  talk, it will probably come from this slide.

### Slide 8 — DSL-first example, live
- Small DSL built at the REPL in front of the audience (pick something short: a budget allocator, a state machine, whatever you'd enjoy).
- Five minutes, max. End with: "Now we have a language for this problem."

---

## Act 3 — The killer demo (~15 min)

### Slide 9 — Testing is the biggest DSL we built
- "We wrote a test framework *in* ActorForth to exercise ActorForth."
- Tests are actors. Hundreds of them. Parallelism default.
- Coverage is another sink on the traced dispatch path.
- **Average measured size reduction vs. the Erlang tests they replaced: 41%.**

### Slide 10 — Live demo
```
cd ~/projects/ActorForth
rebar3 shell
> af_test_runner:run(["samples/talk_demo.test.a4"], [{render,dashboard}]).
```
- Walk through `samples/talk_demo.test.a4` briefly: nested groups, `group-serial`, `skip-test`, `max-depth`, the deliberate failure.
- On the dashboard:
  - Point out the pids in "Running now"
  - Show the progress bar
  - When the deliberate failure lands, read the diagnostic: *"extra atom 'bogus-word-typo' on stack..."*
- Close: "Each test was an actor. You saw the pids. You saw the failure pointed at source. That's a test framework we built in our own language."

### Slide 11 — The test DSL's size
- Show the LOC table from `docs/design/test-dsl-plan.md` ("Measured result")
- One sentence: "We spent ~2000 lines of infrastructure once; every new test file is ~40% shorter forever."

---

## Act 4 — Closing (~5 min)

### Slide 12 — The canonical workflow
- "Build a DSL for your problem. Solve the problem in your DSL. The rest of the work is at the REPL."
- Mention LLM fit briefly — see Chapter 26 of the book for the design argument.

### Slide 13 — What's not there yet
- Honest list:
  - Core Erlang target (multi-clause, product types incomplete)
  - LSP multi-file + incremental
  - Production-grade ring-mode exec stack
- Framing: "This is the shape of what's left. The language is small enough that each of those is tractable."

### Slide 14 — Where to go
- Repo URL
- `docs/IntroToActorForth.md` for the book
- `docs/architectural-drivers-considerations.md` for the architectural argument
- `samples/` for runnable examples
- `lib/testing/` for the test DSL itself as reference code

### Slide 15 — Thanks / Q&A

---

## Demo rehearsal checklist

Run through this at least twice before the talk:

- [ ] `rebar3 compile` clean
- [ ] `rebar3 eunit` green (~1994 tests)
- [ ] `af_test_runner:run(["lib/testing"]).` green (~246 tests + 1 skip)
- [ ] `af_test_runner:run(["samples/talk_demo.test.a4"], [{render,dashboard}]).`
      runs end-to-end, dashboard renders, failure shows `extra_atom`
      diagnosis
- [ ] Terminal font big enough for back row (1.5x typical)
- [ ] Terminal dimensions >= 100×30 for dashboard not to wrap
- [ ] ANSI colors show correctly on projector
- [ ] Pre-load `samples/talk_demo.test.a4` in Sublime with LSP active
      (syntax highlight visible, hover working if you plan to show it)
- [ ] Backup recording of the demo in case of tech trouble

## Speaker notes — things to keep in mind

- The `extra_atom` diagnostic is the emotional peak of the demo — treat it like a punchline, pause after it.
- When the dashboard shows the pids, say "these are real OS processes" — that's the actors-are-everywhere claim, land it.
- "The best ActorForth programs rarely use ActorForth primitives" is the anchor quote. Repeat it at least twice: once early, once in closing.
- Don't apologize for what's not there. Just name it honestly.
