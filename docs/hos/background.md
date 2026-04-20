---
marp: true
theme: default
paginate: true
backgroundColor: "#fff"
---

<!--
  Higher Order Software and the Elevator Demo
  Background / slide deck for the 2026-04-30 talk.

  This file is Marp-format markdown: it reads as a plain document
  on GitHub, and projects as a slide deck via:

      npx @marp-team/marp-cli docs/hos/background.md --pdf
      npx @marp-team/marp-cli docs/hos/background.md --html
      npx @marp-team/marp-cli -s docs/hos/background.md  # live preview

  Slide breaks are `---` lines. Plain markdown for content.
-->

# Higher Order Software
## and the Elevator Demo

ActorForth (a4) applies Margaret Hamilton's 1976 axioms
on top of the Erlang BEAM actor runtime.

*This deck is the backgrounder for the live demo
(`samples/hos/elevator/run_hos_tests.sh`).*

---

## The problem

Six bug categories keep showing up in the reference elevator,
whether written in Python, OCaml, or Rust:

1. **Door opens while motor is moving** (sibling trust)
2. **Motor moves without checking door** (sibling trust, symmetric)
3. **Emergency ignored during travel** (TOCTOU on a shared flag)
4. **Operations in wrong order** (order-by-statement-sequence)
5. **Emergency system reaches directly into cars** (sibling back-channel)
6. **Dispatcher uses stale car positions** (snapshot coordination)

Not failures of programmer intelligence. Failures of what the
language makes *easy to express.*

---

## Margaret Hamilton

- Led the Apollo flight software team at MIT in the 1960s.
- Coined the term *software engineering* so her work would be
  taken as seriously as the hardware engineering around her.
- Presidential Medal of Freedom, 2016.
- Founded Higher Order Software Inc. in 1976 to commercialise
  the axioms this deck is about.

Her experience with Apollo shaped what became HOS.

---

## The Apollo 11 lesson

During the Lunar Module descent, alarms 1202 and 1201 fired:
the landing radar was flooding the guidance computer with
unscheduled interrupts.

Hamilton's executive software *rejected the inputs safely*
rather than crashing. The landing proceeded. Armstrong
stepped out.

The safe-rejection behaviour was not a defensive afterthought.
It was a *structural* property of the executive: inputs with no
declared handler were dropped, not processed.

That property became Hamilton's Axiom 3.

---

## The central insight

Two ways to prevent a class of bugs:

| Discipline                          | Structure                            |
|-------------------------------------|--------------------------------------|
| "Be careful to check motor state"   | "Door cannot be named from Motor"    |
| Fragile to maintainer turnover.     | Durable.                             |
| Reviewer must look at every call.   | Reviewer reads the scope once.       |
| Passes CI until it doesn't.         | Does not compile if violated.        |

HOS is language design built entirely around making specific
bug classes **structurally unexpressable**.

---

## Hamilton's six axioms (1976)

In her original formulation:

1. A module **controls the invocation** of its immediate subordinates, and only them.
2. A module **rejects invalid input** only in its own input space, and only there.
3. A module is **responsible for its own output**, and only its own.
4. A module **controls the ordering** of its immediate subordinates.
5. A module **controls access** to its own resources.
6. A module **controls the scope of data** flowing across its tree position.

Four of these are load-bearing for the a4 elevator demo.
We will cover them one at a time.

---

## Axiom 1: invocation scope

A module invokes its **immediate subordinates**, and only them.

```
    BuildingSystem
    |-- Dispatcher
    |   `-- Car
    |       |-- Door
    |       `-- Motor
    `-- EmergencySource
```

- **Forbidden**: `EmergencySource` reaches into `Car` (skip-level,
  and across the tree).
- **Forbidden**: `Door` reaches into `Motor` (sibling).
- **Permitted**: `Car` calls `Door` or `Motor`.

"What cannot be named cannot be called."

---

## Axiom 2: input rejection

A module in state S, receiving input I for which no transition
`(S, I)` is declared, **silently drops I.**

No crash. No partial effect. No warning the module acted on.

This is what Apollo 11's executive did with the flooded radar
interrupts. In the elevator: a late `arrived` timer self-send
arriving at a stopped motor has no transition from `Stopped`
under `arrived` and is dropped. The motor stays put.

The axiom turns "unexpected input" from a defensive-coding
obligation into a **structural guarantee**.

---

## Axiom 3: output responsibility

A module is responsible for its own output, and only its own.

- A module **cannot emit arbitrary effects** from inside a handler.
- Every effect is **declared** on a transition row.
- Every effect target is in scope (Axiom 1).

In a4: stateful systems have *empty* handler bodies. Every
subsystem interaction is declared on a transition as
`: Target event`. The table *is* the output specification.

A reviewer reading the transitions table sees every
side effect the module will ever produce.

---

## Axiom 4: order

A module controls the **order** in which its immediate
subordinates are invoked.

In the naive Python:

```python
self.door.close()
self.motor.move_to(floor)
self.door.open()
```

The order is *statement sequence*. A future maintainer can
swap lines; another thread can call the subsystems directly.
Nothing structural prevents it.

In a4, order is the *sequence of events the parent accepts*,
with each transition declaring its effect. Reordering requires
visibly editing the transitions table.

---

## Axioms 5 and 6

Present for completeness. Not enforced by the a4 checker in v1:

- **Axiom 5 (resource access)**: a module controls access to its
  own resources. Related to encapsulation. A4 approximates via
  Axiom 1 scope and effect target checks; fine-grained resource
  ACLs are out of scope for the elevator demo.

- **Axiom 6 (data scope)**: a module controls the scope of data
  flowing through it. Dependent on a data model the v1 DSL
  does not have (handler signatures carry no arguments in v1;
  data propagation is deferred to v2).

Flagged as future work in the chapter and in the HOS design
memory.

---

## What HOS is *not*

- **Not a type system** (Haskell, Rust). Types constrain values;
  HOS constrains *invocation structure* and *input/output
  responsibility*.
- **Not formal verification** (TLA+, Agda). No proof obligations,
  no theorem prover, no invariant annotations.
- **Not model checking**. No exhaustive state-space exploration.
- **Not an actor framework** (Erlang, Akka). Those give you
  mailbox isolation; HOS gives you the axioms *about* what
  can be sent and who can send to whom.

HOS is **language design at the structural level**, chosen
to make specific bug categories unexpressable.

---

## The six naive elevator flaws

| # | Flaw                                  | Axiom violated |
|---|---------------------------------------|----------------|
| 1 | Door opens while motor moves          | Axiom 4 (order) + 3 (output) |
| 2 | Motor moves without checking door     | Axiom 4 + 3                  |
| 3 | Emergency polled, not messaged        | Axiom 2 (input rejection)    |
| 4 | Cycle order by statement sequence     | Axiom 4                      |
| 5 | EmergencySystem reaches into Car      | Axiom 1 (scope)              |
| 6 | Dispatcher uses stale car snapshot    | Axiom 6 (data scope, deferred) |

A single flaw class can violate more than one axiom; HOS is
not fussy about which one "really" catches it, as long as one
of them does.

---

## How a4 realises Axiom 1: scope

`src/af_hos_check.erl :: check_handler/3`

Walks every token of every handler body. For each token, tests
membership in the **scope set**:

```
{ parent_name, self_name, child_names,
  childs_declared_events, own_transition_states,
  a4_builtins }
```

Anything else is a compile-time `axiom_1` violation. Error
message names the system, the handler, and the offending token.

~40 lines of Erlang. Enforced by *walking a list*, not by any
runtime coordination.

---

## How a4 realises Axiom 2: input rejection

**Dual enforcement, compile and runtime.**

Compile-time (Tier 1): *exhaustive emergency check.* A trigger
whose transitions converge on one state is classified an
emergency trigger; every non-emergency state must cover it,
or the spec is rejected. Guarantees no emergency can arrive
in a state that drops it silently in the *normal* sense.

Runtime: `af_hos_runtime:dispatch_transition/3`. Look up
`(current_state, event)` in the transitions table. No match,
no effect, no state change. The message is logged and life
goes on.

---

## How a4 realises Axiom 3: output responsibility

Two checks in `af_hos_check.erl`:

1. **Stateful body rule**: a system that declares `state` must
   have empty `on X -> ;` handler bodies. Any content is a
   compile error. Drivers are transitions, not code.

2. **Effect target scope**: each transition's `: Target event`
   clause has its target validated against the scope set from
   Axiom 1.

Combined: a module cannot produce effects its parent did not
declare. The table is the full output manifest.

---

## How a4 realises Axiom 4: order

Order is encoded in the **events that sequence transitions**:

```
IdleAtFloor -> PreparingToMove assign : Door close
PreparingToMove -> Moving closed : Motor move-to
Moving -> Arriving arrived : after 200 settled
Arriving -> DoorOpening settled : Door open
```

Each step is triggered by a distinct event. `Motor move-to`
only fires after `closed` (Door's upward signal). `Door open`
only fires after `settled` (Car's self-timer, after `arrived`).

A reviewer reads the sequence directly. A writer cannot
reorder without visibly deleting transition rows.

---

## A4 vs BEAM: who does what

| Capability                          | Provided by |
|-------------------------------------|-------------|
| Axiom 1 scope check                 | **a4** (compile) |
| Axiom 2 exhaustive emergency        | **a4** (compile) |
| Axiom 2 runtime input rejection     | a4 (runtime dispatch, but) |
| Axiom 3 stateful body empty         | **a4** (compile) |
| Axiom 3 effect target scope         | **a4** (compile) |
| Axiom 4 transitions as order        | **a4** (DSL + runtime) |
| Mailbox FIFO per actor              | **BEAM** |
| Process isolation (no shared memory)| **BEAM** |
| `erlang:send_after` timer           | **BEAM** |
| Process crash safety                | **BEAM** |
| Supervisor-based restart policies   | **BEAM** (unused here) |

The axioms are all a4. Their *support* comes from BEAM.

---

## What BEAM gives us for free

Without BEAM, implementing HOS on top of a thread+mutex language
would require *writing* a mailbox, a per-actor event loop, a
timer primitive, and a crash-isolation scheme before we could
even start on the axioms. Each of those has its own race-
condition history.

BEAM means the foundation properties the correctness argument
depends on are already present and battle-tested:

- FIFO per-actor mailbox is how Axiom 2 input rejection composes
  with emergency preemption.
- Process isolation is how Axiom 1 is a runtime as well as a
  compile property.
- `send_after` is how the timer primitive works without blocking.

a4 could target other runtimes. Building HOS on a non-actor
platform would be a much larger project.

---

## How much of this is a4, in lines?

Rough accounting of the HOS layer:

| Component                            | LOC |
|--------------------------------------|----:|
| `af_hos_dsl.erl` (the DSL)           | 296 |
| `af_hos_check.erl` (the axioms)      | 722 |
| `af_hos_runtime.erl` (dispatch)      | 369 |
| `lib/hos/system.a4` (DSL types)      | 122 |
| **Total HOS layer**                  | **1,509** |

The elevator spec itself is **233 lines**, 93 non-comment.

For comparison: the correct references are 444 / 445 / 494 lines
of Python / OCaml / Rust, *per elevator implementation*, with
the correctness properties defended by hand rather than enforced.

---

## The correctness proof: four pillars

1. **Compile-time axioms** (`af_hos_check`).
   19 tests. 5 axiom checks. Rejects ill-formed specs.

2. **FAT on the valid spec** (`af_hos_elevator_tests`).
   21 tests. Observes correct preemption on the running tree.

3. **Violation catalog** (`af_hos_violation_catalog_tests`).
   19 tests. Every axiom violated in the minimal way, every
   one caught by name.

4. **Runtime evidence** (`af_hos_comparison_tests` +
   `af_hos_stress_tests`). 9 tests. Naive-Python flaws either
   unexpressable or traced to be absent at runtime, under
   sustained interleaving.

Total: **68 tests**. Full project suite: 2121 tests green.

---

## What the live demo shows

Six scenes. Each gated on enter if run with `--pause`.

1. Introduction.
2. **Three Python exploits** make FLAWs 1, 3, 4 manifest on
   screen with timestamped state dumps.
3. **Two a4 checker rejections** refuse the equivalent specs
   inline, with the exact `axiom_1` / `axiom_5` messages.
4. The four test pillars run; every one green.
5. Line count comparison across four languages.
6. Summary.

About 100 seconds straight through; presenter-paced with
`--pause`. See `samples/hos/elevator/run_hos_tests.sh`.

---

## Closing

HOS is 50 years old. The axioms compose with an actor runtime
in a way they did not with the 1970s hardware Hamilton targeted.
The elevator is the smallest case that shows all four key
axioms in action.

Not claimed:

- That HOS is the only way to prevent these flaws.
- That every control problem maps cleanly to HOS.
- That the v1 a4 DSL covers every axiom (Axioms 5 and 6 are
  acknowledged as future work).

What *is* claimed:

- The six naive-reference flaws are compile-time rejected,
  structurally unexpressable, or prevented at runtime by the
  axioms as realised.
- The evidence lives in 68 tests, readable as a catalog.

---

## References

- Hamilton, M. H., & Zeldin, S. (1976). *Higher Order Software --
  A Methodology for Defining Software.* IEEE Transactions on
  Software Engineering, SE-2(1).
- Chapter: `docs/hos/elevator.md` (full walkthrough, honest scope).
- Demo script: `samples/hos/elevator/run_hos_tests.sh`.
- References: `samples/hos/elevator/comparison/` (py, ml, rs
  pairs plus adversarial exploits).
- Design memory: HOS DSL decisions and correctness roadmap.
