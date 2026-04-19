# The Elevator: Structural Safety by Declared Events

> "The way to build a system that cannot fail is to make the failing
> versions unrepresentable."
> - after Margaret Hamilton

This chapter is about a small claim that turns out to be a big one.
We take a textbook control problem (an elevator), list six
well-known interface errors in the textbook implementations, and
show that in the a4 HOS spec they are either rejected at compile
time or structurally prevented by the runtime model. The chapter
ends with an honest list of what is still scoped for v2.

The structural claim now rests on four pieces, working together:

- **Axiom 1 scope.** No system may reference a name outside its
  parent, children, own events, transition states, or a4 builtins.
  Effects and body tokens both go through this check.
- **Effects-in-transitions.** A subsystem interaction is always a
  declared row in the transitions table, never an inline body
  token. Reorderings are visible in the table.
- **One event fires one transition.** There is no cascade. Multi-
  step sequences happen because multiple events arrive, each
  driving one step. Child-to-parent signals (upward events) drive
  parent steps; the parent cannot proceed until the signal arrives.
  This is the structural replacement for "imperative body walks
  the state machine."
- **Axiom 4 input rejection.** An event whose declared transition
  is not valid from the live state is a no-op. Unexpected input
  is safe.

---

## 1. Why an elevator

The canonical introductory control problem. Real parts (door,
motor, car, emergency source, dispatcher, building), real ordering
constraints (open door only while stopped, start motor only while
closed), real concurrency (emergency events cutting across the
normal flow), real consequences (people get crushed if you get it
wrong).

Three "correct" reference implementations come with this repo:
`comparison/elevator_correct.py`, `elevator_correct.ml`,
`elevator_correct.rs`. Each runs 300 to 500 lines. Each has a
suite of FAT tests checking that six specific bugs do not recur:

1. **Door opens while motor is moving** (sibling trust).
2. **Motor moves while door is open** (mirror of 1).
3. **Emergency flag race** (TOCTOU on a polled boolean).
4. **Operation reordering mid-transit** (state machine skips a step).
5. **EmergencySource reaches a Car directly**, bypassing Dispatcher.
6. **Duplicate assignment from a stale dispatcher snapshot**.

In the reference versions the tests are the insurance. In the a4
HOS spec, the insurance is structural.

---

## 2. The tree

```
BuildingSystem
|-- Dispatcher
|   `-- Car
|       |-- Door
|       `-- Motor
`-- EmergencySource
```

`EmergencySource` has no connection to `Car`; its only neighbour
is `BuildingSystem`. `Dispatcher` has `Car` as its only child.
`Door` and `Motor` are siblings under `Car` and have no connection
to each other. Nothing in the tree crosses the hierarchy. What
cannot be named cannot be called.

---

## 3. Semantic model

A stateful HOS system declares:

- Its state type (e.g., `state CarState`).
- Its expected events as empty `on X -> ;` declarations (optional
  documentation; the real driver is the transitions table).
- A transitions table whose rows are one of:
  - `From -> To Trigger`
  - `From -> To Trigger : Target event` (downward command or
    upward signal; target is a child name, parent name, or self)
  - `From -> To Trigger : after <ms> event` (timer: scheduled
    self-send)

**One event fires one transition.** When an event E arrives at a
stateful system in state S, the runtime looks up the single row
`(S, _, E)` in the transitions table. If found, it fires the row:
dispatches the effect (if any), advances current_state to the To
column, and returns. There is no cascade; a second transition
requires a second event.

If no row matches `(S, _, E)`, the event is logged and ignored
(Axiom 4 input rejection). This is the same contract as a4 itself:
tokens that don't match operations become atoms on the stack; no
syntax errors, only type errors. An unexpected event produces no
state change and no behaviour.

**Child-to-parent upward signals.** A child declares an effect on
its parent using the parent's name as the target:

```
Stopped -> Moving move-to : after 500 arrived
Moving -> Stopped arrived : Car arrived
```

The second row reads: "when Motor transitions Moving to Stopped
under trigger `arrived`, send event `arrived` to Car." Car's
transitions table has `Moving -> Arriving arrived`, so Car proceeds
only after receiving the upward signal from Motor. The
synchronisation is structural: Car *cannot* transition past Moving
until Motor has reported arrival.

**Timer primitive.** `: after <ms> event` schedules a self-send.
The mailbox stays open during the wait, so other events (stop,
trigger-emergency) are processed as they arrive. A late self-send
whose declared transition is not valid from the live state is
input-rejected. This gives preemption for free: `stop` while Motor
is in Moving fires `Moving -> Stopped stop`; the late `arrived`
self-send hits Motor in Stopped, where no row `(Stopped, _, arrived)`
is declared, and is dropped.

The timer's fire-and-ignore-if-rejected semantics is a deliberate
choice. Alternatives considered and not adopted:

- *Cancellable timers.* A stop could cancel pending schedules.
  Simpler systems do not need this; input rejection covers the
  common case. Cancellation would help systems with many
  concurrent timers.
- *Retry-on-late-arrival.* Some actor frameworks re-fire a late
  timer by applying it to the current state. For real-time
  control this is almost never the right default.

**Handler bodies.** Stateful systems have empty handler bodies.
The transitions table is authoritative. Any content in a stateful
handler body is an axiom violation. Stateless systems (no `state`
declared) still use handler bodies for pass-through routing.

**Events are pure signals.** Handler signatures carry no arguments
in v1. Data propagation (which floor, which assignment) is a v2
concern to be handled via system state fields. A sketch of v2:

```
# v2: data in state, reads in effects
system Motor
    state MotorState   # contains target_floor: Int
    on move-to -> ;
    on arrived -> ;

    transitions
        Stopped -> Moving move-to : after self.travel-time arrived
        Moving -> Stopped arrived : Car arrived
```

where `self.travel-time` reads from Motor's state. Not implemented
in v1; noted for future work.

---

## 4. The valid spec

Here is `samples/hos/elevator/elevator.a4`, verbatim. It compiles.
Every handler body is empty. Every subsystem interaction is a
declared effect.

```a4
system Door
    parent Car
    state DoorState

    on open -> ;
    on close -> ;
    on opened -> ;
    on closed -> ;

    transitions
        Closed -> Opening open : after 100 opened
        Opening -> Open opened : Car opened
        Open -> Closing close : after 100 closed
        Closing -> Closed closed : Car closed
        Closed -> Closed close : Car closed
        Open -> Open open : Car opened
end

system Motor
    parent Car
    state MotorState

    on move-to -> ;
    on arrived -> ;
    on stop -> ;

    transitions
        Stopped -> Moving move-to : after 500 arrived
        Moving -> Stopped arrived : Car arrived
        Moving -> Stopped stop
end

system Car
    parent Dispatcher
    state CarState
    children Door Motor

    on assign -> ;
    on closed -> ;
    on opened -> ;
    on arrived -> ;
    on settled -> ;
    on depart -> ;
    on trigger-emergency -> ;
    on clear-emergency -> ;

    transitions
        IdleAtFloor -> PreparingToMove assign : Door close
        PreparingToMove -> Moving closed : Motor move-to
        Moving -> Arriving arrived : after 200 settled
        Arriving -> DoorOpening settled : Door open
        DoorOpening -> Loading opened : after 300 depart
        Loading -> DoorClosing depart : Door close
        DoorClosing -> IdleAtFloor closed

        IdleAtFloor -> EmergencyStopped trigger-emergency : Motor stop
        PreparingToMove -> EmergencyStopped trigger-emergency : Motor stop
        Moving -> EmergencyStopped trigger-emergency : Motor stop
        Arriving -> EmergencyStopped trigger-emergency : Motor stop
        DoorOpening -> EmergencyStopped trigger-emergency : Motor stop
        Loading -> EmergencyStopped trigger-emergency : Motor stop
        DoorClosing -> EmergencyStopped trigger-emergency : Motor stop

        EmergencyStopped -> EmergencyDoorOpen clear-emergency : Door open
        EmergencyDoorOpen -> IdleAtFloor opened
end

system EmergencySource
    parent BuildingSystem
    state EmergencyState

    on trigger -> ;
    on clear -> ;

    transitions
        Idle -> Active trigger : BuildingSystem set-emergency
        Active -> Idle clear : BuildingSystem clear-emergency
end

system Dispatcher
    parent BuildingSystem
    state DispatcherState
    children Car

    on request -> ;
    on set-emergency -> ;
    on clear-emergency -> ;

    transitions
        Normal -> Normal request : Car assign
        Normal -> Emergency set-emergency : Car trigger-emergency
        Emergency -> Normal clear-emergency : Car clear-emergency
end

system BuildingSystem
    state BuildingState
    children Dispatcher EmergencySource

    on floor-button-press -> ;
    on fire-alarm -> ;
    on fire-clear -> ;
    on set-emergency -> ;
    on clear-emergency -> ;

    transitions
        Normal -> Normal floor-button-press : Dispatcher request
        Normal -> Normal fire-alarm : EmergencySource trigger
        Normal -> Emergency set-emergency : Dispatcher set-emergency
        Emergency -> Normal clear-emergency : Dispatcher clear-emergency
        Emergency -> Emergency fire-clear : EmergencySource clear
end
```

Notes:

- **Door** has multi-step open/close with timers, plus two
  idempotent self-loops (`Closed -> Closed close`,
  `Open -> Open open`) so that Car's waiting transitions are
  unblocked when Door is already in the target state.
- **Motor** uses the timer for 500ms travel. Normal arrival signals
  Car upward; interrupted stop does not (Car already knows, since
  it issued the stop).
- **Car**'s seven normal-flow transitions are each triggered by a
  distinct event: `assign` from Dispatcher, `closed` from Door,
  `arrived` from Motor, `opened` from Door, `depart` from self-
  timer, `closed` from Door. Each transition is one actor message
  cycle; any emergency in the mailbox interleaves.
- **Dispatcher**'s `Normal -> Normal request` is an internal
  transition (Harel/UML terminology): state does not change; the
  effect dispatches `Car assign`. Dedicated internal-transition
  syntax is future work.
- **BuildingSystem** is stateful. The emergency path
  `fire-alarm -> EmergencySource trigger -> (upward) set-emergency
  -> Dispatcher set-emergency -> Car trigger-emergency -> Motor
  stop` is a chain of single-step transitions through the tree.

---

## 5. What the compiler rejects

Each classic flaw expressed as an HOS spec is rejected at compile
time with a specific axiom citation. The FAT suite in
`test/af_hos_elevator_tests.erl` asserts every rejection.

### Flaw 5: sibling back channel

```a4
system EmergencySource
    parent BuildingSystem
    on trigger -> ;
        Car trigger-emergency
end
```

```
axiom_1: system 'EmergencySource' handler 'trigger' references
'Car' -- not a parent / child / local / state / transition name
or a4 builtin
```

### Flaws 1/2, skip-level down: reach past child to grandchild

```a4
system Dispatcher
    parent BuildingSystem
    children Car
    on malicious-direct-drive -> ;
        Motor move-to
end
```

```
axiom_1: ... references 'Motor' -- not in scope
```

### Stateful body rejected

Any content in a stateful system's handler body is an axiom
violation. This subsumes the older Flaw 1/2 "subsystem call inline"
rejection: with event-driven semantics, bodies cannot exist on
stateful systems at all.

```a4
system C
    state CState
    on step -> ;
        -> Ready
    transitions
        Idle -> Ready step
end
```

```
axiom_5: system 'C' handler 'step' has a non-empty body; stateful
systems are driven by the transitions table (one event fires one
transition). Delete the body and declare transitions for this
trigger instead.
```

### Flaw 3: forged event name

```a4
system Car
    parent Dispatcher
    on step -> ;
        check-emergency-flag
end
```

```
axiom_1: ... references 'check-emergency-flag' -- not in scope
```

The polled-boolean pattern has no home in the tree.

### Parent/children consistency, either direction

```a4
system Door parent Somewhere end
system Car parent Dispatcher children Door end
```

```
HOS parent/children consistency errors:
  system 'Car' lists child 'Door', but 'Door' declares
  parent='Somewhere' not 'Car'
```

Bidirectional on every registration.

### Effect target out of scope

```a4
system C
    parent P
    state CState
    on go -> ;
    transitions
        Idle -> Done go : Stranger step
end
```

```
axiom_1: system 'C' declares transition Idle -> Done under
trigger 'go' with effect on 'Stranger', but 'Stranger' is not in
scope
```

### Timer scheduling an undeclared event

```a4
system M
    parent P
    state MState
    on go -> ;
    transitions
        Idle -> Running go : after 100 ghost
end
```

```
axiom_1: system 'M' declares timer effect Idle -> Running under
trigger 'go' scheduling event 'ghost', but 'ghost' is not a
declared handler on this system
```

---

## 6. The happy path

One floor-button-press event at the root drives a full assign
cycle through the tree in roughly 1.2 seconds of wall clock time.

```
Time    Event                              State transitions
----    -----                              -----------------
0ms     floor-button-press at BuildingSystem
        BuildingSystem -> Dispatcher request
        Dispatcher -> Car assign
        Car (IdleAtFloor -> PreparingToMove) -> Door close
        Door idempotent self-loop on Closed -> Car closed
        Car (PreparingToMove -> Moving) -> Motor move-to
        Motor (Stopped -> Moving), schedule arrived @500ms

500ms   Motor.arrived fires (self-timer)
        Motor (Moving -> Stopped) -> Car arrived
        Car (Moving -> Arriving), schedule settled @200ms

700ms   Car.settled fires (self-timer)
        Car (Arriving -> DoorOpening) -> Door open
        Door (Closed -> Opening), schedule opened @100ms

800ms   Door.opened fires
        Door (Opening -> Open) -> Car opened
        Car (DoorOpening -> Loading), schedule depart @300ms

1100ms  Car.depart fires
        Car (Loading -> DoorClosing) -> Door close
        Door (Open -> Closing), schedule closed @100ms

1200ms  Door.closed fires
        Door (Closing -> Closed) -> Car closed
        Car (DoorClosing -> IdleAtFloor) -- cycle complete
```

Each step is one event, one transition, one actor message cycle.
The safety invariant (door does not open until motor has stopped
and the car has settled) holds by construction: Car's
`Moving -> Arriving` transition is triggered by `arrived`, which
only arrives after Motor enters Stopped; the subsequent
`Arriving -> DoorOpening` is triggered by the `settled` self-timer
and is the only transition that emits `Door open`. An FAT test
asserts this directly: at 400ms into the cycle, Door's log
contains only `close` and Motor's contains only `move-to`; Door
has not yet received `open` because Car has not yet received
`arrived`, much less `settled`.

### Emergency preemption

A fire-alarm at any point in the cycle routes through
`BuildingSystem -> EmergencySource trigger -> BuildingSystem
set-emergency -> Dispatcher set-emergency -> Car trigger-emergency
-> Motor stop`. Each hop is a declared transition in a separate
actor. If Motor is mid-travel (scheduled arrived outstanding), the
stop arrives first because Motor's mailbox is FIFO; Motor
transitions `Moving -> Stopped stop` with no upward signal; the
late scheduled `arrived` eventually reaches Motor in Stopped and
is input-rejected. An FAT test asserts Motor's log in this
scenario: `[move-to, stop, arrived]`, with Motor stopped at the end.

#### Why preemption is correct

The preemption guarantee rests on three structural properties,
each load-bearing:

1. **BEAM mailbox serialization.** Each actor process owns a FIFO
   mailbox. Messages sent to an actor are delivered in send order
   and processed one at a time. No two events can "cross" inside
   one actor; no partial-state observation is possible between
   transitions. This is a property of the runtime, not the DSL,
   and it is what lets us reason about preemption at all.

2. **Axiom 4 input rejection.** In any state, an event that has no
   declared transition from the current state is silently dropped.
   A late scheduled self-send (the `arrived` timer) arriving at
   Motor in `Stopped` has no declared transition and is ignored.
   The actor does not crash, does not move, does not raise; the
   stale event simply vanishes. Without this property, any
   preempted system would leave a ticking bomb in its mailbox.

3. **Exhaustive emergency entries** (checked at compile time).
   Every non-emergency state of Car declares a transition under
   `trigger-emergency` into `EmergencyStopped`. This is the
   property most easily broken by hand and is enforced by the
   checker: a trigger whose transitions converge on a single
   target state is classified as an emergency trigger, and every
   state that is neither the target nor a recovery-path state
   downstream of the target must declare the transition. A missing
   row is a compile error, not a runtime silence.

The combination is the whole argument. Without (1), two events
could interleave mid-transition. Without (2), the deferred
`arrived` would either crash Motor or restart travel. Without (3),
the emergency could arrive in a state Car does not handle and
would be silently dropped. With all three, preemption is
structural: a fire-alarm in flight at any point in the cycle is
caught at Car, Motor is stopped, and the stale travel completion
is discarded. The runtime FAT test demonstrates the end-to-end
trace; the compiler check is what guarantees no hole is hiding in
a state the test did not exercise.

---

## 7. What this means, honestly

Sound:

- **Flaw 1/2 cannot be hidden in a handler body** because stateful
  systems have no handler bodies. Subsystem interactions are
  declared on transitions. A door-before-motor reordering is a
  visible edit of the transitions table, which is where a safety
  review lands.
- **Flaw 1/2 also cannot race past subsystems at runtime.** Car's
  transition to Arriving is triggered by Motor's `arrived` signal,
  and Arriving only advances to DoorOpening on the `settled`
  self-timer. Car cannot proceed until each signal arrives.
  Removing the cascade eliminated the race that a synchronous
  multi-marker body had introduced.
- **Flaws 3 and 5 are Axiom 1 scope violations.** The scope check
  runs on both handler body tokens (for stateless systems) and
  effect targets (for all systems). Transition triggers are also
  scope-checked: a trigger that is not a declared handler on this
  system is a compile error, so a typo cannot silently vanish.
- **Flaw 4** as "body declares a transition the table does not"
  no longer applies because there are no bodies on stateful
  systems. The analogous check is "every declared transition is
  in the table by construction" which is trivially true.
- **Every declared state is reachable** from the initial state via
  some chain of transitions. Unreachable states are rejected at
  compile time as axiom_5 violations.
- **Every emergency state is covered.** A trigger whose transitions
  all converge on a single target state is classified as an
  emergency trigger; every non-emergency state must declare a
  transition into the target under that trigger, or the spec is
  rejected. This is the compile-time half of the emergency
  preemption correctness argument.
- **Parent/children consistency** runs bidirectionally on every
  registration.
- **Emergency routing is declared** via BuildingSystem and
  EmergencySource stateful transitions, and preempts Car's work
  because each Car step is its own message cycle.

Scoped, honestly:

- **Dispatcher's self-loop is a smell.** `Normal -> Normal request
  : Car assign` is a transition that exists only to carry an
  effect. Dedicated "internal transition" syntax (Harel /UML) is
  future work.
- **Idempotent self-loops on Door** (`Closed -> Closed close`,
  `Open -> Open open`) exist to unblock a waiting parent when the
  child is already in the target state. A future version could
  make this implicit via a "signal completion even on no-op"
  rule, but for now the idempotent rows are explicit.
- **Multi-car dispatcher selection.** The spec has one Car. A
  production dispatcher would hold N cars and pick one atomically.
  Needs a collection primitive.
- **Cross-system preconditions.** A transition's effect target is
  scope-checked but not precondition-checked. If a transition
  declares `: Door open` when the logical precondition is "Motor
  is stopped", the language does not enforce that precondition
  today. The event-driven synchronisation catches it at runtime
  (Car can't reach the Arriving transition without the arrived
  signal, nor DoorOpening without settled), but a compile-time
  check would be stronger.
- **Data propagation.** Handler signatures carry no arguments.
  Motor does not know which floor it's supposed to travel to. v2
  will address this via state fields; the v1 spec demonstrates
  control topology only.
- **Physical actuation beyond time.** The timer primitive covers
  "this takes N milliseconds." More complex actuation (sensors,
  external I/O, physical feedback) needs additional primitives.
  Only the time dimension is covered.
- **Physical settling time is a placeholder.** The `Arriving`
  state uses a 200ms timer as a stand-in for "mechanical
  alignment complete." Real hardware would signal via sensor
  input, routed as an upward event from a future `Sensor`
  subsystem. The timer keeps the control topology the same.

---

## 8. Running it yourself

```bash
rebar3 shell
1> af_repl:init_types().
2> af_type_compiler:clear_pending_checks().
3> af_hos_check:clear_registry().
4> {ok, C} = file:read_file("samples/hos/elevator/elevator.a4").
5> af_interpreter:interpret_tokens(
       af_parser:parse(binary_to_list(C), "elevator"),
       af_interpreter:new_continuation()).
```

The FAT suite:

```bash
rebar3 eunit --module=af_hos_elevator_tests
```

Twenty-one tests.
Four happy-path (compiles, all systems registered, tree spawns,
event routes). Ten compile-time rejections (sibling, skip-level,
stateful body, forged event, consistency in two directions,
parallel dispatcher, effect target out of scope, timer event
undeclared, and one Flaw-4-legacy body rejection). Four
event-dispatch and safety tests (full cycle ordering, safety
invariant timing, fire-alarm to motor-stop, mid-assign emergency
preemption). Three timer-specific tests (scheduled delay,
preemption via stop, timer event undeclared).

---

## 9. Honest numbers

The a4 HOS elevator spec is 233 lines including comments. The
Rust reference runs ~406 code lines for single-car. Stripping
comments and blanks from the a4 spec, roughly 93 code lines: a
~4x reasoning-surface reduction, for the single-car case.

Multi-car will add rows to the transitions table plus a collection
primitive. I'd estimate 140 to 160 code lines when that lands.
Rust stays at ~420 for multi-car. Ratio around 2.5x to 3x.

The real claim is not the ratio. It is that:

- The six classic flaws are either compile-time rejections or
  structurally unrepresentable in the spec.
- Runtime synchronisation between Car and its children is
  structural, not asserted by runtime checks.
- Emergency preemption works by construction, because each step
  of the control flow is its own message cycle.

Hamilton's Axiom 1 plus effects-in-transitions plus
one-event-one-transition plus Axiom 4 input rejection compose to a
system where the failure modes from the reference implementations
either do not typecheck or cannot race. That is a real structural
claim, earned incrementally over several passes.

---

## Appendix: files

- `samples/hos/elevator/elevator.a4` - the valid spec.
- `samples/hos/elevator/violations.a4` - each violation guarded as
  a comment block. Uncomment to observe the rejection.
- `src/af_hos_check.erl` - axiom checker.
- `src/af_hos_dsl.erl` - the `system ... end` DSL surface.
- `src/af_hos_runtime.erl` - actor-tree runtime with one-event-
  one-transition dispatch.
- `lib/hos/system.a4` - `SystemNode`, `HandlerSpec`, and
  `TransitionSpec` product types.
- `test/af_hos_elevator_tests.erl` - 21-test FAT suite.
- `test/af_hos_dsl_tests.erl` - DSL surface tests.
