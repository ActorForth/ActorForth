---
title: ActorForth + HOS
sub_title: Higher Order Software, structurally enforced at compile time
author: ""
theme:
  name: light
---

# Why HOS

System-of-Systems software where the cost of an unexpected event
is measured in lives or money: lifts, payment terminals, signalling,
medical devices, protocol state.

Margaret Hamilton's discipline. Apollo. The ancestor of formal
state-machine design.

The a4 surface puts that discipline in your editor, with a checker
that runs at compile time.

<!-- speaker_note: 30 seconds. Set the stage. Audience is BEAM-literate. -->

<!-- end_slide -->

# We will build one HOS system slowly

The smallest interesting system in the elevator example is `Door`.

We will add one line at a time, naming each new concept as it
appears. By the end you will have read every keyword the DSL uses.

After Door, we will look at the rest of the elevator (Motor, Car,
Dispatcher, BuildingSystem, EmergencySource) full source per slide,
no further explanation.

Then three classes of defect, each shown twice: as the author wrote
it, then with the violation highlighted and Hamilton's classification
on screen.

Then the spec running under FAT, the line counts vs Python / OCaml /
Rust, and a closing.

<!-- speaker_note: 20 seconds. Tell them what is coming. -->

<!-- end_slide -->

# The HOS DSL is pure a4

Before we start, the architectural claim:

The DSL is **a sequence of registered ActorForth operations**, not a
text format parsed by a hidden Erlang routine. Every keyword in the
slides that follow is an op in the type registry. The dispatcher IS
the parser.

```forth
"lib/hos/dsl.a4" load
```

This loads the DSL definitions: type declarations for `SystemDef`,
`Row`, `TransitionsBlock`, `Timer`; multi-clause ops for `system`,
`parent`, `fsm`, `state`, `event`, `child`, `transitions`, `trans`,
`;`, `.`. About 220 lines of pure a4 plus three tiny Erlang
primitives (`tag-name`, `register-pusher`, `nip`) and one finaliser
hook (`hos-finalise`) that bridges to the axiom checker.

There is no sectioning parser. There is no token buffer. Each token
dispatches through the type registry the same way `dup` does.

<!-- speaker_note: This is the whole point. The DSL is a4. The teaching slides that follow show this concretely. -->

<!-- end_slide -->

# Door, step 1: opening the system block

```forth +line_numbers
Door system
```

`Door` is a token. The interpreter searches the dispatch chain:
TOS dict → TOS handler → Any dict → literals → Atom-push fallback.
Nothing matches `Door` — the stack is empty, no handler, no Any
op, not a literal. It falls through and is pushed as `{Atom, "Door"}`.

`system` is a registered op on `Atom` with signature `( Atom -> SystemDef )`.
TOS is now `Atom`, dispatch finds `system` on the Atom dict, fires it.
Body in `lib/hos/dsl.a4`:

```forth
: system Atom -> SystemDef ;
    empty-systemdef .
```

`empty-systemdef` consumes the Atom and uses the auto-generated
`systemdef` constructor to build a flat-tuple `SystemDef` with the
name field set and everything else at defaults.

Stack after the line: `( SystemDef )`.

<!-- speaker_note: The values-first ordering. Door (the value) comes first, then `system` (the operation that consumes it). Same shape as `5 dup`. -->

<!-- end_slide -->

# Door, step 2: declaring the parent

```forth +line_numbers
Door system
    Car parent
```

`Car`: not a registered op yet. Pushes as `{Atom, "Car"}`.

`parent`: registered as a multi-clause op. The clause that fires
when TOS is Atom does TWO things in one body:

```forth
: parent SystemDef Atom -> SystemDef ;
    dup
    "Any" to-atom swap "Subsystem" to-atom register-pusher
    parent-name! .
```

1. `register-pusher` installs a no-arg op named `Car` on the Any
   dictionary. From this point on, the token `Car` dispatches as
   that op and pushes `{Subsystem, "Car"}`.
2. `parent-name!` is the auto-generated setter for SystemDef's
   `parent-name` field. Sets it to `"Car"`.

Door is positioned in the HOS hierarchy: its parent is `Car`. Axiom 1
will permit Door's transitions to reference `Car` and nothing else
above the tree.

<!-- speaker_note: register-pusher is the trick that lets state and event names dispatch through the dictionary instead of being intercepted by a greedy handler. -->

<!-- end_slide -->

# Door, step 3: declaring the FSM type

```forth +line_numbers
Door system
    Car parent
    DoorState fsm
```

`DoorState` pushes as Atom. `fsm` is a one-clause op:

```forth
: fsm SystemDef Atom -> SystemDef ;
    state-name! .
```

It just sets the `state-name` field. No pusher: the FSM type name
is metadata, not a value the user references later.

`DoorState` is a label. There is no separate `type DoorState ...`
declaration anywhere. The actual state values (`Closed`, `Opening`,
`Open`, `Closing`) come next, declared one at a time.

A system that calls `fsm` is **stateful**: it is driven by its
transitions table, not by handler bodies. (Stateless systems omit
`fsm`.)

<!-- end_slide -->

# Door, step 4: declaring states

```forth +line_numbers
Door system
    Car parent
    DoorState fsm
    Closed state  Opening state  Open state  Closing state
```

`Closed`: not yet a registered op. Pushes as Atom.
`state`: multi-clause. The Atom-input clause:

```forth
: state SystemDef Atom -> SystemDef ;
    "Any" to-atom swap "State" to-atom register-pusher .
```

Installs a no-arg op `Closed` on Any that pushes `{State, "Closed"}`.
SystemDef passes through unchanged (the FSM type is a single label
declared by `fsm`; the state names are just made dispatchable).

Same pattern for `Opening`, `Open`, `Closing`.

After this line, four more pusher ops live in the dictionary. When
any of those names appears as a token, it dispatches and pushes a
typed `State` value.

<!-- speaker_note: Four state declarations in one line. Each is two tokens, each is one ordinary dispatch. No special parsing. -->

<!-- end_slide -->

# Door, step 5: declaring events

```forth +line_numbers
Door system
    Car parent
    DoorState fsm
    Closed state  Opening state  Open state  Closing state
    open event    close event    opened event  closed event
```

`open` pushes as Atom. `event` (Atom-input clause) installs an `open`
op on Any that pushes `{Event, "open"}`, AND appends `open` to the
SystemDef's `events-list` field:

```forth
: event SystemDef Atom -> SystemDef ;
    dup
    "Any" to-atom swap "Event" to-atom register-pusher
    events-list swap cons events-list! .
```

The events-list becomes the system's declared handler set. The HOS
axiom checker uses it to validate timer effects and to enforce input
rejection (Hamilton's Axiom 4): events not in this list are silently
rejected at runtime.

After this line, four event names dispatch as Event values, and the
SystemDef carries the four event names in its events-list.

<!-- end_slide -->

# Door, step 6: opening the transitions block

```forth +line_numbers
Door system
    Car parent
    DoorState fsm
    Closed state  Opening state  Open state  Closing state
    open event    close event    opened event  closed event

    transitions
```

`transitions` is a one-clause op `( SystemDef -> TransitionsBlock )`:

```forth
: transitions SystemDef -> TransitionsBlock ;
    nil
    transitionsblock .
```

It uses the auto-generated `transitionsblock` constructor to wrap
the SystemDef into a `TransitionsBlock` value with an empty rows
list. The original SystemDef is now stashed inside the TB's
`enclosing-sys` field.

Stack: `( TransitionsBlock )`.

<!-- end_slide -->

# Door, step 7: a basic transition row

```forth +line_numbers
Door system
    Car parent
    DoorState fsm
    Closed state  Opening state  Open state  Closing state
    open event    close event    opened event  closed event

    transitions
        Closed  Opening  open  trans
```

Each token in this row dispatches through the dictionary:

- `Closed` → registered State pusher → pushes `{State, "Closed"}`.
- `Opening` → State pusher → pushes `{State, "Opening"}`.
- `open` → registered Event pusher → pushes `{Event, "open"}`.
- `trans` → multi-clause op. The basic-row clause matches stack
  pattern `( TB State State Event -> TB )` and uses an Erlang impl
  to build a `Row` and prepend it to the TB's rows-list.

No syntactic special-casing. Each token is an ordinary op dispatched
on the type registry.

<!-- speaker_note: Multi-clause `trans` is the key. Pattern matching on stack types picks the right row builder. -->

<!-- end_slide -->

# Door, step 8: a transition with a timer effect

```forth +line_numbers
Door system
    Car parent
    DoorState fsm
    Closed state  Opening state  Open state  Closing state
    open event    close event    opened event  closed event

    transitions
        Closed  Opening  open  100 after opened  trans
```

Add a timer clause: `100 after opened`.

- `100` pushes Int.
- `after` is a one-clause op `( Int -> Timer )` whose body uses
  the auto-generated `Timer` constructor.
- `opened` pushes Event.

Stack at `trans`: `( TB State State Event Timer Event )`.

`trans` has a clause with that exact sig. Its impl builds a Row
with the effect-target slot set to "after" and the delay slot set
to 100. At runtime, the Door system schedules an `opened` self-send
100ms after this transition fires.

<!-- end_slide -->

# Door, step 9: a transition with a parent-effect

```forth +line_numbers
Door system
    Car parent
    DoorState fsm
    Closed state  Opening state  Open state  Closing state
    open event    close event    opened event  closed event

    transitions
        Closed   Opening  open    100 after opened  trans
        Opening  Open     opened  Car opened        trans
```

`Car` already dispatches as `{Subsystem, "Car"}` (registered when we
said `Car parent`). `opened` dispatches as `{Event, "opened"}`.

Stack at the second `trans`: `( TB State State Event Subsystem Event )`.

A different `trans` clause matches that pattern and builds a row
where the effect dispatches `opened` to Car. This is an upward
signal from child to parent.

The Axiom 1 scope check permits Car here because Car is Door's
declared parent. Sibling references would fail the same check.

<!-- end_slide -->

# Door, step 10: complete

```forth +line_numbers
Door system
    Car parent
    DoorState fsm
    Closed state  Opening state  Open state  Closing state
    open event    close event    opened event  closed event

    transitions
        Closed   Opening  open      100 after opened   trans
        Opening  Open     opened    Car opened         trans
        Open     Closing  close     100 after closed   trans
        Closing  Closed   closed    Car closed         trans
        Closed   Closed   close     Car closed         trans
        Open     Open     open      Car opened         trans
    ;
.
```

Two new ops finish the block:

- `;` is registered on TransitionsBlock. Body uses `enclosing-sys`
  and `rows` (auto-bound from the TB) plus `nip` to leave just the
  updated SystemDef on the stack.
- `.` is registered on SystemDef. Body calls `hos-finalise`, an
  Erlang-impl op that converts the SystemDef product into a
  `HosBlueprint`, runs the axiom checker, and registers the system.

If the axiom checker rejects the spec, `hos-finalise` raises
`{axiom_violation, Msg}` and the source fails to compile.

Door is now a registered HOS system.

<!-- speaker_note: ; and . are not special syntax. They are ordinary ops registered on TB and SystemDef respectively, dispatched by the same mechanism as `dup` or `+`. -->

<!-- end_slide -->

# Motor

```forth +line_numbers
Motor system
    Car parent
    MotorState fsm
    Stopped state  Moving state
    move-to event  arrived event  stop event
    transitions
        Stopped  Moving   move-to   500 after arrived  trans
        Moving   Stopped  arrived   Car arrived        trans
        Moving   Stopped  stop                         trans
    ;
.
```

<!-- speaker_note: Same shape as Door. The `stop` row has no effect clause; the parent (Car) issued the stop and already knows the motor is stopped. -->

<!-- end_slide -->

# Car

```forth +line_numbers
Car system
    Dispatcher parent
    CarState fsm
    Door child  Motor child
    IdleAtFloor state    PreparingToMove state
    Moving state         Arriving state
    DoorOpening state    Loading state
    DoorClosing state    EmergencyStopped state
    EmergencyDoorOpen state
    assign event   closed event   opened event   arrived event
    settled event  depart event
    trigger-emergency event  clear-emergency event
    transitions
        IdleAtFloor      PreparingToMove   assign            Door close          trans
        PreparingToMove  Moving            closed            Motor move-to       trans
        Moving           Arriving          arrived           200 after settled   trans
        Arriving         DoorOpening       settled           Door open           trans
        DoorOpening      Loading           opened            300 after depart    trans
        Loading          DoorClosing       depart            Door close          trans
        DoorClosing      IdleAtFloor       closed                                trans
        # plus 7 emergency rows and the clear-emergency path
    ;
.
```

<!-- speaker_note: New decl `child`. Same multi-clause shape as `parent`: registers a Subsystem pusher AND appends to children-list. The hierarchy is now visible: Car owns Door and Motor, Car is owned by Dispatcher. -->

<!-- end_slide -->

# EmergencySource

```forth +line_numbers
EmergencySource system
    BuildingSystem parent
    EmergencyState fsm
    Idle state  Active state
    trigger event  clear event
    transitions
        Idle    Active  trigger  BuildingSystem set-emergency   trans
        Active  Idle    clear    BuildingSystem clear-emergency trans
    ;
.
```

<!-- speaker_note: A small two-state source. Routes external events upward to BuildingSystem. The `set-emergency`/`clear-emergency` events haven't been declared yet (they're BuildingSystem's events, declared later); they push as Atoms and `trans` matches the Subsystem-Atom clause. The axiom checker validates them at register time. -->

<!-- end_slide -->

# Dispatcher

```forth +line_numbers
Dispatcher system
    BuildingSystem parent
    DispatcherState fsm
    Car child
    Normal state  Emergency state
    request event  set-emergency event  clear-emergency event
    transitions
        Normal     Normal     request          Car assign              trans
        Normal     Emergency  set-emergency    Car trigger-emergency   trans
        Emergency  Normal     clear-emergency  Car clear-emergency     trans
    ;
.
```

<!-- speaker_note: The `Normal Normal request` row is an internal transition (Harel/UML terminology): state does not change, but the effect dispatches Car assign. -->

<!-- end_slide -->

# BuildingSystem

```forth +line_numbers
BuildingSystem system
    BuildingState fsm
    Dispatcher child  EmergencySource child
    Normal state  Emergency state
    floor-button-press event  fire-alarm event  fire-clear event
    set-emergency event       clear-emergency event
    transitions
        Normal     Normal     floor-button-press  Dispatcher request           trans
        Normal     Normal     fire-alarm          EmergencySource trigger      trans
        Normal     Emergency  set-emergency       Dispatcher set-emergency     trans
        Emergency  Normal     clear-emergency     Dispatcher clear-emergency   trans
        Emergency  Emergency  fire-clear          EmergencySource clear        trans
    ;
.
```

<!-- speaker_note: Root of the tree. No `parent` — it's the top-level system. Children are Dispatcher (the elevator scheduler) and EmergencySource (the fire alarm). -->

<!-- end_slide -->

# Defect 1, phase 1: a system reaching across the tree

```forth +line_numbers
"../../../../../lib/hos/dsl.a4" load

BadEmergency system
    BuildingSystem parent
    EmergencyState fsm
    Idle state  Active state
    trigger event
    transitions
        Idle  Active  trigger  Car trigger-emergency  trans
    ;
.
```

`BadEmergency` declares `BuildingSystem` as its parent. Inside the
transition row, it references `Car`.

`Car` isn't declared: not a parent, not a child, not in this system's
event or state set. It pushes as Atom. The Atom-target `trans` clause
fires and builds a Row whose effect target is the literal string `"Car"`.

What happens at finalisation?

<!-- speaker_note: Walk the audience through. The dispatcher accepts the row syntactically; the axiom checker decides whether the names actually mean anything in this system's scope. -->

<!-- end_slide -->

# Defect 1, phase 2: AXIOM 1 violation

```forth {9|all} +line_numbers
"../../../../../lib/hos/dsl.a4" load

BadEmergency system
    BuildingSystem parent
    EmergencyState fsm
    Idle state  Active state
    trigger event
    transitions
        Idle  Active  trigger  Car trigger-emergency  trans
    ;
.
```

## Hamilton AXIOM 1: Immediate Control (Scope)

A system may reference its parent, its declared children, its own
state names, its own events, and a4 builtins. Nothing else.

`BadEmergency` reaches across the tree to `Car` (its sibling under
`BuildingSystem`). Sibling back-channels are how control hierarchies
decay: subsystems coordinate without going through their common
parent, the dependency graph becomes a mesh, and reasoning about
local behaviour stops working.

The compiler refuses to register this system.

```bash +exec_replace
samples/hos/elevator/comparison/exploits/a4_check.escript \
    samples/hos/elevator/comparison/exploits/flaw_5_bad.a4 2>&1 | head -5
```

<!-- end_slide -->

# Defect 2, phase 1: a stateful car with a stop event

```forth +line_numbers
"../../../../../lib/hos/dsl.a4" load

BadCar system
    BadCarState fsm
    Idle state  Moving state  Loading state  Halted state
    assign event  done event  stop event  clear event
    transitions
        Idle     Moving    assign  trans
        Moving   Loading   done    trans
        Loading  Idle      done    trans

        Idle     Halted    stop    trans
        Moving   Halted    stop    trans

        Halted   Idle      clear   trans
    ;
.
```

`stop` is the emergency trigger. Look at the transitions table.
Which states does it cover? Which does it not?

<!-- speaker_note: Pause and let the audience see the gap. Some will catch it; some will not. -->

<!-- end_slide -->

# Defect 2, phase 2: AXIOM 5 violation

```forth {12,13|all} +line_numbers
"../../../../../lib/hos/dsl.a4" load

BadCar system
    BadCarState fsm
    Idle state  Moving state  Loading state  Halted state
    assign event  done event  stop event  clear event
    transitions
        Idle     Moving    assign  trans
        Moving   Loading   done    trans
        Loading  Idle      done    trans

        Idle     Halted    stop    trans
        Moving   Halted    stop    trans

        Halted   Idle      clear   trans
    ;
.
```

## Hamilton AXIOM 5: Input Rejection (Exhaustive Emergency)

Every emergency trigger must be honoured from every reachable
state, or the system can silently swallow the event.

`BadCar` declares `stop` from `Idle` and `Moving`. `Loading` is
reachable (`Moving Loading done trans`) but has no `stop` row. An
emergency arriving while the car is in Loading is input-rejected
without notice. Dropped fire alarms.

The exhaustive-emergency check finds the missing row at compile
time. The author sees the gap before the fire.

```bash +exec_replace
samples/hos/elevator/comparison/exploits/a4_check.escript \
    samples/hos/elevator/comparison/exploits/flaw_3_bad.a4 2>&1 | head -5
```

<!-- end_slide -->

# Defect 3: the stateful body rule is now structural

The Hamilton stateful-body rule says: a stateful system is driven
by its transitions table, not by handler body code. The OLD keyword
DSL had `on EVENT -> ; <body>` syntax and an axiom check rejected
non-empty bodies on stateful systems.

The values-first DSL removes the trap entirely. Event declarations
have no body slot:

```forth
open event
```

Two tokens. The first pushes as Atom; the second registers an Event
pusher and appends to the system's events-list. There is no
syntactic place to put body code on a stateful event.

The structural claim: a class of bug that needed a runtime axiom
check in the OLD DSL has become **unconstructible** in the new one.
The DSL itself is the proof.

<!-- speaker_note: Hamilton's discipline doesn't change. What changes is which checks the DSL does syntactically vs which the axiom pass does at register time. -->

<!-- end_slide -->

# The same checker, the valid spec, a live FAT

```bash +exec_replace
rebar3 eunit --module=af_hos_elevator_tests 2>&1 \
    | sed -E 's/\x1b\[[0-9;]*m//g' \
    | grep -E "All [0-9]+ tests passed|Failed:" \
    | head -2
```

`samples/hos/elevator/elevator.a4` (the equivalent spec in the OLD
keyword DSL, runtime-tested for years) passes every axiom check,
then runs through 21 functional acceptance tests covering door
open and close, motor travel, the two-elevator dispatch, and
emergency routing.

The values-first equivalent
(`samples/hos/elevator/talk/elevator_a4_native.a4`) compiles to the
same `HosBlueprint` and the same axioms apply. Whether the spec is
rejected or accepted is decided at compile time, not at runtime.

<!-- end_slide -->

# Same controller, four languages

```bash +exec_replace
for f in samples/hos/elevator/elevator.a4 \
         samples/hos/elevator/comparison/elevator_naive.py \
         samples/hos/elevator/comparison/elevator_correct.py \
         samples/hos/elevator/comparison/elevator_naive.ml \
         samples/hos/elevator/comparison/elevator_correct.ml \
         samples/hos/elevator/comparison/elevator_naive.rs \
         samples/hos/elevator/comparison/elevator_correct.rs
do
    n=$(wc -l < "$f")
    printf "%-40s %5s\n" "$(basename "$f")" "$n"
done
echo
echo "a4 non-comment / non-blank lines: $(awk 'NF && !/^[[:space:]]*#/' samples/hos/elevator/elevator.a4 | wc -l)"
```

The a4 spec is 93 non-comment lines. The smallest correct reference
(Python) is 444. Ratio about 4.7x.

The a4 version is also the only version whose correctness properties
are enforced at compile time rather than defended by hand.

<!-- end_slide -->

# Three defects, three Hamilton axioms

| Defect | Hamilton classification | Where caught |
|---|---|---|
| Sibling back-channel | AXIOM 1 (Scope) | axiom checker, at `.` |
| Emergency coverage gap | AXIOM 5 (Input Rejection) | axiom checker, at `.` |
| Stateful handler body code | Stateful Body Rule | DSL syntax, by construction |

Every one of these is a class of bug in real systems-of-systems
code. Every one of these the a4 toolchain refuses to compile, by
construction.

The structural claim:

- *Declarations* tell you what the system can do.
- The *transitions* table tells you when.
- The *axioms* tell you what cannot be expressed.
- The *DSL itself* is a4: ops, types, multi-clause dispatch.

There is no fourth thing.

<!-- speaker_note: Land the closing. The structural claim is the takeaway. Pause at the end. -->
