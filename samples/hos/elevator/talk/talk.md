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

# Door, step 1: opening the system block

```forth +line_numbers
system Door
```

`system` is the only entry point of the HOS DSL.

- **Kind:** ActorForth operation, registered on the global `Any` dictionary.
- **Signature:** `( -> SystemDef )`
- **Implementation:** Erlang, in `src/af_hos_dsl.erl`.

`system` pushes a fresh `SystemDef` value onto the stack. Now look
at how `SystemDef` itself is declared:

```erlang
af_type:register_type(#af_type{
    name    = 'SystemDef',
    ops     = #{},
    handler = fun handle_system_def/2
}).
```

The dictionary `ops` is empty. *No operations are registered on
SystemDef.* Every token that hits a SystemDef on TOS finds nothing
in the dict and goes straight to the handler.

So what about `Door`? The interpreter looks for an op called `Door`
on SystemDef (none), then on Any (none), then literal handlers
(none), then falls through to the SystemDef handler. The handler
appends `Door` to the buffer as raw text.

There is no special "name" syntax. `Door` is simply *the first token
captured.* At finalisation, the parser reads the first captured
token as the system's name. Whatever name you put after `system`
becomes the name of the system being defined.

<!-- speaker_note: This is the key mechanism. The same handler-on-a-type pattern any user can apply to define a DSL. Tie back to slide 6 of the outline. -->

<!-- end_slide -->

# Door, step 2: declaring the parent

```forth +line_numbers
system Door
  parent Car
```

`parent` is a section keyword.

- **Kind:** Plain text token. *Not* an a4 operation.
- **Captured by:** the `SystemDef` handler.
- **Parsed at:** finalisation time, by `parse_system_tokens` (Erlang).
- **Pattern:** `parent <SystemName>`

The `SystemDef` handler stores `parent`, then `Car`, into the
buffer. When the block finalises, the parser recognises `parent`
as a section marker and reads the next token as the parent's name.

This declares Door's position in the HOS hierarchy: its parent in
the control tree is `Car`. Hamilton's Axiom 1 then permits Door's
body to reference `Car` and nothing else above the tree.

<!-- end_slide -->

# Door, step 3: declaring state

```forth +line_numbers
system Door
  parent Car
  state DoorState
```

`state` is a section keyword.

- **Kind:** Plain text token, captured by handler.
- **Pattern:** `state <StateMachineName>`
- **Effect on the spec:** marks this system as **stateful**.

The handler captures `state`, then `DoorState`. The parser recognises
`state` as a section marker and reads the next token as the name
the author has chosen for this system's state machine.

`DoorState` is a label, nothing more. It is not a separately-defined
type and does not need a `type DoorState ...` declaration anywhere.
The actual state values (`Closed`, `Opening`, `Open`, `Closing`)
will appear in the transitions table later; the parser will collect
them automatically. `DoorState` is just the name you give to that
collection so error messages and documentation can refer to it.

A stateful system is driven by its transitions table. Every state
change must be a row in that table. The `on EVENT -> ;` declarations
that follow are documentation of the events the system knows about,
and they *must have empty bodies*. Driving is by the table, not by
code.

(Stateless systems use `state None` and play a different role:
routers and dispatchers that may have body code in their handlers.)

<!-- end_slide -->

# Door, step 4: declaring an event handler

```forth +line_numbers
system Door
  parent Car
  state DoorState

  on open -> ;
```

`on` opens a handler declaration. Four new tokens land on this line:

- `on` is a section keyword. Captured by the handler. The parser
  recognises it as the start of a handler block.
- `open` is the **event name**. The next token after `on`. This is
  the event Door will accept from outside.
- `->` separates the handler's input type signature from its output
  type signature. Both are empty here, so the arrow stands alone.
- `;` closes the handler header. What follows up to the next section
  is the handler **body**. Door has no body, so `;` and the next
  section keyword (or another `on`) sit back-to-back.

Pattern in full: `on <EventName> [<input-types>] -> [<output-types>] ; <body>`

Door declares it knows about the event `open`. The runtime will
accept `open` from outside, look up the matching row in the
transitions table, and fire that row. If no row matches the current
state, the runtime input-rejects the event (Hamilton's Axiom 4).

There is no body. On a stateful system, the body is the wrong place
for state changes: the transitions table is the law.

<!-- end_slide -->

# Door, step 5: the full event vocabulary

```forth +line_numbers
system Door
  parent Car
  state DoorState

  on open -> ;
  on close -> ;
  on opened -> ;
  on closed -> ;
```

Four events Door knows about:

- `open`, `close` — commands from `Car` (the parent).
- `opened`, `closed` — self-events from internal timers; also
  appear as upward signals to `Car` so Car can advance.

If anyone sends Door an event whose name is not in this list, the
runtime input-rejects it. Hamilton's Axiom 4. Silent rejection is
not an error condition; it is the intended behaviour. The receiver
*declares* what it accepts.

<!-- end_slide -->

# Door, step 6: opening the transitions block

```forth +line_numbers
system Door
  parent Car
  state DoorState

  on open -> ;
  on close -> ;
  on opened -> ;
  on closed -> ;

  transitions
```

`transitions` opens the table.

- **Kind:** Plain text section keyword.
- **What follows:** a sequence of transition rows until the block ends.

For a stateful system, every row is a state change the runtime
will accept. Anything not in this table cannot happen. The table
is the law.

<!-- end_slide -->

# Door, step 7: a basic transition row

```forth +line_numbers
system Door
  parent Car
  state DoorState

  on open -> ;
  on close -> ;
  on opened -> ;
  on closed -> ;

  transitions
    Closed -> Opening open
```

A transition row is three tokens: `From -> To trigger`.

- `Closed` — the source state (a state name, declared implicitly
  by appearing in this table)
- `Opening` — the destination state
- `open` — the trigger event (must be one of the `on` declarations
  on this system)

In English: when this system is in state `Closed`, an `open` event
moves it to state `Opening`. Nothing else happens. No side effects.

<!-- end_slide -->

# Door, step 8: a transition with a timer effect

```forth +line_numbers
system Door
  parent Car
  state DoorState

  on open -> ;
  on close -> ;
  on opened -> ;
  on closed -> ;

  transitions
    Closed -> Opening open : after 100 opened
```

Add an effect clause: `: after <ms> <event>`.

- **Kind:** part of the transitions section's row syntax.
- **Pattern:** `From -> To trigger : after <ms> <event>`
- **Runtime:** uses `send-after` (an a4 actor primitive) to schedule
  a self-send of `<event>` after `<ms>` milliseconds.

When this transition fires, the runtime schedules `opened` to be
delivered to *this same system* after 100 ms. The mailbox stays open
during the wait, so events arriving in the meantime are handled
normally and may pre-empt the timer.

This is the structural replacement for "hold a lock and sleep."

<!-- end_slide -->

# Door, step 9: a transition with a parent-effect

```forth +line_numbers
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
```

Effect form `: <Target> <event>`.

- **Pattern:** `From -> To trigger : <SystemName> <event>`
- **Runtime:** sends `<event>` to the named system.

When `Opening -> Open opened` fires, the runtime also sends `opened`
to `Car`. This is an upward signal: the child telling the parent
"I have completed the work you asked me to do."

The Axiom 1 scope check permits `Car` here because Car is Door's
declared parent. The same syntax (`: <Target> <event>`) works for
sending downward to a declared child.

<!-- end_slide -->

# Door, step 10: complete

```forth +line_numbers
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
```

`end` closes the block.

- **Kind:** *Not* an a4 op. It is a literal token name that the
  `SystemDef` handler intercepts as a special case.
- **Effect:** the handler stops collecting tokens, hands the buffer
  to the parser, runs the axiom checker, and registers the resulting
  `HosBlueprint`. The `SystemDef` accumulator is discarded.

Door is now a registered HOS system. From this point on, any other
spec can declare it as a child.

The last two transition rows handle redundant inputs (`close` while
already `Closed`, `open` while already `Open`) by self-looping with
the appropriate upward signal. That is structural: redundant events
are not errors; they are events that happen to leave the state
unchanged but still need to inform the parent.

<!-- end_slide -->

# Motor

```forth +line_numbers
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
```

<!-- speaker_note: Same shape as Door. The `stop` row has no effect clause; the parent (Car) issued the stop and already knows the motor is stopped. -->

<!-- end_slide -->

# Car

```forth +line_numbers
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
        Idle -> ClosingDoor assign : Door close
        ClosingDoor -> PreparingToMove closed : Motor move-to
        PreparingToMove -> Moving arrived : after 200 settled
        Moving -> Arriving settled : Door open
        Arriving -> DoorOpening opened : after 1500 depart
        DoorOpening -> Idle depart : Door close
        Idle -> Stopped trigger-emergency : Motor stop
        Stopped -> Stopped trigger-emergency
        Moving -> Stopped trigger-emergency : Motor stop
        ClosingDoor -> Stopped trigger-emergency : Motor stop
        PreparingToMove -> Stopped trigger-emergency : Motor stop
        Arriving -> Stopped trigger-emergency : Door open
        DoorOpening -> Stopped trigger-emergency
        Stopped -> Idle clear-emergency : Door open
end
```

<!-- speaker_note: New keyword `children Door Motor`. Pattern is `children <Name1> <Name2> ...`. Same shape otherwise. The hierarchy is now visible; Car owns Door and Motor, and Car is owned by Dispatcher. -->

<!-- end_slide -->

# EmergencySource

```forth +line_numbers
system EmergencySource
    parent BuildingSystem

    on trigger-fire-alarm -> ;

    transitions
        Active -> Active trigger-fire-alarm : BuildingSystem trigger-fire-alarm
end
```

<!-- speaker_note: A stateless-feeling source. Routes one external event upward to BuildingSystem. -->

<!-- end_slide -->

# Dispatcher

```forth +line_numbers
system Dispatcher
    parent BuildingSystem
    state DispatcherState
    children Car

    on floor-button-press -> ;
    on emergency -> ;
    on clear-emergency -> ;

    transitions
        Idle -> Idle floor-button-press : Car assign
        Idle -> Stopped emergency : Car trigger-emergency
        Stopped -> Stopped emergency
        Stopped -> Idle clear-emergency : Car clear-emergency
end
```

<!-- speaker_note: Two cars in the production spec; this slide shows one for clarity. The dispatcher owns Car and translates external floor-button events into assign commands. -->

<!-- end_slide -->

# BuildingSystem

```forth +line_numbers
system BuildingSystem
    state BuildingSystemState
    children Dispatcher EmergencySource

    on floor-button-press -> ;
    on trigger-fire-alarm -> ;
    on clear-emergency -> ;

    transitions
        Normal -> Normal floor-button-press : Dispatcher floor-button-press
        Normal -> Emergency trigger-fire-alarm : Dispatcher emergency
        Emergency -> Emergency trigger-fire-alarm
        Emergency -> Normal clear-emergency : Dispatcher clear-emergency
end
```

<!-- speaker_note: The root of the tree. No `parent` because this is the top-level system. Its children are Dispatcher (the elevator scheduler) and EmergencySource (the fire alarm). -->

<!-- end_slide -->

# Defect 1, phase 1: a system reaching across the tree

```forth +line_numbers
system BadEmergency
    parent BuildingSystem
    on trigger -> ;
        Car trigger-emergency
end
```

`BadEmergency` declares `BuildingSystem` as its parent. Inside the
handler body, it references `Car`.

Read the next slide and ask the checker what it thinks.

<!-- speaker_note: Walk the audience through. Where would the audience expect this to be rejected, if at all? -->

<!-- end_slide -->

# Defect 1, phase 2: AXIOM 1 violation

```forth {4|all} +line_numbers
system BadEmergency
    parent BuildingSystem
    on trigger -> ;
        Car trigger-emergency
end
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
system BadCar
    state BadCarState

    on assign -> ;
    on done -> ;
    on stop -> ;
    on clear -> ;

    transitions
        Idle -> Moving assign
        Moving -> Loading done
        Loading -> Idle done

        Idle -> Halted stop
        Moving -> Halted stop

        Halted -> Idle clear
end
```

`stop` is the emergency trigger. Look at the transitions table.
Which states does it cover? Which does it not?

<!-- speaker_note: Pause and let the audience see the gap. Some will catch it; some will not. -->

<!-- end_slide -->

# Defect 2, phase 2: AXIOM 5 violation

```forth {12,13|all} +line_numbers
system BadCar
    state BadCarState

    on assign -> ;
    on done -> ;
    on stop -> ;
    on clear -> ;

    transitions
        Idle -> Moving assign
        Moving -> Loading done
        Loading -> Idle done

        Idle -> Halted stop
        Moving -> Halted stop

        Halted -> Idle clear
end
```

## Hamilton AXIOM 5: Input Rejection (Exhaustive Emergency)

Every emergency trigger must be honoured from every reachable
state, or the system can silently swallow the event.

`BadCar` declares `stop` from `Idle` and `Moving`. Loading is
reachable (`Moving -> Loading done`) but has no `stop` row. An
emergency arriving while the car is in Loading is input-rejected
without notice. Dropped fire alarms.

The exhaustive-emergency check finds the missing row at compile
time. The author sees the gap before the fire.

```bash +exec_replace
samples/hos/elevator/comparison/exploits/a4_check.escript \
    samples/hos/elevator/comparison/exploits/flaw_3_bad.a4 2>&1 | head -5
```

<!-- end_slide -->

# Defect 3, phase 1: a stateful handler with body code

```forth +line_numbers
system BadStatefulBody
    state BadStatefulBodyState
    on go -> ;
        drop
    transitions
        Idle -> Running go
end
```

This system is stateful (`state BadStatefulBodyState` declares
that). The `on go -> ;` handler then carries body code (`drop`,
an a4 stack op).

The transitions table also declares `Idle -> Running go`.

Two mechanisms competing for one event. What should happen when
`go` arrives?

<!-- speaker_note: Pause. The ambiguity is the point. -->

<!-- end_slide -->

# Defect 3, phase 2: Stateful Body Rule violation

```forth {4|all} +line_numbers
system BadStatefulBody
    state BadStatefulBodyState
    on go -> ;
        drop
    transitions
        Idle -> Running go
end
```

## Hamilton: Stateful Body Rule

A system that declares a state is driven by its transitions table,
not by handler body code. The `on` declaration documents that the
system knows about the event; the table decides what happens.

The checker resolves the ambiguity by refusing the question:
stateful handlers must declare empty bodies. Effects belong on
transition rows; state changes belong in the transitions table.
No handler body code on stateful systems, full stop.

```bash +exec_replace
samples/hos/elevator/comparison/exploits/a4_check.escript \
    samples/hos/elevator/comparison/exploits/flaw_stateful_body.a4 2>&1 | head -5
```

<!-- end_slide -->

# The same checker, the valid spec, a live FAT

```bash +exec_replace
rebar3 eunit --module=af_hos_elevator_tests 2>&1 \
    | sed -E 's/\x1b\[[0-9;]*m//g' \
    | grep -E "All [0-9]+ tests passed|Failed:" \
    | head -2
```

`samples/hos/elevator/elevator.a4` passes every axiom check, then
runs through 21 functional acceptance tests covering door open
and close, motor travel, the two-elevator dispatch, and emergency
routing.

Same checker. Same axioms. Whether the spec is rejected or accepted
is decided at compile time, not at runtime.

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

| Defect | Hamilton classification |
|---|---|
| Sibling back-channel | AXIOM 1 (Scope) |
| Emergency coverage gap | AXIOM 5 (Input Rejection) |
| Stateful handler body code | Stateful Body Rule |

Every one of these is a class of bug in real systems-of-systems
code. Every one of these the a4 checker refuses to compile, by
construction.

The structural claim:

- *Declarations* tell you what the system can do.
- The *transitions* table tells you when.
- The *axioms* tell you what cannot be expressed.

There is no fourth thing.

<!-- speaker_note: Land the closing. The structural claim is the takeaway. Pause at the end. -->
