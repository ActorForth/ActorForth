# Escrow demo

A three-party escrow contract as an ActorForth actor, alongside a
reference implementation in Solidity. Same behaviour, same state
machine, written in each language's idiom.

## The contract

Three participants — **buyer**, **seller**, **verifier** (oracle) — and an
escrowed amount. The state machine:

```
  Pending  --deposit(amount, buyer)-->  Funded
  Funded   --verify(verifier)-->        Verified
  Verified --release()-->               Released
  Funded   --dispute(party)-->          Refunded
```

Invalid transitions are no-ops. `release` before `verify` changes
nothing. `deposit` after `verify` changes nothing. `dispute` after
release changes nothing.

## Run the test

```
rebar3 shell
1> af_repl:run_file("samples/escrow/escrow_test.a4").
```

Expected output:

```
escrow happy path: passed
escrow state guards: passed
escrow dispute path: passed
```

The test exercises: happy path (deposit → verify → release), invalid
transitions at each state, and the dispute path (deposit → dispute →
refund). Every invalid call is verified to leave state unchanged.

## Comparison with Solidity

Line counts (non-comment, non-blank):

| | Lines |
|---|---|
| `escrow.a4` | 54 |
| `Escrow.sol` | 57 |

The line count isn't the story. The story is where validation lives.

### Solidity

```solidity
modifier inStatus(Status s) {
    if (status != s) revert InvalidState(status, s);
    _;
}

function release() external inStatus(Status.Verified) {
    status = Status.Released;
    ...
}
```

Validation is a **runtime** `revert`. If you forget the `inStatus` modifier,
the compiler won't catch it — the function will mutate state from any
status. A forgotten modifier is how [at least one](https://github.com/sushiswap/sushiswap/issues/230)
real-world exploit has worked.

### ActorForth

```
: release Escrow -> Escrow ;
    : Escrow where status VERIFIED == -> Escrow ;
        set-released
    : Escrow -> Escrow ;
        noop .
```

Validation is a **dispatch-time** guard on a sub-clause. There is no
"forgot the modifier" failure mode: the guard is part of the word's
definition, and dispatch picks the right clause or the no-op fallthrough.
Swapping the guard off isn't a code-review catch — there's nothing to
swap off.

## What a4 buys beyond line count

1. **Actors out of the box.** `"alice" "bob" "oracle" 1000 PENDING escrow server`
   turns the contract into a live actor. Multiple concurrent escrows
   between different parties are trivially isolated — the BEAM mailbox
   serialises messages per-actor. Solidity contracts are already
   per-address-isolated, but Solidity has no other concurrency model;
   if you want to orchestrate multiple contracts interacting, you're in
   Solidity's transaction-reentrancy minefield. a4 actors just send
   messages to each other.

2. **Typed messages.** `<< 1000 "alice" deposit >>` can only be
   constructed with an Int then a String then the word name. The typed
   stack catches misordered arguments at dispatch, not after state has
   changed. Solidity ABI decoding does not protect against the
   programmer-error "I passed the wrong argument" — the decoded value
   is untyped bytes until modifiers check.

3. **The contract reads like a specification.** `: deposit Escrow Int
   String -> Escrow ;` followed by the guarded sub-clauses reads
   top-to-bottom: "a deposit on an Escrow with an Int and a String
   returns an Escrow; if status is Pending, apply the deposit;
   otherwise drop the args." No `external`/`internal`/`view`/`payable`
   decorators, no `msg.sender`, no modifier-before-definition
   indirection. The declaration IS the spec.

## Files in this directory

- `escrow.a4` — ActorForth implementation (~54 non-comment lines)
- `escrow_test.a4` — end-to-end test through happy/invalid/dispute paths
- `Escrow.sol` — Solidity reference implementation (~57 non-comment lines)
- (planned) `escrow.py` / `escrow.erl` / `escrow.ts` — more references

## Known limitations (intentional for the demo scope)

- a4 `escrow.a4` does not implement actual fund movement — just the state
  machine. Real fund custody would need an Account/Wallet actor that
  this Escrow actor calls via `<< >>`. Adding that is ~15 more lines;
  it's omitted so the comparison stays apples-to-apples with the
  commented-out parts of the Solidity file.
- Caller-identity checks (buyer vs seller vs verifier) live in the
  guards conceptually but are coded as "drop the caller string after
  checking status." Strengthening these to full identity checks is a
  few more lines per guard and doesn't change the comparison.
