# Auction demo

A live auction actor. Each auction is its own actor; bids are validated
inside the actor's dispatch handler, so concurrent bids from different
callers are naturally serialised by the BEAM mailbox.

Validation lives in guard expressions — an invalid bid never touches
state:

- auction closed → the bid is dropped
- bid not strictly greater than the current bid → dropped
- otherwise → state updated, new current_bid and highest_bidder

## Run the tests across all languages

```
./run_all.sh
```

This builds and runs the reference implementation in every language and
confirms each one's pass message. Expected ActorForth output:

```
auction test: direct word calls passed
auction test: actor roundtrip passed
```

The test exercises both direct word calls (no actor) and the full
`<< ... >>` actor-message protocol, covering acceptance, low-bid
rejection, and post-close rejection.

## Code-size comparison (same functionality)

Non-blank, non-comment lines of the implementation file:

| Language     | Impl lines | Notes |
|--------------|-----------:|-------|
| **a4**       | **37**     | `type Auction` with `server` — actor is a typed instance; guards dispatch bid outcome |
| Elixir       | 47         | GenServer with struct + pattern-matched `handle_call` clauses |
| TypeScript   | 56         | Class with validated `bid` method |
| Python       | 61         | Class with `asyncio.Lock` per auction |
| C++20        | 61         | Class with `std::mutex` guarding state |
| Erlang       | 62         | gen_server with record + case-based validation |

(ActorForth test harness is an additional 30 lines in `auction_test.a4`.)

## Why a4 is short here

Three things compound to make the a4 version the smallest:

1. **Actor-as-type.** `server` turns any typed instance into an actor.
   No `init/1`, `handle_call/3`, `handle_cast/2`, `terminate/2` callbacks.
   Erlang and Elixir lose 10–20 lines to gen_server shape alone.

2. **Guards express preconditions.** The three validation cases (closed,
   too-low, accept) are three sub-clauses with `where` guards. In every
   other language they become `if/else` chains or match expressions that
   name the state explicitly. In a4 they dispatch on the guard result.

3. **Typed messages make the interface tight.** `<< 150 "alice" bid >>`
   can only be built with an Int and a String in that order; the type
   system rejects `<< "alice" 150 bid >>` at dispatch, before state is
   touched.

Elixir is the closest competitor — its struct + pattern-matched `handle_call`
clauses are compact — but it still carries explicit GenServer boilerplate
(`use GenServer`, `init/1`, `start_link`, `handle_call` heads with `_from`,
state-tuple returns) that a4 elides entirely.
