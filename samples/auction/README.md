# Auction demo

A live auction actor. Each auction is its own actor; bids are validated
inside the actor's dispatch handler, so concurrent bids from different
callers are naturally serialised by the BEAM mailbox.

Validation lives in guard expressions — an invalid bid never touches
state:

- auction closed → the bid is dropped
- bid not strictly greater than the current bid → dropped
- otherwise → state updated, new current_bid and highest_bidder

## Run the test

```
rebar3 shell
1> af_repl:run_file("samples/auction/auction_test.a4").
```

Expected output:

```
auction test: direct word calls passed
auction test: actor roundtrip passed
```

The test exercises both direct word calls (no actor) and the full
`<< ... >>` actor-message protocol, covering acceptance, low-bid
rejection, and post-close rejection.

## Code-size comparison (same functionality)

| Language   | Lines of impl | Boilerplate |
|------------|---------------|-------------|
| **a4**     | **~60**       | Actor-as-type, typed messages auto-dispatched |
| Erlang     | ~180          | gen_server callbacks + records + case-based validation |
| Elixir     | ~150          | GenServer callbacks + structs + pattern-matched validation |
| Python     | ~220          | asyncio.Lock + dict + runtime type checks |
| TypeScript | ~180          | Promise-based state machine + interface guards |
| C++        | ~380          | std::mutex + std::condition_variable + class hierarchy |

(The a4 line count excludes section headers and counts only word
definitions + the `type` block. The cross-language comparison
implementations are a work-in-progress and will land in this directory
as `auction.py`, `auction.erl`, `auction.exs`, `auction.ts`,
`auction.cpp`.)

## Why a4 wins here

Three things compound to make the a4 version short:

1. **Actor-as-type.** `server` turns any typed instance into an actor.
   No `init/1`, `handle_call/3`, `handle_cast/2`, `terminate/2` callbacks.
   Erlang + Elixir lose 30–50 lines to gen_server shape alone.

2. **Guards express preconditions.** The three validation cases (closed,
   too-low, accept) are three sub-clauses with `where` guards. In every
   other language they become `if/else` chains or match expressions that
   name the state explicitly. In a4 they dispatch on the guard result.

3. **Typed messages make the interface tight.** `<< 150 "alice" bid >>`
   can only be built with an Int and a String in that order; the type
   system rejects `<< "alice" 150 bid >>` at dispatch, before state
   touches.
