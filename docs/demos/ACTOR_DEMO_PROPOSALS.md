# Actor concurrency demo — design options

Goal: a single small program that flatters ActorForth's actor + typed-message
+ product-type model, implemented identically in a4, Python, C++, TypeScript,
Erlang, Elixir for the talk's cross-language comparison. "Small" = ~100 lines
of a4, readable live in a 10-minute demo slot.

## Evaluation criteria

1. **Real-world**, not contrived. Something the audience has written before.
2. **Shows what a4 is good at**: typed commands that can't be malformed, actor
   state isolation without locks, compact state-machine expression.
3. **Honest across languages**: the comparisons should be what an idiomatic
   author in each language would produce, not strawmen.
4. **Compelling read**: someone skimming the a4 code should see why it's shorter.

## Proposal A (recommended): per-API-key rate limiter

**Problem**: an API gateway that rate-limits by client API key using a token
bucket. Each key has its own bucket state that refills at a fixed rate.

**Why a4 wins on this**:
- **Actor per key** is the natural model. Each bucket actor holds its own
  state; no shared mutex. In Python/C++ you need a concurrent dictionary +
  fine-grained locking or a single lock that serialises everything.
- **Typed commands** (`check` vs `configure`) with value constraints. The
  actor's dispatcher can't receive a malformed command because the type
  system doesn't let one be constructed.
- **State transitions** (bucket refill logic) are short a4 words with
  pattern-matched clauses on value constraints. In other languages they
  become `if`/`else` chains with magic numbers.

**Rough shape in a4** (~80 lines):

```
type Bucket
    tokens    Int
    capacity  Int
    last_ms   Int
.

: now -> Int ; 0 erlang-apply0 erlang-system-time os ... .   # millisecond clock

# Refill using elapsed time since last touch
: refill Bucket Int -> Bucket ;
    over last_ms - ... tokens + capacity min tokens! ... .

: allow? Bucket -> Bucket Bool ;
    now refill
    dup tokens 0 > dup
        : Bool where dup -> Bucket Bool ; tokens 1 - tokens! true ;
        : Bool        -> Bucket Bool ; false ;
    .

: configure Bucket Int Int -> Bucket ; capacity! last_ms! .

# Gateway: one actor per API key, kept in a registry
: gate String -> Bool ; ... registry lookup, dispatch allow? ... .
```

**Audience response expected**: "that's it?" The Python version is ~150
lines with `asyncio.Lock`; the C++ version is ~300 lines with
`std::mutex` per bucket and a concurrent map. Erlang is close (~120)
because it's the natural model there — but it's still record + gen_server
boilerplate, and a4 is shorter and stronger-typed.

**Complexity for us to build**: moderate. ~2 days including all five
reference implementations and an integrated test harness that hits each
with the same synthetic request stream and compares throughput +
correctness.

## Proposal B: live auction with typed events

**Problem**: an auction engine. Each auction is an actor holding state
`{current_bid, highest_bidder, end_time, status}`. Commands: `bid`,
`query`, `close`. Events emitted: `bid_accepted`, `bid_rejected`,
`auction_closed`.

**Why a4 wins**:
- **Guard expressions** shine: `bid` is valid iff (amount > current_bid
  AND status == Open). Single a4 word with a `where` guard.
- **Typed events** make the log naturally queryable.
- **Actor-per-auction** gives concurrent auctions trivially.

**Bigger than Proposal A**: ~150 lines of a4. Bigger advantage over other
languages but also more to show on screen.

**Complexity**: moderate-high. ~3 days.

## Proposal C: distributed two-phase commit coordinator

**Problem**: coordinator actor + N participant actors. Coordinator sends
`prepare`, collects replies, then sends `commit` or `abort`.

**Why a4 wins**:
- State machine in the coordinator is a perfect match for multi-clause
  dispatch on state tag.
- Typed protocol messages prevent malformed traffic.

**Caveat**: classic distributed systems academic problem. Audience may
view it as too textbook. Less "I've built this at work" feeling than A.

**Complexity**: medium. ~2 days.

---

# Recommendation

**Start with Proposal A (rate limiter)**. It's:
- Universally recognised — every backend engineer has built one or fought one.
- Small enough to fit on screen.
- Shows off exactly the right things (per-entity actor, typed commands,
  guard validation).
- Honest across languages (Python asyncio.Lock, C++ std::mutex+concurrent map,
  Erlang gen_server, Elixir GenServer) — all idiomatic, none strawmen.

Build in this order:
1. **a4 reference implementation** under `test/demos/ratelimiter/ratelimiter.a4`
2. **Cross-language equivalents** in Python / C++ / TypeScript / Erlang / Elixir
3. **Unified test harness** (`run_all.sh`) that runs the same 10k-request
   synthetic workload against each, reports throughput and peak memory.
4. Short README that walks the reader through the a4 version.

I can start implementation when you green-light the proposal. Expected: a4
version + README in one sitting (~2–3 hours), then the cross-language
implementations one per sitting after that.
