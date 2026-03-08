"""
debounce_python_equivalent.py — Python equivalent of debounce_demo.a4

For side-by-side comparison with the ActorForth demo. Implements the same
feature set: product types, EventStore actor, Logger actor, and tests.

APPROACH
========
Python has no built-in actor model. The standard library offers threading
and queue primitives, so we build actors from scratch using Thread + Queue.
This is the idiomatic Python approach (asyncio would be similar in structure
but with different syntax).

WHAT THIS FILE DEMONSTRATES
============================
  debounce_demo.a4:                  ~80 lines of definitions + tests
  debounce_python_equivalent.py:     ~220 lines for the same functionality

The difference comes from:
  1. No native actor primitive — must build actor base class with
     message loop, dispatch, start/stop lifecycle, and reply mechanism.
  2. No type enforcement at message boundaries — any dict/object can be
     sent to any actor. A typo in a message field is a runtime KeyError
     deep in the handler, not a dispatch-time rejection.
  3. Explicit threading boilerplate — Thread, Queue, Event, daemon flags.
  4. Reply mechanism must be hand-built with response queues or Events.
  5. Dataclasses provide named fields but no stack-based composition —
     each field must be named at construction site.

NOTE: Python's GIL means these threads don't truly run in parallel.
The actor model here provides isolation and message-passing semantics
but not parallelism — unlike ActorForth on the BEAM, where each actor
is a lightweight process with preemptive scheduling.
"""

from dataclasses import dataclass
from threading import Thread, Event
from queue import Queue, Empty
from typing import Any
import time


# ==============================================================
# Product types — equivalent to ActorForth type declarations
# ==============================================================
# ActorForth:  type Entry path String hash String size Int kind String .
# Python: 5 lines per dataclass (decorator + class + fields)

@dataclass
class Entry:
    path: str
    hash: str
    size: int
    kind: str

@dataclass
class Stats:
    size_diff: int
    count_diff: int

@dataclass
class Batch:
    added: int
    deleted: int
    size_delta: int
    count_delta: int

@dataclass
class Tally:
    batches: int = 0

@dataclass
class Store:
    events: int = 0


# ==============================================================
# Actor base class — no equivalent needed in ActorForth
# ==============================================================
# ActorForth: "0 store server" (3 words, zero infrastructure)
# Python: 45 lines of threading/queue plumbing that every actor needs.

class Actor:
    """Base actor class providing message loop, cast/call, and lifecycle."""

    def __init__(self, state):
        self._state = state
        self._mailbox = Queue()
        self._running = Event()
        self._thread = Thread(target=self._loop, daemon=True)

    def start(self):
        self._running.set()
        self._thread.start()
        return self

    def stop(self):
        self._mailbox.put(("__stop__", None, None))
        self._thread.join(timeout=2.0)

    def cast(self, method: str, *args):
        """Async send — no reply. No type checking on method or args."""
        self._mailbox.put((method, args, None))

    def call(self, method: str, *args) -> Any:
        """Sync send — blocks for reply. No type checking."""
        reply_queue = Queue()
        self._mailbox.put((method, args, reply_queue))
        return reply_queue.get(timeout=5.0)

    def _loop(self):
        while self._running.is_set():
            try:
                method, args, reply_queue = self._mailbox.get(timeout=0.1)
            except Empty:
                continue

            if method == "__stop__":
                self._running.clear()
                break

            # Dispatch by method name string — no type safety here.
            # A typo like "reocrd" instead of "record" silently does nothing
            # (caught by the catch-all hasattr check, or raises AttributeError
            # deep in the handler). In ActorForth, the typed vocab rejects
            # unknown words before the message is sent.
            handler = getattr(self, f"handle_{method}", None)
            if handler is None:
                # Silent drop — equivalent to Erlang's catch-all clause.
                if reply_queue:
                    reply_queue.put(None)
                continue

            result = handler(*args)
            if reply_queue:
                reply_queue.put(result)


# ==============================================================
# EventStore actor
# ==============================================================
# ActorForth equivalent:
#   : record Store Int -> Store ;
#       swap events rot + events! .
#   : total Store -> Store Int ;
#       dup events .
#   0 store server

class StoreActor(Actor):
    def __init__(self):
        super().__init__(Store(events=0))

    def handle_record(self, n: int):
        self._state.events += n

    def handle_total(self) -> int:
        return self._state.events


# ==============================================================
# Logger actor
# ==============================================================
# ActorForth equivalent:
#   : log_batch Tally Batch -> Tally ;
#       drop batches 1 + batches! .
#   : logged Tally -> Tally Int ;
#       dup batches .
#   0 tally server

class TallyActor(Actor):
    def __init__(self):
        super().__init__(Tally(batches=0))

    def handle_log_batch(self, batch: Batch):
        # The type hint says Batch, but Python won't enforce it.
        # You can pass a string, an int, or None — no error until
        # (maybe) something downstream blows up. In ActorForth,
        # the actor vocab checks the type before dispatch.
        self._state.batches += 1

    def handle_logged(self) -> int:
        return self._state.batches


# ==============================================================
# Tests
# ==============================================================

def test_product_types():
    """Equivalent to the product type test section of debounce_demo.a4."""

    # --- Batch ---
    # ActorForth: 5 3 200 2 batch
    #             added 5 assert-eq
    b = Batch(added=5, deleted=3, size_delta=200, count_delta=2)
    assert b.added == 5
    assert b.deleted == 3
    assert b.size_delta == 200
    assert b.count_delta == 2

    # --- Stats ---
    s = Stats(size_diff=1024, count_diff=1)
    assert s.size_diff == 1024
    assert s.count_diff == 1

    # --- Entry ---
    e = Entry(path="/docs/report.pdf", hash="abc123",
              size=4096, kind="application/pdf")
    assert e.path == "/docs/report.pdf"
    assert e.hash == "abc123"
    assert e.size == 4096
    assert e.kind == "application/pdf"


def test_actors():
    """Equivalent to the actor test section of debounce_demo.a4."""

    store = StoreActor().start()
    tally = TallyActor().start()

    # --- Logger tests ---
    # ActorForth: << 5 3 200 2 batch log_batch >>
    tally.cast("log_batch", Batch(added=5, deleted=3, size_delta=200, count_delta=2))

    # ActorForth: << 10 1 500 9 batch log_batch >>
    tally.cast("log_batch", Batch(added=10, deleted=1, size_delta=500, count_delta=9))

    # Must sleep because cast is async and Python has no ordering guarantee
    # between cast and subsequent call without explicit synchronization.
    # ActorForth's server protocol handles this transparently.
    time.sleep(0.05)

    # ActorForth: << logged >> 2 assert-eq
    assert tally.call("logged") == 2

    # --- EventStore tests ---
    # ActorForth: << 5 record >> << 3 record >>
    store.cast("record", 5)
    store.cast("record", 3)

    time.sleep(0.05)

    # ActorForth: << total >> 8 assert-eq
    assert store.call("total") == 8

    # Cleanup — ActorForth: << stop >> drop
    store.stop()
    tally.stop()


def test_no_type_safety():
    """
    Demonstrate what ActorForth prevents but Python allows.
    These calls all succeed silently — no error, no warning.
    """
    tally = TallyActor().start()

    # Send completely wrong type — Python doesn't care
    tally.cast("log_batch", "not a batch at all")
    time.sleep(0.02)
    # It incremented anyway! The handler just does += 1, never checks the arg.
    assert tally.call("logged") == 1

    # Send to nonexistent method — silently dropped
    tally.cast("nonexistent_method", 42)
    time.sleep(0.02)
    # No error. No indication anything went wrong.

    tally.stop()


def run_once():
    test_product_types()
    test_actors()


def bench(n: int):
    """Run tests n times, report min/avg/max in microseconds."""
    # Warm-up
    run_once()

    times = []
    for _ in range(n):
        start = time.perf_counter()
        run_once()
        elapsed_us = int((time.perf_counter() - start) * 1_000_000)
        times.append(elapsed_us)

    print(f"BENCH_RESULT: min={min(times)} avg={sum(times)//n} "
          f"max={max(times)} iters={n}")


if __name__ == "__main__":
    import sys
    if len(sys.argv) >= 3 and sys.argv[1] == "--bench":
        run_once()  # correctness check
        test_no_type_safety()
        print("All Python equivalent tests passed.")
        bench(int(sys.argv[2]))
    else:
        test_product_types()
        test_actors()
        test_no_type_safety()
        print("All Python equivalent tests passed.")
