"""
throughput_python.py — Actor message dispatch throughput benchmark

Python equivalent of throughput_demo.a4. A Counter actor receives N bump
casts, then a sync get_count call verifies all arrived. No sleep delays —
the sync call guarantees FIFO ordering via Queue.

NOTE: Python's GIL means the actor thread and main thread don't truly run
in parallel, but the message dispatch overhead is real.
"""

from threading import Thread, Event
from queue import Queue, Empty
from typing import Any
import time
import sys


class Actor:
    """Minimal actor: Thread + Queue + dispatch."""

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
        self._thread.join(timeout=5.0)

    def cast(self, method: str, *args):
        self._mailbox.put((method, args, None))

    def call(self, method: str, *args) -> Any:
        reply_queue = Queue()
        self._mailbox.put((method, args, reply_queue))
        return reply_queue.get(timeout=10.0)

    def _loop(self):
        while self._running.is_set():
            try:
                method, args, reply_queue = self._mailbox.get(timeout=0.5)
            except Empty:
                continue
            if method == "__stop__":
                self._running.clear()
                break
            handler = getattr(self, f"handle_{method}", None)
            if handler is None:
                if reply_queue:
                    reply_queue.put(None)
                continue
            result = handler(*args)
            if reply_queue:
                reply_queue.put(result)


class CounterActor(Actor):
    def __init__(self):
        super().__init__(0)

    def handle_bump(self):
        self._state += 1

    def handle_get_count(self) -> int:
        return self._state


def run_once(n=100000):
    actor = CounterActor().start()
    for _ in range(n):
        actor.cast("bump")
    result = actor.call("get_count")
    assert result == n, f"Expected {n}, got {result}"
    actor.stop()


def bench(iters: int, n: int = 100000):
    # Warm-up
    run_once(n)

    times = []
    for _ in range(iters):
        start = time.perf_counter()
        run_once(n)
        elapsed_us = int((time.perf_counter() - start) * 1_000_000)
        times.append(elapsed_us)

    print(f"BENCH_RESULT: min={min(times)} avg={sum(times)//iters} "
          f"max={max(times)} iters={iters} msgs={n}")


if __name__ == "__main__":
    if len(sys.argv) >= 3 and sys.argv[1] == "--bench":
        run_once()
        print("Python throughput test passed (100000 messages).")
        bench(int(sys.argv[2]))
    else:
        run_once()
        print("Python throughput test passed (100000 messages).")
