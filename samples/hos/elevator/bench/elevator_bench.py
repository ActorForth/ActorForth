"""
elevator_bench.py

Throughput benchmark for the Python "correct" elevator reference
(samples/hos/elevator/comparison/elevator_correct.py).

Strategy: monkeypatch `time.sleep` to a no-op BEFORE importing the
reference module. Every `time.sleep(...)` call inside the reference
(door open/close, motor per-floor travel, loading dwell) becomes a
pass-through. The state machine, the validated transitions, the
condition-variable coordination, the lock acquisitions, and the
threads ALL stay intact. The only thing stripped is wall-clock
waiting for mechanical events that do not apply in a pure
throughput measurement.

Counting convention: one full cycle of the elevator state machine
(request -> preparing -> moving -> arriving -> door opening -> loading
-> door closing -> idle) is counted as 16 "events", matching the
Erlang HOS bench's total message count across all subsystems for one
floor-button-press cycle. This gives an apples-to-apples comparison
across the three language implementations.

Output format:
    [BENCH] Python correct events/sec=YYY cycles=N wall=XXX.XXXs

The orchestrator script (run_bench.sh) greps for [BENCH] lines.
"""

import argparse
import os
import sys
import time as _time

# Patch sleep BEFORE importing the reference module. The reference
# module holds a reference to `time.sleep` through the `time` module,
# so replacing the attribute on `time` is picked up by every call
# inside the module.
_original_sleep = _time.sleep
_time.sleep = lambda *_args, **_kwargs: None

# Path mangling so this script can be run from any directory.
_HERE = os.path.dirname(os.path.abspath(__file__))
_COMPARISON = os.path.normpath(os.path.join(_HERE, "..", "comparison"))
if _COMPARISON not in sys.path:
    sys.path.insert(0, _COMPARISON)

# Import the reference. Because time.sleep is already patched, every
# sleep inside the module's classes picks up the no-op version.
import elevator_correct  # noqa: E402
from elevator_correct import Dispatcher, Direction, CarState  # noqa: E402

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------

EVENTS_PER_CYCLE = 16  # Matches the Erlang HOS bench's counting.


def wait_all_idle(dispatcher, deadline_s=30.0):
    """Wait until every car is IdleAtFloor AND the pending queue is
    empty. Uses unpatched sleep for the polling loop itself so we do
    not busy-wait forever."""
    deadline = _time.monotonic() + deadline_s
    while _time.monotonic() < deadline:
        cars = dispatcher.get_cars_for_test()
        # Access internal pending via lock-free check; dispatcher is
        # the single writer during this poll, but we snapshot carefully.
        with dispatcher._lock:  # noqa: SLF001
            pending_empty = len(dispatcher._pending) == 0  # noqa: SLF001
        all_idle = all(
            c.state == CarState.IDLE_AT_FLOOR for c in cars
        )
        if pending_empty and all_idle:
            return True
        # Use the original (unpatched) sleep so this polling loop
        # does not spin.
        _original_sleep(0.001)
    return False


def run_bench(num_requests, num_cars=2, warmup=10):
    dispatcher = Dispatcher(num_cars=num_cars)

    # Start threads manually (mirrors build_system() but lets us
    # control shutdown and ownership from the bench).
    import threading
    car_threads = []
    for car in dispatcher.get_cars_for_test():
        t = threading.Thread(target=car.run, daemon=True)
        t.start()
        car_threads.append(t)
    dispatcher_thread = threading.Thread(
        target=dispatcher.run, daemon=True)
    dispatcher_thread.start()

    try:
        # Warm-up: send `warmup` requests, let the system drain.
        for i in range(warmup):
            dispatcher.request(1 + (i % 5), Direction.UP)
        if not wait_all_idle(dispatcher):
            raise RuntimeError(
                "warm-up: system did not return to idle within 30s")

        # Timed run.
        start = _time.monotonic()
        for i in range(num_requests):
            floor = 1 + (i % 5)
            direction = Direction.UP if (i % 2 == 0) else Direction.DOWN
            dispatcher.request(floor, direction)
        # Wait for the dispatcher queue to drain AND all cars to be
        # back at IDLE_AT_FLOOR. This is the clean "all done" signal.
        if not wait_all_idle(dispatcher, deadline_s=120.0):
            raise RuntimeError(
                "timed run: system did not drain within 120s; "
                "something is wrong")
        end = _time.monotonic()
    finally:
        dispatcher.stop()
        # Unpatched sleep for the shutdown settling.
        _original_sleep(0.05)

    return end - start


def main():
    ap = argparse.ArgumentParser(
        description="Benchmark the Python correct elevator.")
    ap.add_argument("n", type=int, nargs="?", default=1000,
                    help="number of requests in the timed run (default 1000)")
    ap.add_argument("--cars", type=int, default=2,
                    help="number of cars (default 2)")
    ap.add_argument("--warmup", type=int, default=10,
                    help="warm-up requests before timing (default 10)")
    args = ap.parse_args()

    wall = run_bench(
        num_requests=args.n,
        num_cars=args.cars,
        warmup=args.warmup,
    )
    total_events = args.n * EVENTS_PER_CYCLE
    rate = total_events / wall if wall > 0 else 0.0
    print(f"Python correct: cars={args.cars} N={args.n} "
          f"wall={wall:.3f}s events/sec={rate:.2f}")
    # Grep target for the orchestrator.
    print(f"[BENCH] Python correct events/sec={rate:.2f} "
          f"cycles={args.n} wall={wall:.3f}s")


if __name__ == "__main__":
    main()
