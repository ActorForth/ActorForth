"""
elevator_correct.py

The same two-elevator system as elevator_naive.py, with all flaws fixed.
Works correctly under concurrent load and emergency injection.
About 2.5x the lines. See elevator_usl_writeup.md for the comparison.
"""

import threading
import time
from enum import Enum, auto
from collections import deque
from dataclasses import dataclass, field
from typing import Optional, Callable


# ---------- Types ----------

class Direction(Enum):
    UP = 1
    DOWN = -1
    IDLE = 0


class CarState(Enum):
    """Explicit state machine for the whole car, not per-subsystem flags."""
    IDLE_AT_FLOOR = auto()
    DOOR_OPENING = auto()
    LOADING = auto()
    DOOR_CLOSING = auto()
    PREPARING_TO_MOVE = auto()
    MOVING = auto()
    ARRIVING = auto()
    EMERGENCY_STOPPED = auto()
    EMERGENCY_DOOR_OPEN = auto()


@dataclass(frozen=True)
class Request:
    """Frozen: requests are values, not references. No aliasing bugs."""
    floor: int
    direction: Direction
    timestamp: float = field(default_factory=time.time)


@dataclass
class Assignment:
    """Binding of a request to a car, owned by the dispatcher."""
    request: Request
    car_id: int
    assigned_at: float = field(default_factory=time.time)
    completed: bool = False


# ---------- State-transition policy ----------

# The permitted transitions for CarState. Expressed as data so we can
# validate transitions instead of scattering if/else checks through methods.
# This is what USL would give you for free from the control graph.
_PERMITTED = {
    CarState.IDLE_AT_FLOOR: {
        CarState.DOOR_OPENING,
        CarState.PREPARING_TO_MOVE,
        CarState.EMERGENCY_STOPPED,
    },
    CarState.DOOR_OPENING: {
        CarState.LOADING,
        CarState.EMERGENCY_DOOR_OPEN,
    },
    CarState.LOADING: {
        CarState.DOOR_CLOSING,
        CarState.EMERGENCY_DOOR_OPEN,
    },
    CarState.DOOR_CLOSING: {
        CarState.PREPARING_TO_MOVE,
        CarState.IDLE_AT_FLOOR,
        CarState.EMERGENCY_STOPPED,
    },
    CarState.PREPARING_TO_MOVE: {
        CarState.MOVING,
        CarState.EMERGENCY_STOPPED,
    },
    CarState.MOVING: {
        CarState.ARRIVING,
        CarState.EMERGENCY_STOPPED,
    },
    CarState.ARRIVING: {
        CarState.DOOR_OPENING,
        CarState.IDLE_AT_FLOOR,
        CarState.EMERGENCY_STOPPED,
    },
    CarState.EMERGENCY_STOPPED: {
        CarState.EMERGENCY_DOOR_OPEN,
        CarState.IDLE_AT_FLOOR,
    },
    CarState.EMERGENCY_DOOR_OPEN: {
        CarState.IDLE_AT_FLOOR,
    },
}


class InvalidTransition(Exception):
    pass


# ---------- Subsystems as private helpers of Car ----------

class _Door:
    """Only accessible through its owning Car. No public interface."""

    def __init__(self):
        self._is_open = False

    def _open(self):
        time.sleep(0.5)
        self._is_open = True

    def _close(self):
        time.sleep(0.5)
        self._is_open = False

    @property
    def is_open(self):
        return self._is_open


class _Motor:
    """Only accessible through its owning Car. No public interface."""

    def __init__(self):
        self._current_floor = 1
        self._moving = False

    def _move_to(self, floor: int):
        self._moving = True
        direction = 1 if floor > self._current_floor else -1
        while self._current_floor != floor:
            time.sleep(0.3)
            self._current_floor += direction
        self._moving = False

    @property
    def current_floor(self):
        return self._current_floor

    @property
    def is_moving(self):
        return self._moving


# ---------- Car: the single authority for its subsystems ----------

class Car:
    """
    A car owns its door and motor. Nothing outside the Car can touch them.
    All operations go through methods that enforce the state machine.
    """

    def __init__(self, car_id: int, on_state_change: Callable[[int, CarState], None]):
        self.car_id = car_id
        self._door = _Door()
        self._motor = _Motor()
        self._state = CarState.IDLE_AT_FLOOR
        self._state_cv = threading.Condition()
        self._on_state_change = on_state_change
        self._running = True
        self._assignment: Optional[Assignment] = None

    @property
    def current_floor(self):
        return self._motor.current_floor

    @property
    def state(self):
        with self._state_cv:
            return self._state

    def _transition(self, new_state: CarState):
        """Atomic state transition with validation. Called with _state_cv held."""
        if new_state not in _PERMITTED[self._state]:
            raise InvalidTransition(
                f"Car {self.car_id}: {self._state.name} -> {new_state.name}"
            )
        self._state = new_state
        self._state_cv.notify_all()
        self._on_state_change(self.car_id, new_state)

    def assign(self, assignment: Assignment) -> bool:
        """Called by dispatcher ONLY. Returns True if accepted."""
        with self._state_cv:
            if self._state != CarState.IDLE_AT_FLOOR:
                return False
            if self._assignment is not None:
                return False
            self._assignment = assignment
            self._state_cv.notify_all()
            return True

    def trigger_emergency(self):
        """Called by dispatcher ONLY, never by EmergencySystem directly."""
        with self._state_cv:
            if self._state in (CarState.EMERGENCY_STOPPED,
                                CarState.EMERGENCY_DOOR_OPEN):
                return
            self._state = CarState.EMERGENCY_STOPPED
            self._state_cv.notify_all()
            self._on_state_change(self.car_id, self._state)

    def clear_emergency(self):
        """Called by dispatcher ONLY."""
        with self._state_cv:
            if self._state == CarState.EMERGENCY_DOOR_OPEN:
                self._door._close()
            self._transition(CarState.IDLE_AT_FLOOR)

    def run(self):
        """One thread per car."""
        while self._running:
            with self._state_cv:
                while (self._running
                       and self._assignment is None
                       and self._state not in (CarState.EMERGENCY_STOPPED,)):
                    self._state_cv.wait(timeout=0.5)

                if not self._running:
                    return

                if self._state == CarState.EMERGENCY_STOPPED:
                    self._door._open()
                    self._transition(CarState.EMERGENCY_DOOR_OPEN)
                    while (self._running
                           and self._state == CarState.EMERGENCY_DOOR_OPEN):
                        self._state_cv.wait(timeout=0.5)
                    continue

                assignment = self._assignment

            self._execute_request(assignment)

    def _execute_request(self, assignment: Assignment):
        target = assignment.request.floor

        if self._motor.current_floor != target:
            with self._state_cv:
                if self._state != CarState.IDLE_AT_FLOOR:
                    return
                self._transition(CarState.PREPARING_TO_MOVE)

            with self._state_cv:
                if self._state != CarState.PREPARING_TO_MOVE:
                    return
                self._transition(CarState.MOVING)

            self._motor._move_to(target)

            with self._state_cv:
                if self._state != CarState.MOVING:
                    return
                self._transition(CarState.ARRIVING)

        with self._state_cv:
            if self._state not in (CarState.ARRIVING, CarState.IDLE_AT_FLOOR):
                return
            self._transition(CarState.DOOR_OPENING)

        self._door._open()

        with self._state_cv:
            if self._state != CarState.DOOR_OPENING:
                return
            self._transition(CarState.LOADING)

        time.sleep(1.0)

        with self._state_cv:
            if self._state != CarState.LOADING:
                return
            self._transition(CarState.DOOR_CLOSING)

        self._door._close()

        with self._state_cv:
            if self._state != CarState.DOOR_CLOSING:
                return
            self._transition(CarState.IDLE_AT_FLOOR)
            self._assignment = None
            assignment.completed = True

    def stop(self):
        with self._state_cv:
            self._running = False
            self._state_cv.notify_all()


# ---------- Dispatcher: the single parent of the cars ----------

class Dispatcher:
    """
    Parent of all cars. All external actions (requests, emergency)
    go through here. Maintains consistent model of car states.
    """

    def __init__(self, num_cars: int):
        self._pending: deque = deque()
        self._assignments: list = []
        self._car_states: dict = {}
        self._lock = threading.RLock()
        self._cv = threading.Condition(self._lock)
        self._running = True
        self._emergency_active = False

        self._cars = [
            Car(car_id=i, on_state_change=self._on_car_state_change)
            for i in range(num_cars)
        ]
        for car in self._cars:
            self._car_states[car.car_id] = CarState.IDLE_AT_FLOOR

    def _on_car_state_change(self, car_id: int, new_state: CarState):
        with self._lock:
            self._car_states[car_id] = new_state
            self._cv.notify_all()

    def request(self, floor: int, direction: Direction):
        with self._lock:
            if self._emergency_active:
                return
            self._pending.append(Request(floor, direction))
            self._cv.notify_all()

    def trigger_emergency(self):
        with self._lock:
            if self._emergency_active:
                return
            self._emergency_active = True
            cars_snapshot = list(self._cars)
        for car in cars_snapshot:
            car.trigger_emergency()

    def clear_emergency(self):
        with self._lock:
            if not self._emergency_active:
                return
            self._emergency_active = False
            cars_snapshot = list(self._cars)
        for car in cars_snapshot:
            car.clear_emergency()

    def get_cars_for_test(self):
        """Test-only accessor, for injecting FAT scenarios."""
        return list(self._cars)

    def run(self):
        while self._running:
            with self._lock:
                while (self._running
                       and (not self._pending or self._emergency_active)):
                    self._cv.wait(timeout=0.2)

                if not self._running:
                    return

                request = self._pending.popleft()

                idle_cars = [
                    c for c in self._cars
                    if self._car_states[c.car_id] == CarState.IDLE_AT_FLOOR
                ]
                if not idle_cars:
                    self._pending.appendleft(request)
                    self._cv.wait(timeout=0.5)
                    continue

                best_car = min(
                    idle_cars,
                    key=lambda c: abs(c.current_floor - request.floor)
                )
                assignment = Assignment(request=request, car_id=best_car.car_id)

            accepted = best_car.assign(assignment)
            if not accepted:
                with self._lock:
                    self._pending.appendleft(request)
                    self._cv.notify_all()
            else:
                with self._lock:
                    self._assignments.append(assignment)

    def stop(self):
        with self._lock:
            self._running = False
            self._cv.notify_all()
        for car in self._cars:
            car.stop()


# ---------- EmergencySystem: no direct reference to cars ----------

class EmergencySystem:
    """
    Only holds a reference to the Dispatcher. Physically cannot
    communicate with cars directly. This is what enforces the
    single-parent axiom in Python: by not giving out the reference.
    """

    def __init__(self, dispatcher: Dispatcher):
        self._dispatcher = dispatcher

    def trigger(self):
        self._dispatcher.trigger_emergency()

    def clear(self):
        self._dispatcher.clear_emergency()


def build_system():
    dispatcher = Dispatcher(num_cars=2)
    emergency = EmergencySystem(dispatcher)

    threads = []
    for car in dispatcher.get_cars_for_test():
        t = threading.Thread(target=car.run, daemon=True)
        t.start()
        threads.append(t)
    dt = threading.Thread(target=dispatcher.run, daemon=True)
    dt.start()
    threads.append(dt)

    return dispatcher, emergency, threads


if __name__ == "__main__":
    dispatcher, emergency, threads = build_system()
    print("Corrected elevator system running. Press Ctrl+C to stop.")
    try:
        dispatcher.request(5, Direction.UP)
        time.sleep(0.1)
        dispatcher.request(3, Direction.DOWN)
        time.sleep(10)
        cars = dispatcher.get_cars_for_test()
        for c in cars:
            print(f"Car {c.car_id} floor: {c.current_floor}, state: {c.state.name}")
    except KeyboardInterrupt:
        dispatcher.stop()
