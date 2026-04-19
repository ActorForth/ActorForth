"""
elevator_naive.py

A two-elevator controller with deliberate interface errors.
Do not use this for anything real. See elevator_usl_writeup.md for the
bug catalog and FAT test scenarios this implementation fails.
"""

import threading
import time
from enum import Enum
from collections import deque
from dataclasses import dataclass, field
from typing import Optional


class Direction(Enum):
    UP = 1
    DOWN = -1
    IDLE = 0


class DoorState(Enum):
    CLOSED = "closed"
    OPENING = "opening"
    OPEN = "open"
    CLOSING = "closing"


class MotorState(Enum):
    STOPPED = "stopped"
    MOVING = "moving"


@dataclass
class Request:
    floor: int
    direction: Direction
    timestamp: float = field(default_factory=time.time)


class Door:
    """Controls a single elevator's door."""

    def __init__(self, car_id: int):
        self.car_id = car_id
        self.state = DoorState.CLOSED
        self._lock = threading.Lock()

    def open(self):
        with self._lock:
            if self.state == DoorState.CLOSED:
                self.state = DoorState.OPENING
                # FLAW 1: No check that the motor is actually stopped.
                # Door trusts whoever called open() to have verified this.
                time.sleep(0.5)  # Physical door-open time
                self.state = DoorState.OPEN

    def close(self):
        with self._lock:
            if self.state == DoorState.OPEN:
                self.state = DoorState.CLOSING
                time.sleep(0.5)
                self.state = DoorState.CLOSED


class Motor:
    """Controls a single elevator's motor."""

    def __init__(self, car_id: int):
        self.car_id = car_id
        self.state = MotorState.STOPPED
        self.current_floor = 1
        self._lock = threading.Lock()

    def move_to(self, floor: int):
        with self._lock:
            # FLAW 2: No check that the door is actually closed.
            # Motor trusts its caller. Same sibling-trust problem as Door.
            self.state = MotorState.MOVING
            direction = 1 if floor > self.current_floor else -1
            while self.current_floor != floor:
                time.sleep(0.3)  # Physical travel time per floor
                self.current_floor += direction
            self.state = MotorState.STOPPED


class ElevatorCar:
    """A single elevator car. Owns its door, motor, and request queue."""

    def __init__(self, car_id: int):
        self.car_id = car_id
        self.door = Door(car_id)
        self.motor = Motor(car_id)
        self.queue: deque = deque()
        self._lock = threading.Lock()
        self._running = True
        self._emergency = False

    def add_request(self, request: Request):
        with self._lock:
            self.queue.append(request)

    def run(self):
        """Main loop -- one thread per car."""
        while self._running:
            # FLAW 3: Emergency check is a flag read outside the lock.
            # TOCTOU: dispatcher can set _emergency between this check
            # and the motor.move_to() call below.
            if self._emergency:
                self.door.open()
                time.sleep(0.1)
                continue

            request = None
            with self._lock:
                if self.queue:
                    request = self.queue.popleft()

            if request is None:
                time.sleep(0.1)
                continue

            # FLAW 4: The order of operations is enforced by convention,
            # not by structure. Door-close, motor-move, door-open is the
            # right sequence, but nothing prevents a future maintainer from
            # reordering these, and nothing prevents two threads from
            # interleaving them for two different requests.
            self.door.close()
            self.motor.move_to(request.floor)
            self.door.open()

    def emergency_stop(self):
        # FLAW 5: Sibling-to-sibling communication. Called by
        # EmergencySystem directly on the car, bypassing the Dispatcher.
        self._emergency = True

    def stop(self):
        self._running = False


class Dispatcher:
    """Assigns requests to cars. The 'parent' of the two cars."""

    def __init__(self, cars):
        self.cars = cars
        self.pending: deque = deque()
        self._lock = threading.Lock()
        self._running = True

    def request(self, floor: int, direction: Direction):
        with self._lock:
            self.pending.append(Request(floor, direction))

    def run(self):
        while self._running:
            request = None
            with self._lock:
                if self.pending:
                    request = self.pending.popleft()

            if request is None:
                time.sleep(0.1)
                continue

            # FLAW 6 (compounds 4): Dispatcher picks the "nearest" car using
            # a snapshot of each car's current_floor, but there's no
            # coordination with the car's actual commitment to that position.
            best_car = min(
                self.cars,
                key=lambda c: abs(c.motor.current_floor - request.floor)
            )
            best_car.add_request(request)


class EmergencySystem:
    """Fire alarm, emergency stop button, etc."""

    def __init__(self, cars, dispatcher: Dispatcher):
        self.cars = cars
        self.dispatcher = dispatcher  # Held but not used -- see below

    def trigger(self):
        # FLAW 5 again, more starkly: EmergencySystem has a reference to
        # the Dispatcher but doesn't use it. It reaches directly into cars.
        for car in self.cars:
            car.emergency_stop()


def build_system():
    cars = [ElevatorCar(1), ElevatorCar(2)]
    dispatcher = Dispatcher(cars)
    emergency = EmergencySystem(cars, dispatcher)

    threads = []
    for car in cars:
        t = threading.Thread(target=car.run, daemon=True)
        t.start()
        threads.append(t)
    dt = threading.Thread(target=dispatcher.run, daemon=True)
    dt.start()
    threads.append(dt)

    return cars, dispatcher, emergency, threads


if __name__ == "__main__":
    cars, dispatcher, emergency, threads = build_system()
    print("Naive elevator system running. Press Ctrl+C to stop.")
    print(f"Cars: {[c.car_id for c in cars]}")
    try:
        # Issue a few test requests
        dispatcher.request(5, Direction.UP)
        time.sleep(0.1)
        dispatcher.request(3, Direction.DOWN)
        time.sleep(5)
        print(f"Car 1 floor: {cars[0].motor.current_floor}, "
              f"door: {cars[0].door.state.value}")
        print(f"Car 2 floor: {cars[1].motor.current_floor}, "
              f"door: {cars[1].door.state.value}")
    except KeyboardInterrupt:
        for c in cars:
            c.stop()
        dispatcher._running = False
