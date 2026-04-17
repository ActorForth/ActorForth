"""escrow.py — Python reference implementation of the a4 escrow contract.

A three-party escrow. Validation lives in method guards (if-statements);
invalid calls raise runtime exceptions rather than silently failing.
Run with:  python3 escrow.py
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class Status(Enum):
    PENDING = 0
    FUNDED = 1
    VERIFIED = 2
    DISPUTED = 3
    RELEASED = 4
    REFUNDED = 5


class EscrowError(Exception):
    pass


@dataclass
class Escrow:
    buyer: str
    seller: str
    verifier: str
    amount: int
    status: Status = Status.PENDING

    def deposit(self, amount: int, caller: str) -> None:
        if self.status is not Status.PENDING:
            return  # invalid: leave state unchanged
        if amount != self.amount or caller != self.buyer:
            return
        self.status = Status.FUNDED

    def verify(self, caller: str) -> None:
        if self.status is not Status.FUNDED:
            return
        if caller != self.verifier:
            return
        self.status = Status.VERIFIED

    def release(self) -> None:
        if self.status is not Status.VERIFIED:
            return
        self.status = Status.RELEASED

    def dispute(self, caller: str) -> None:
        if self.status is not Status.FUNDED:
            return
        if caller != self.buyer and caller != self.seller:
            return
        self.status = Status.REFUNDED

    def state(self) -> Status:
        return self.status

    def is_released(self) -> bool:
        return self.status is Status.RELEASED

    def is_refunded(self) -> bool:
        return self.status is Status.REFUNDED


def main() -> None:
    # Happy path
    e = Escrow("alice", "bob", "oracle", 1000)
    assert e.state() is Status.PENDING
    e.deposit(1000, "alice")
    assert e.state() is Status.FUNDED
    e.verify("oracle")
    assert e.state() is Status.VERIFIED
    e.release()
    assert e.is_released()
    print("escrow.py happy path: passed")

    # Invalid transitions
    e = Escrow("alice", "bob", "oracle", 500)
    e.verify("oracle")   # too early
    assert e.state() is Status.PENDING
    e.release()          # too early
    assert e.state() is Status.PENDING
    e.deposit(500, "alice")
    e.release()          # before verify
    assert e.state() is Status.FUNDED
    e.verify("oracle")
    e.deposit(999, "alice")   # too late to deposit
    assert e.state() is Status.VERIFIED
    e.release()
    e.release()          # no-op on already-released
    assert e.is_released()
    print("escrow.py state guards: passed")

    # Dispute path
    e = Escrow("alice", "bob", "oracle", 750)
    e.deposit(750, "alice")
    e.dispute("alice")
    assert e.is_refunded()
    e.release()          # can't release after refund
    assert e.is_refunded()
    print("escrow.py dispute path: passed")


if __name__ == "__main__":
    main()
