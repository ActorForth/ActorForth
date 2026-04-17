"""auction.py — Python reference implementation of the a4 auction demo.

Idiomatic Python: asyncio + dataclass. One Auction per task/asyncio.Lock.
`bid` is an async method guarded by the lock so concurrent bids are
serialised.
"""

from __future__ import annotations

import asyncio
from dataclasses import dataclass
from enum import Enum


class Status(Enum):
    OPEN = 0
    CLOSED = 1


@dataclass
class Auction:
    item: str
    current_bid: int
    highest_bidder: str
    status: Status = Status.OPEN

    def __post_init__(self) -> None:
        self._lock = asyncio.Lock()

    async def bid(self, amount: int, bidder: str) -> bool:
        """Try to place a bid. Returns True if accepted."""
        async with self._lock:
            if self.status is Status.CLOSED:
                return False
            if amount <= self.current_bid:
                return False
            self.current_bid = amount
            self.highest_bidder = bidder
            return True

    async def close(self) -> None:
        async with self._lock:
            self.status = Status.CLOSED

    async def leader(self) -> str:
        async with self._lock:
            return self.highest_bidder

    async def leading_bid(self) -> int:
        async with self._lock:
            return self.current_bid

    async def is_open(self) -> bool:
        async with self._lock:
            return self.status is Status.OPEN


async def main() -> None:
    a = Auction(item="laptop", current_bid=100, highest_bidder="")

    assert await a.bid(150, "alice")
    assert await a.leader() == "alice"
    assert await a.leading_bid() == 150

    assert not await a.bid(120, "bob")  # too low
    assert await a.leader() == "alice"
    assert await a.leading_bid() == 150

    assert await a.bid(200, "bob")
    assert await a.leader() == "bob"
    assert await a.leading_bid() == 200

    await a.close()
    assert not await a.is_open()

    assert not await a.bid(500, "carol")  # closed
    assert await a.leader() == "bob"
    assert await a.leading_bid() == 200

    print("auction.py: all assertions passed")


if __name__ == "__main__":
    asyncio.run(main())
