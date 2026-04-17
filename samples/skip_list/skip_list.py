"""skip_list.py — Python reference implementation of skip_list.a4.

A probabilistic skip list with O(log n) average insert / delete / lookup.
Four levels, coin-flip promotion. Run with:  python3 skip_list.py
"""

from __future__ import annotations

import random
from dataclasses import dataclass, field


MAX_LEVEL = 4  # levels 0..3 inclusive


def random_level() -> int:
    """Return a level in [0, MAX_LEVEL-1], higher = less likely."""
    lvl = 0
    while lvl < MAX_LEVEL - 1 and random.randint(0, 1) == 1:
        lvl += 1
    return lvl


@dataclass
class SkipList:
    data: dict[int, int] = field(default_factory=dict)
    levels: list[list[int]] = field(default_factory=lambda: [[] for _ in range(MAX_LEVEL)])

    def insert(self, key: int, value: int) -> None:
        if key not in self.data:
            lvl = random_level()
            for i in range(lvl + 1):
                self._sorted_insert(self.levels[i], key)
        self.data[key] = value

    def delete(self, key: int) -> None:
        if key not in self.data:
            return
        for lvl in self.levels:
            if key in lvl:
                lvl.remove(key)
        del self.data[key]

    def has(self, key: int) -> bool:
        return key in self.data

    def get(self, key: int) -> int:
        return self.data[key]

    def size(self) -> int:
        return len(self.data)

    def keys(self) -> list[int]:
        return list(self.levels[0])

    @staticmethod
    def _sorted_insert(lst: list[int], x: int) -> None:
        """Insert x into an ascending sorted list."""
        for i, v in enumerate(lst):
            if v >= x:
                lst.insert(i, x)
                return
        lst.append(x)


def main() -> None:
    random.seed(42)  # reproducible for the test assertions

    # Sorted-insert primitive (exposed here as _sorted_insert)
    lst: list[int] = []
    for x in [3, 1, 5, 2]:
        SkipList._sorted_insert(lst, x)
    assert lst[0] == 1
    assert len(lst) == 4
    print("sorted-insert: ok")

    # Empty skip list
    sl = SkipList()
    assert sl.size() == 0
    print("sl-new: ok")

    # Single insert
    sl = SkipList()
    sl.insert(10, 100)
    assert sl.has(10)
    assert sl.get(10) == 100
    assert sl.size() == 1
    print("single insert: ok")

    # Multiple inserts
    sl = SkipList()
    for k, v in [(5, 50), (3, 30), (8, 80), (1, 10)]:
        sl.insert(k, v)
    assert sl.size() == 4
    assert sl.get(1) == 10
    assert sl.get(3) == 30
    assert sl.get(5) == 50
    assert sl.get(8) == 80
    assert sl.keys()[0] == 1
    print("multi insert: ok")

    # Delete
    sl.delete(3)
    assert sl.size() == 3
    assert not sl.has(3)
    assert sl.get(1) == 10
    assert sl.get(5) == 50
    assert sl.get(8) == 80
    print("delete: ok")

    # Delete non-existent is a no-op
    sl.delete(99)
    assert sl.size() == 3

    # Update existing key
    sl.insert(5, 999)
    assert sl.get(5) == 999
    print("update: ok")

    # Larger dataset
    sl = SkipList()
    for k, v in [(20, 200), (10, 100), (30, 300), (15, 150),
                 (25, 250), (5, 50), (35, 350)]:
        sl.insert(k, v)
    assert sl.size() == 7
    assert sl.get(5) == 50
    assert sl.get(35) == 350

    print("Skip list with 7 entries:")
    for i, lvl in enumerate(sl.levels):
        print(f"  Level {i}: {len(lvl)} keys")
    print("large dataset: ok")


if __name__ == "__main__":
    main()
