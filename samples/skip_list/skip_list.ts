// skip_list.ts — TypeScript reference implementation of skip_list.a4.
//   npx tsx skip_list.ts

const MAX_LEVEL = 4;

// Deterministic-for-tests RNG.
class Rand {
  private state: number;
  constructor(seed: number) { this.state = seed; }
  // Simple xorshift, good enough for test reproducibility.
  nextInt(mod: number): number {
    let x = this.state;
    x ^= x << 13; x ^= x >>> 17; x ^= x << 5;
    this.state = x >>> 0;
    return (this.state % mod);
  }
}

const rng = new Rand(42);

function randomLevel(): number {
  let lvl = 0;
  while (lvl < MAX_LEVEL - 1 && rng.nextInt(2) === 1) lvl++;
  return lvl;
}

function sortedInsert(lst: number[], x: number): void {
  for (let i = 0; i < lst.length; i++) {
    if (lst[i] >= x) { lst.splice(i, 0, x); return; }
  }
  lst.push(x);
}

class SkipList {
  data = new Map<number, number>();
  levels: number[][] = Array.from({ length: MAX_LEVEL }, () => []);

  insert(key: number, value: number): void {
    if (!this.data.has(key)) {
      const lvl = randomLevel();
      for (let i = 0; i <= lvl; i++) sortedInsert(this.levels[i], key);
    }
    this.data.set(key, value);
  }

  delete(key: number): void {
    if (!this.data.has(key)) return;
    for (const lvl of this.levels) {
      const idx = lvl.indexOf(key);
      if (idx >= 0) lvl.splice(idx, 1);
    }
    this.data.delete(key);
  }

  has(key: number): boolean { return this.data.has(key); }
  get(key: number): number | undefined { return this.data.get(key); }
  size(): number { return this.data.size; }
  keys(): number[] { return [...this.levels[0]]; }
}

function assert(cond: unknown, msg: string): void {
  if (!cond) { console.error("ASSERT FAIL:", msg); process.exit(1); }
}

// sorted-insert
{
  const lst: number[] = [];
  for (const x of [3, 1, 5, 2]) sortedInsert(lst, x);
  assert(lst[0] === 1, "sorted-insert first is 1");
  assert(lst.length === 4, "sorted-insert length 4");
  console.log("sorted-insert: ok");
}

// empty
{
  const sl = new SkipList();
  assert(sl.size() === 0, "empty size");
  console.log("sl-new: ok");
}

// single
{
  const sl = new SkipList();
  sl.insert(10, 100);
  assert(sl.has(10), "has 10");
  assert(sl.get(10) === 100, "get 10 -> 100");
  assert(sl.size() === 1, "size 1");
  console.log("single insert: ok");
}

// multi
{
  const sl = new SkipList();
  for (const [k, v] of [[5,50],[3,30],[8,80],[1,10]]) sl.insert(k, v);
  assert(sl.size() === 4, "size 4");
  assert(sl.get(1) === 10, "get 1");
  assert(sl.get(3) === 30, "get 3");
  assert(sl.get(5) === 50, "get 5");
  assert(sl.get(8) === 80, "get 8");
  assert(sl.keys()[0] === 1, "keys first is 1");
  console.log("multi insert: ok");

  sl.delete(3);
  assert(sl.size() === 3, "size after delete");
  assert(!sl.has(3), "3 gone");
  console.log("delete: ok");

  sl.delete(99);
  assert(sl.size() === 3, "delete non-existent is no-op");

  sl.insert(5, 999);
  assert(sl.get(5) === 999, "update");
  console.log("update: ok");
}

// larger
{
  const sl = new SkipList();
  for (const [k, v] of [[20,200],[10,100],[30,300],[15,150],
                         [25,250],[5,50],[35,350]]) sl.insert(k, v);
  assert(sl.size() === 7, "size 7");
  assert(sl.get(5) === 50, "get 5");
  assert(sl.get(35) === 350, "get 35");
  console.log("Skip list with 7 entries:");
  sl.levels.forEach((lvl, i) => console.log(`  Level ${i}: ${lvl.length} keys`));
  console.log("large dataset: ok");
}
