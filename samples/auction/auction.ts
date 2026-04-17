// auction.ts — TypeScript reference implementation of the a4 auction demo.
//
// Idiomatic TypeScript: class-based, async methods, discriminated union for
// status. One mutex per auction would be natural, but single-threaded JS /
// Deno / Node means we only need an async queue if we wanted to span
// cooperative tasks. Run with:
//
//   npx tsx auction.ts

type Status = "open" | "closed";

class Auction {
  item: string;
  currentBid: number;
  highestBidder: string;
  status: Status = "open";

  constructor(item: string, startBid: number, startBidder: string) {
    this.item = item;
    this.currentBid = startBid;
    this.highestBidder = startBidder;
  }

  async bid(amount: number, bidder: string): Promise<boolean> {
    if (this.status === "closed") return false;
    if (amount <= this.currentBid) return false;
    this.currentBid = amount;
    this.highestBidder = bidder;
    return true;
  }

  async close(): Promise<void> {
    this.status = "closed";
  }

  async leader(): Promise<string> {
    return this.highestBidder;
  }

  async leadingBid(): Promise<number> {
    return this.currentBid;
  }

  async isOpen(): Promise<boolean> {
    return this.status === "open";
  }
}

function assert(cond: unknown, msg: string): void {
  if (!cond) {
    console.error("ASSERT FAIL:", msg);
    process.exit(1);
  }
}

async function main(): Promise<void> {
  const a = new Auction("laptop", 100, "");

  assert(await a.bid(150, "alice"), "alice 150 accepted");
  assert((await a.leader()) === "alice", "leader is alice");
  assert((await a.leadingBid()) === 150, "leading bid is 150");

  assert(!(await a.bid(120, "bob")), "bob 120 rejected (too low)");
  assert((await a.leader()) === "alice", "leader still alice");
  assert((await a.leadingBid()) === 150, "leading bid still 150");

  assert(await a.bid(200, "bob"), "bob 200 accepted");
  assert((await a.leader()) === "bob", "leader is bob");
  assert((await a.leadingBid()) === 200, "leading bid is 200");

  await a.close();
  assert(!(await a.isOpen()), "auction is closed");

  assert(!(await a.bid(500, "carol")), "carol 500 rejected (closed)");
  assert((await a.leader()) === "bob", "leader still bob");
  assert((await a.leadingBid()) === 200, "leading bid still 200");

  console.log("auction.ts: all assertions passed");
}

main();
