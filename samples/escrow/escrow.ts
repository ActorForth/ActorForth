// escrow.ts — TypeScript reference implementation of the a4 escrow contract.
//   npx tsx escrow.ts

enum Status {
  Pending,
  Funded,
  Verified,
  Disputed,
  Released,
  Refunded,
}

class Escrow {
  buyer: string;
  seller: string;
  verifier: string;
  amount: number;
  status: Status = Status.Pending;

  constructor(buyer: string, seller: string, verifier: string, amount: number) {
    this.buyer = buyer;
    this.seller = seller;
    this.verifier = verifier;
    this.amount = amount;
  }

  deposit(amount: number, caller: string): void {
    if (this.status !== Status.Pending) return;
    if (amount !== this.amount || caller !== this.buyer) return;
    this.status = Status.Funded;
  }

  verify(caller: string): void {
    if (this.status !== Status.Funded) return;
    if (caller !== this.verifier) return;
    this.status = Status.Verified;
  }

  release(): void {
    if (this.status !== Status.Verified) return;
    this.status = Status.Released;
  }

  dispute(caller: string): void {
    if (this.status !== Status.Funded) return;
    if (caller !== this.buyer && caller !== this.seller) return;
    this.status = Status.Refunded;
  }

  state(): Status { return this.status; }
  isReleased(): boolean { return this.status === Status.Released; }
  isRefunded(): boolean { return this.status === Status.Refunded; }
}

function assert(cond: unknown, msg: string): void {
  if (!cond) {
    console.error("ASSERT FAIL:", msg);
    process.exit(1);
  }
}

// Happy path
{
  const e = new Escrow("alice", "bob", "oracle", 1000);
  assert(e.state() === Status.Pending, "starts pending");
  e.deposit(1000, "alice");
  assert(e.state() === Status.Funded, "funded after deposit");
  e.verify("oracle");
  assert(e.state() === Status.Verified, "verified");
  e.release();
  assert(e.isReleased(), "released");
  console.log("escrow.ts happy path: passed");
}

// State guards
{
  const e = new Escrow("alice", "bob", "oracle", 500);
  e.verify("oracle");
  assert(e.state() === Status.Pending, "can't verify before funded");
  e.release();
  assert(e.state() === Status.Pending, "can't release before funded");
  e.deposit(500, "alice");
  e.release();
  assert(e.state() === Status.Funded, "can't release before verify");
  e.verify("oracle");
  e.deposit(999, "alice");
  assert(e.state() === Status.Verified, "can't re-deposit");
  e.release();
  e.release();
  assert(e.isReleased(), "released idempotent");
  console.log("escrow.ts state guards: passed");
}

// Dispute path
{
  const e = new Escrow("alice", "bob", "oracle", 750);
  e.deposit(750, "alice");
  e.dispute("alice");
  assert(e.isRefunded(), "refunded after dispute");
  e.release();
  assert(e.isRefunded(), "release is no-op after refund");
  console.log("escrow.ts dispute path: passed");
}
