// debounce_typescript_equivalent.ts — TypeScript equivalent of debounce_demo.a4
//
// Idiomatic TypeScript: class-per-actor with internal async message queue.
// Run with:  npx tsx debounce_typescript_equivalent.ts
//
// Comparison with a4:
//   debounce_demo.a4:                       ~80 lines
//   debounce_typescript_equivalent.ts:      ~160 lines
//
// What the extra lines are:
//   - No built-in actor primitive. Each Actor subclasses a base that we
//     implement: message queue, dispatch loop, start/stop lifecycle,
//     cast/call pattern.
//   - Named interfaces per message type — no stack-based composition.
//   - TypeScript's structural typing helps at compile time but gives no
//     runtime enforcement: a typo in a message field type-checks if the
//     shapes happen to overlap.

// ============================================================
// Product types (interfaces + constructor functions)
// ============================================================

interface Entry {
  path: string;
  hash: string;
  size: number;
  kind: string;
}

interface Stats {
  sizeDiff: number;
  countDiff: number;
}

interface Batch {
  added: number;
  deleted: number;
  sizeDelta: number;
  countDelta: number;
}

// ============================================================
// Actor base class (no native primitive in TS)
// ============================================================

type Handler<S> = (state: S, ...args: any[]) => any;

abstract class Actor<S> {
  protected state: S;
  private mailbox: Array<{
    kind: "cast" | "call";
    name: string;
    args: any[];
    resolver?: (v: any) => void;
  }> = [];
  private processing = false;

  constructor(initial: S) {
    this.state = initial;
  }

  protected abstract handlers(): Record<string, Handler<S>>;

  cast(name: string, ...args: any[]): void {
    this.mailbox.push({ kind: "cast", name, args });
    this.pump();
  }

  call<T>(name: string, ...args: any[]): Promise<T> {
    return new Promise<T>((resolve) => {
      this.mailbox.push({ kind: "call", name, args, resolver: resolve });
      this.pump();
    });
  }

  private async pump(): Promise<void> {
    if (this.processing) return;
    this.processing = true;
    try {
      while (this.mailbox.length > 0) {
        const msg = this.mailbox.shift()!;
        const handler = this.handlers()[msg.name];
        if (!handler) throw new Error(`unknown message: ${msg.name}`);
        const result = handler(this.state, ...msg.args);
        if (msg.kind === "call") msg.resolver!(result);
      }
    } finally {
      this.processing = false;
    }
  }
}

// ============================================================
// StoreActor — counts recorded events
// ============================================================

class StoreActor extends Actor<{ events: number }> {
  constructor() { super({ events: 0 }); }
  protected handlers() {
    return {
      record: (s: { events: number }, n: number) => { s.events += n; },
      total:  (s: { events: number }) => s.events,
    };
  }
}

// ============================================================
// TallyActor — counts logged batches
// ============================================================

class TallyActor extends Actor<{ batches: number }> {
  constructor() { super({ batches: 0 }); }
  protected handlers() {
    return {
      log_batch: (s: { batches: number }, _b: Batch) => { s.batches += 1; },
      logged:    (s: { batches: number }) => s.batches,
    };
  }
}

// ============================================================
// Tests
// ============================================================

function assert(cond: unknown, msg: string): void {
  if (!cond) {
    console.error("ASSERT FAIL:", msg);
    process.exit(1);
  }
}

async function main(): Promise<void> {
  // Product type construction and access
  const b: Batch = { added: 5, deleted: 3, sizeDelta: 200, countDelta: 2 };
  assert(b.added === 5, "batch added");
  assert(b.deleted === 3, "batch deleted");
  assert(b.sizeDelta === 200, "batch sizeDelta");
  assert(b.countDelta === 2, "batch countDelta");

  const s: Stats = { sizeDiff: 1024, countDiff: 1 };
  assert(s.sizeDiff === 1024, "stats sizeDiff");
  assert(s.countDiff === 1, "stats countDiff");

  const e: Entry = {
    path: "/docs/report.pdf",
    hash: "abc123",
    size: 4096,
    kind: "application/pdf",
  };
  assert(e.path === "/docs/report.pdf", "entry path");
  assert(e.hash === "abc123", "entry hash");

  // Actors
  const store = new StoreActor();
  const tally = new TallyActor();

  tally.cast("log_batch", { added: 5, deleted: 3, sizeDelta: 200, countDelta: 2 });
  tally.cast("log_batch", { added: 10, deleted: 1, sizeDelta: 500, countDelta: 9 });

  assert((await tally.call<number>("logged")) === 2, "tally logged 2");

  store.cast("record", 3);
  store.cast("record", 7);
  assert((await store.call<number>("total")) === 10, "store total 10");

  console.log("debounce.ts: all assertions passed");
}

main();
