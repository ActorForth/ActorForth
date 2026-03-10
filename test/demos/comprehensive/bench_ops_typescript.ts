#!/usr/bin/env npx tsx
/**
 * bench_ops_typescript.ts — Comprehensive operations benchmark (TypeScript)
 *
 * Two modes:
 *   1. TS Native — plain TypeScript functions (same work as Python/C++ benchmarks)
 *   2. A4/TS Interpreted — A4 code running through the TypeScript ActorForth interpreter
 *
 * Outputs BENCH_DATA lines parseable by run_all_benchmarks.py.
 *
 * Usage:
 *   cd ts && npx tsx ../test/demos/comprehensive/bench_ops_typescript.ts
 *   cd ts && npx tsx ../test/demos/comprehensive/bench_ops_typescript.ts --bench 10
 */

import { performance } from "node:perf_hooks";

// ============================================================
// ActorForth TS interpreter imports
// ============================================================
// Imports relative to project root — run from ts/ dir: cd ts && npx tsx ../test/demos/comprehensive/bench_ops_typescript.ts
import { initTypes, evaluate, parse, newContinuation } from "../../../ts/src/index.js";
import { interpretTokens } from "../../../ts/src/interpreter.js";
import { Continuation } from "../../../ts/src/continuation.js";
import { StackItem } from "../../../ts/src/stack_item.js";
import { Token } from "../../../ts/src/token.js";

const N = 10000; // iterations (A4 interpreted is slower; normalize to us/op)

// ============================================================
// TS Native implementations (same computational work as Python/C++)
// ============================================================

function square(x: number): number { return x * x; }
function cube(x: number): number { return x * x * x; }
function arithChain(x: number): number { return Math.abs(square(x) + cube(x)); }

function listOps(): number {
  let lst = [1, 2, 3, 4, 5];
  lst = lst.reverse();
  lst = lst.concat([10, 20]);
  return lst.length;
}

function strWork(s: string): string { return s.trim().toUpperCase(); }
function stringOps(): string { return strWork("  hello world  "); }

function mapOps(): number {
  const m = new Map<string, number>();
  m.set("a", 1);
  m.set("b", 2);
  m.set("c", 3);
  return Array.from(m.keys()).length;
}

class Vec3 {
  constructor(public x: number, public y: number, public z: number) {}
}
function vecMag2(v: Vec3): number {
  return square(v.x) + square(v.y) + square(v.z);
}

function composeTest(x: number): number {
  return Math.abs(Math.max(square(x), cube(x)));
}

// ============================================================
// Benchmark harness
// ============================================================

function bench(label: string, func: () => void, nIters: number = 5): number {
  // Warmup
  func();

  const times: number[] = [];
  for (let i = 0; i < nIters; i++) {
    const start = performance.now();
    func();
    const elapsedUs = (performance.now() - start) * 1000;
    times.push(elapsedUs);
  }

  times.sort((a, b) => a - b);
  const median = times[Math.floor(nIters / 2)];
  const perOp = median / N;
  console.log(`${label}: ${(median / 1000).toFixed(1).padStart(8)} ms  (${perOp.toFixed(3).padStart(9)} us/op)`);
  return median;
}

// ============================================================
// TS Native benchmark runners
// ============================================================

function runNativeArith(n: number): void {
  for (let i = 1; i <= n; i++) arithChain(i);
}
function runNativeListOps(n: number): void {
  for (let i = 0; i < n; i++) listOps();
}
function runNativeStringOps(n: number): void {
  for (let i = 0; i < n; i++) stringOps();
}
function runNativeMapOps(n: number): void {
  for (let i = 0; i < n; i++) mapOps();
}
function runNativeProductOps(n: number): void {
  for (let i = 1; i <= n; i++) vecMag2(new Vec3(i, i + 1, i + 2));
}
function runNativeComposedWords(n: number): void {
  for (let i = 1; i <= n; i++) composeTest(i);
}

// ============================================================
// A4/TS Interpreted benchmark
// ============================================================

function setupA4(): Continuation {
  initTypes();
  let c = newContinuation();
  // Define words (same as Erlang bench_ops.escript)
  c = evaluate(": square Int -> Int ; dup * .", c);
  c = evaluate(": cube Int -> Int ; dup dup * * .", c);
  c = evaluate(": arith-chain Int -> Int ; dup square swap cube + abs .", c);
  c = evaluate(": double Int -> Int ; 2 * .", c);
  c = evaluate(": str-work String -> String ; trim to-upper .", c);
  c = evaluate("type Vec3 x Int y Int z Int .", c);
  c = evaluate(": vec-mag2 Vec3 -> Vec3 Int ; x square swap y square rot + swap z square rot + .", c);
  c = evaluate(": compose-test Int -> Int ; dup square swap cube max abs .", c);
  return c;
}

function runA4Arith(tokens: Token[], cont: Continuation, n: number): void {
  for (let i = 1; i <= n; i++) {
    const c: Continuation = { ...cont, dataStack: [["Int", i] as StackItem] };
    interpretTokens(tokens, c);
  }
}

function runA4List(tokens: Token[], cont: Continuation, n: number): void {
  for (let i = 0; i < n; i++) {
    interpretTokens(tokens, cont);
  }
}

function runA4String(tokens: Token[], cont: Continuation, n: number): void {
  for (let i = 0; i < n; i++) {
    const c: Continuation = { ...cont, dataStack: [["String", "  hello world  "] as StackItem] };
    interpretTokens(tokens, c);
  }
}

function runA4Map(tokens: Token[], cont: Continuation, n: number): void {
  for (let i = 0; i < n; i++) {
    interpretTokens(tokens, cont);
  }
}

function runA4Product(tokens: Token[], cont: Continuation, n: number): void {
  for (let i = 1; i <= n; i++) {
    const c: Continuation = {
      ...cont,
      dataStack: [["Int", i + 2] as StackItem, ["Int", i + 1] as StackItem, ["Int", i] as StackItem],
    };
    interpretTokens(tokens, c);
  }
}

function runA4Composed(tokens: Token[], cont: Continuation, n: number): void {
  for (let i = 1; i <= n; i++) {
    const c: Continuation = { ...cont, dataStack: [["Int", i] as StackItem] };
    interpretTokens(tokens, c);
  }
}

// ============================================================
// Main
// ============================================================

function main(): void {
  let nIters = 5;
  const args = process.argv.slice(2);
  if (args[0] === "--bench" && args[1]) {
    nIters = parseInt(args[1], 10);
  }

  console.log();
  console.log("=== TypeScript Operations Benchmark ===");
  console.log();

  // --- TS Native ---
  console.log(`--- TS Native (${N} iterations, ${nIters} timed runs, median) ---`);

  const tNatArith = bench("  Arithmetic chain",    () => runNativeArith(N), nIters);
  const tNatList  = bench("  List ops",            () => runNativeListOps(N), nIters);
  const tNatStr   = bench("  String ops",          () => runNativeStringOps(N), nIters);
  const tNatMap   = bench("  Map ops",             () => runNativeMapOps(N), nIters);
  const tNatProd  = bench("  Product type ops",    () => runNativeProductOps(N), nIters);
  const tNatComp  = bench("  Compiled word chain", () => runNativeComposedWords(N), nIters);

  const natTotal = tNatArith + tNatList + tNatStr + tNatMap + tNatProd + tNatComp;
  console.log(`\n  TOTAL: ${(natTotal / 1000).toFixed(1).padStart(8)} ms  (${(natTotal / N).toFixed(3).padStart(9)} us/op)`);
  console.log();

  // --- A4/TS Interpreted ---
  console.log(`--- A4/TS Interpreted (${N} iterations, ${nIters} timed runs, median) ---`);

  const cont = setupA4();

  // Pre-parse tokens (done ONCE, reused N times)
  const arithTokens  = parse("arith-chain drop", "bench");
  const listTokens   = parse('nil 1 cons 2 cons 3 cons 4 cons 5 cons reverse nil 10 cons 20 cons append length drop', "bench");
  const stringTokens = parse("str-work drop", "bench");
  const mapTokens    = parse('map-new 1 "a" map-put 2 "b" map-put 3 "c" map-put map-keys length drop', "bench");
  const prodTokens   = parse("vec3 vec-mag2 drop drop", "bench");
  const compTokens   = parse("compose-test drop", "bench");

  const tIntArith = bench("  Arithmetic chain",    () => runA4Arith(arithTokens, cont, N), nIters);
  const tIntList  = bench("  List ops",            () => runA4List(listTokens, cont, N), nIters);
  const tIntStr   = bench("  String ops",          () => runA4String(stringTokens, cont, N), nIters);
  const tIntMap   = bench("  Map ops",             () => runA4Map(mapTokens, cont, N), nIters);
  const tIntProd  = bench("  Product type ops",    () => runA4Product(prodTokens, cont, N), nIters);
  const tIntComp  = bench("  Compiled word chain", () => runA4Composed(compTokens, cont, N), nIters);

  const intTotal = tIntArith + tIntList + tIntStr + tIntMap + tIntProd + tIntComp;
  console.log(`\n  TOTAL: ${(intTotal / 1000).toFixed(1).padStart(8)} ms  (${(intTotal / N).toFixed(3).padStart(9)} us/op)`);
  console.log();

  // --- Speedup ---
  console.log("--- Speedup Summary ---");
  const cats = ["Arithmetic", "List ops", "String ops", "Map ops", "Product", "Word chain"];
  const intTimes = [tIntArith, tIntList, tIntStr, tIntMap, tIntProd, tIntComp];
  const natTimes = [tNatArith, tNatList, tNatStr, tNatMap, tNatProd, tNatComp];
  for (let i = 0; i < cats.length; i++) {
    const speedup = natTimes[i] > 0 ? intTimes[i] / natTimes[i] : 0;
    console.log(`  ${cats[i].padEnd(15)} ${speedup.toFixed(1)}x (TS native -> A4/TS interp)`);
  }
  console.log(`  ${"TOTAL".padEnd(15)} ${(intTotal / natTotal).toFixed(1)}x`);
  console.log();

  // --- Machine-readable output ---
  console.log(
    `BENCH_DATA[ts_native]: arith=${(tNatArith / N).toFixed(3)} list=${(tNatList / N).toFixed(3)} ` +
    `string=${(tNatStr / N).toFixed(3)} map=${(tNatMap / N).toFixed(3)} ` +
    `product=${(tNatProd / N).toFixed(3)} wordchain=${(tNatComp / N).toFixed(3)}`
  );
  console.log(
    `BENCH_DATA[ts_interp]: arith=${(tIntArith / N).toFixed(3)} list=${(tIntList / N).toFixed(3)} ` +
    `string=${(tIntStr / N).toFixed(3)} map=${(tIntMap / N).toFixed(3)} ` +
    `product=${(tIntProd / N).toFixed(3)} wordchain=${(tIntComp / N).toFixed(3)}`
  );
}

main();
