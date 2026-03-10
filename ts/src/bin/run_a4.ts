#!/usr/bin/env node
// run_a4.ts — Execute an ActorForth .a4 file through the TypeScript interpreter.
//
// Usage: node dist/bin/run_a4.js <file.a4>

import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { initTypes, evaluate, newContinuation } from "../index.js";
import { AF_Error } from "../error.js";

const file = process.argv[2];
if (!file) {
  console.error("Usage: run_a4 <file.a4>");
  process.exit(1);
}

const absPath = resolve(file);
let content: string;
try {
  content = readFileSync(absPath, "utf-8");
} catch (e) {
  console.error(`Cannot read file '${absPath}': ${e}`);
  process.exit(1);
}

try {
  initTypes();
  evaluate(content, newContinuation(), absPath);
} catch (e) {
  if (e instanceof AF_Error) {
    console.error(e.format());
  } else {
    console.error(e);
  }
  process.exit(1);
}
