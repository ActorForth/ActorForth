// Browser entry point — exposes ActorForth as a global for use in web pages.
// Bundle with: npx esbuild src/browser.ts --bundle --outfile=dist/actorforth.js --format=iife --global-name=ActorForth

import { initTypes, evaluate, newContinuation, parse, formatItem, formatItemTyped } from "./index.js";
import { Continuation } from "./continuation.js";
import { AF_Error } from "./error.js";
import { StackItem } from "./stack_item.js";

// Override console.log capture for browser output
let outputBuffer: string[] = [];

function captureLog(fn: () => void): string[] {
  outputBuffer = [];
  const origLog = console.log;
  console.log = (...args: unknown[]) => {
    outputBuffer.push(args.map(String).join(" "));
  };
  try {
    fn();
  } finally {
    console.log = origLog;
  }
  return outputBuffer;
}

export interface EvalResult {
  stack: Array<{ type: string; value: unknown; display: string }>;
  output: string[];
  error?: string;
}

let continuation: Continuation | null = null;

export function init(): void {
  initTypes();
  continuation = newContinuation();
}

export function eval_(input: string): EvalResult {
  if (!continuation) init();

  try {
    const output = captureLog(() => {
      continuation = evaluate(input, continuation!, "browser");
    });

    return {
      stack: continuation!.dataStack.map((item: StackItem) => ({
        type: item[0],
        value: item[1],
        display: formatItemTyped(item),
      })),
      output,
    };
  } catch (e) {
    const errorMsg = e instanceof AF_Error ? e.format() : String(e);
    return {
      stack: continuation?.dataStack.map((item: StackItem) => ({
        type: item[0],
        value: item[1],
        display: formatItemTyped(item),
      })) ?? [],
      output: [],
      error: errorMsg,
    };
  }
}

export function reset(): void {
  initTypes();
  continuation = newContinuation();
}

export function getStack(): StackItem[] {
  return continuation?.dataStack ?? [];
}

// Re-export for direct use
export { parse, formatItem, formatItemTyped, newContinuation, evaluate };
