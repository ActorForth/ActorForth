// demos.test.ts — Run all portable .a4 demo scripts through the TS interpreter.
// Each script's internal assert/assert-eq calls verify correctness.

import { jest } from "@jest/globals";
import { readdirSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { initTypes, evaluate, newContinuation } from "../src/index.js";

const __dirname = dirname(fileURLToPath(import.meta.url));
const SAMPLES_DIR = resolve(__dirname, "../samples");

// Scripts that use print (console output) — suppress during tests
const PRINT_SCRIPTS = ["pattern_demo.a4", "testloop.a4", "testloop2.a4"];

const files = readdirSync(SAMPLES_DIR)
  .filter(f => f.endsWith(".a4"))
  .sort();

beforeEach(() => {
  initTypes();
});

describe("Demo scripts", () => {
  for (const file of files) {
    test(`${file}`, () => {
      const absPath = resolve(SAMPLES_DIR, file);
      const content = readFileSync(absPath, "utf-8");

      // Suppress console.log for scripts that print
      let spy: jest.Spied<typeof console.log> | undefined;
      if (PRINT_SCRIPTS.includes(file)) {
        spy = jest.spyOn(console, "log").mockImplementation(() => {});
      }

      try {
        evaluate(content, newContinuation(), absPath);
      } finally {
        spy?.mockRestore();
      }
    });
  }
});
