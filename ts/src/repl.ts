// REPL — interactive Read-Eval-Print Loop for ActorForth in Node.js.

import * as readline from "node:readline";
import { initTypes, evaluate, newContinuation, formatItemTyped } from "./index.js";
import { Continuation } from "./continuation.js";
import { AF_Error } from "./error.js";

function printStack(cont: Continuation): void {
  if (cont.dataStack.length === 0) return;
  const items = cont.dataStack.map(formatItemTyped).join(" ");
  process.stdout.write(`  ${items}\n`);
}

async function main(): Promise<void> {
  initTypes();
  let cont = newContinuation();

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: "a4> ",
  });

  console.log("ActorForth TypeScript. ^D to exit.");
  rl.prompt();

  rl.on("line", (line: string) => {
    const input = line.trim();
    if (input.length === 0) {
      rl.prompt();
      return;
    }

    try {
      cont = evaluate(input, cont, "stdin");
      printStack(cont);
    } catch (e) {
      if (e instanceof AF_Error) {
        console.error(e.format());
      } else {
        console.error(`Error: ${e}`);
      }
    }

    rl.prompt();
  });

  rl.on("close", () => {
    console.log("\nBye.");
    process.exit(0);
  });
}

main();
