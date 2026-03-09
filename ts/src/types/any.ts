// Any type — global operations available to all types.
// dup, drop, swap, rot, over, 2dup, print, stack, words, types, see, assert, assert-eq

import * as Type from "../af_type.js";
import { makeOp } from "../operation.js";
import { Continuation, withStack, withDebug } from "../continuation.js";
import { StackItem, formatItemTyped, formatItem } from "../stack_item.js";
import { raise } from "../error.js";
import { parse } from "../parser.js";
import { interpretTokens } from "../interpreter.js";
import { readFileSync } from "node:fs";
import { isAbsolute, dirname, resolve as pathResolve } from "node:path";

export function init(): void {
  // dup : _a -> _a _a
  Type.addOp("Any", makeOp("dup", ["_a"], ["_a", "_a"], (cont) => {
    const [top, ...rest] = cont.dataStack;
    return withStack(cont, [top, top, ...rest]);
  }));

  // drop : _ ->
  Type.addOp("Any", makeOp("drop", ["_"], [], (cont) => {
    const [, ...rest] = cont.dataStack;
    return withStack(cont, rest);
  }));

  // swap : _a _b -> _b _a
  Type.addOp("Any", makeOp("swap", ["_a", "_b"], ["_b", "_a"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [b, a, ...rest]);
  }));

  // rot : _a _b _c -> _c _a _b
  Type.addOp("Any", makeOp("rot", ["_a", "_b", "_c"], ["_c", "_a", "_b"], (cont) => {
    const [a, b, c, ...rest] = cont.dataStack;
    return withStack(cont, [c, a, b, ...rest]);
  }));

  // over : _a _b -> _b _a _b
  Type.addOp("Any", makeOp("over", ["_a", "_b"], ["_b", "_a", "_b"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [b, a, b, ...rest]);
  }));

  // 2dup : _a _b -> _a _b _a _b
  Type.addOp("Any", makeOp("2dup", ["_a", "_b"], ["_a", "_b", "_a", "_b"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [a, b, a, b, ...rest]);
  }));

  // print : Any ->
  Type.addOp("Any", makeOp("print", ["Any"], [], (cont) => {
    const [top, ...rest] = cont.dataStack;
    console.log(formatItem(top));
    return withStack(cont, rest);
  }));

  // stack : ->  (displays stack contents)
  Type.addOp("Any", makeOp("stack", [], [], (cont) => {
    const stack = cont.dataStack;
    if (stack.length === 0) {
      console.log("Stack empty");
    } else {
      console.log(`Stack(${stack.length}):`);
      stack.forEach((item, i) => {
        console.log(`  ${i}) ${formatItemTyped(item)}`);
      });
    }
    return cont;
  }));

  // words : ->  (displays available words)
  Type.addOp("Any", makeOp("words", [], [], (cont) => {
    for (const typeDef of Type.allTypes()) {
      if (typeDef.ops.size === 0) continue;
      const words = Array.from(typeDef.ops.keys()).sort();
      console.log(`${typeDef.name} : ${words.join(" ")}`);
    }
    return cont;
  }));

  // types : ->  (displays registered types)
  Type.addOp("Any", makeOp("types", [], [], (cont) => {
    const names = Type.allTypes().map(t => t.name).sort();
    console.log(`Types: ${names.join(" ")}`);
    return cont;
  }));

  // see : Atom ->  (display source/definition of a word)
  Type.addOp("Any", makeOp("see", ["Atom"], [], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const wordName = item[1] as string;
    let found = false;
    for (const typeDef of Type.allTypes()) {
      const ops = typeDef.ops.get(wordName);
      if (!ops) continue;
      for (const op of ops) {
        found = true;
        const sigInStr = op.sigIn.map(s => Array.isArray(s) ? `${s[0]}(${s[1]})` : s).join(" ");
        const sigOutStr = op.sigOut.map(s => Array.isArray(s) ? `${s[0]}(${s[1]})` : s).join(" ");
        let sourceStr = "[built-in]";
        if (op.source?.kind === "compiled") {
          sourceStr = op.source.body.map(o => o.name).join(" ") + " .";
        } else if (op.source?.kind === "auto") {
          sourceStr = "[auto-generated]";
        }
        console.log(`  : ${op.name} ${sigInStr} -> ${sigOutStr} ; ${sourceStr}`);
        console.log(`    in type: ${typeDef.name}`);
      }
    }
    if (!found) console.log(`Word '${wordName}' not found.`);
    return withStack(cont, rest);
  }));

  // assert : Bool ->
  Type.addOp("Any", makeOp("assert", ["Bool"], [], (cont) => {
    const [item, ...rest] = cont.dataStack;
    if (item[1] !== true) {
      raise("assertion_failed", "Assertion failed: expected True on stack", cont);
    }
    return withStack(cont, rest);
  }));

  // assert-eq : Any Any ->
  Type.addOp("Any", makeOp("assert-eq", ["Any", "Any"], [], (cont) => {
    const [expected, actual, ...rest] = cont.dataStack;
    if (expected[0] !== actual[0] || expected[1] !== actual[1]) {
      raise("assert_eq_failed",
        `Expected ${formatItemTyped(expected)} but got ${formatItemTyped(actual)}`, cont);
    }
    return withStack(cont, rest);
  }));

  // debug : ->  (pushes Debug marker)
  Type.registerType({ name: "Debug", ops: new Map(), handler: handleDebug });
  Type.addOp("Any", makeOp("debug", [], ["Debug"], (cont) => {
    return withStack(cont, [["Debug", {}], ...cont.dataStack]);
  }));

  // load : String ->  (load and evaluate a .a4 file, Node.js only)
  Type.addOp("Any", makeOp("load", ["String"], [], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const filePath = item[1] as string;
    // Resolve relative paths against current file's directory
    let resolved = filePath;
    if (!isAbsolute(filePath) && cont.currentToken?.file) {
      const dir = dirname(cont.currentToken.file);
      if (dir !== "." && dir !== "eval" && dir !== "stdin" && dir !== "test" && dir !== "browser") {
        resolved = pathResolve(dir, filePath);
      }
    }
    let content: string;
    try {
      content = readFileSync(resolved, "utf-8");
    } catch (e) {
      raise("load_error", `Cannot read file '${resolved}': ${e}`, cont);
    }
    const tokens = parse(content!, resolved);
    return interpretTokens(tokens, withStack(cont, rest));
  }));
}

function handleDebug(tokenValue: string, cont: Continuation): Continuation {
  const [, ...rest] = cont.dataStack;
  if (tokenValue === "on") {
    return withDebug(withStack(cont, rest), true);
  } else if (tokenValue === "off") {
    return withDebug(withStack(cont, rest), false);
  }
  throw new Error(`debug: expected on/off, got '${tokenValue}'`);
}
