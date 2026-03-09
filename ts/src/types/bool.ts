// Bool type — boolean operations and comparisons.

import * as Type from "../af_type.js";
import { makeOp } from "../operation.js";
import { withStack } from "../continuation.js";

export function init(): void {
  Type.registerType({ name: "Bool", ops: new Map() });

  // bool : Atom -> Bool
  Type.addOp("Any", makeOp("bool", ["Atom"], ["Bool"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const val = (item[1] as string).toLowerCase();
    if (val === "true") return withStack(cont, [["Bool", true], ...rest]);
    if (val === "false") return withStack(cont, [["Bool", false], ...rest]);
    throw new Error(`Cannot convert '${item[1]}' to Bool`);
  }));

  // Constructor pass-through: bool (Bool -> Bool) — no-op when already Bool
  Type.addOp("Any", makeOp("bool", ["Bool"], ["Bool"], (cont) => cont));

  // literal : Atom -> Bool  (handles True/False and true/false)
  Type.addOp("Bool", makeOp("literal", ["Atom"], ["Bool"], (cont) => {
    const [item] = cont.dataStack;
    const val = (item[1] as string).toLowerCase();
    if (val === "true") return withStack(cont, [["Bool", true], ...cont.dataStack.slice(1)]);
    if (val === "false") return withStack(cont, [["Bool", false], ...cont.dataStack.slice(1)]);
    throw new Error("not a boolean");
  }));

  // not : Bool -> Bool
  Type.addOp("Bool", makeOp("not", ["Bool"], ["Bool"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", !(a[1] as boolean)], ...rest]);
  }));

  // and : Bool Bool -> Bool
  Type.addOp("Bool", makeOp("and", ["Bool", "Bool"], ["Bool"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", (b[1] as boolean) && (a[1] as boolean)], ...rest]);
  }));

  // or : Bool Bool -> Bool
  Type.addOp("Bool", makeOp("or", ["Bool", "Bool"], ["Bool"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", (b[1] as boolean) || (a[1] as boolean)], ...rest]);
  }));

  // Comparison operators — registered in Any dict so they work on any types
  // == : Any Any -> Bool
  Type.addOp("Any", makeOp("==", ["Any", "Any"], ["Bool"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", a[0] === b[0] && a[1] === b[1]], ...rest]);
  }));

  // != : Any Any -> Bool
  Type.addOp("Any", makeOp("!=", ["Any", "Any"], ["Bool"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", a[0] !== b[0] || a[1] !== b[1]], ...rest]);
  }));

  // Numeric/string comparisons — work on same-type pairs
  for (const [name, fn] of [
    ["<",  (x: number, y: number) => x < y],
    [">",  (x: number, y: number) => x > y],
    ["<=", (x: number, y: number) => x <= y],
    [">=", (x: number, y: number) => x >= y],
  ] as const) {
    Type.addOp("Any", makeOp(name, ["Any", "Any"], ["Bool"], (cont) => {
      const [a, b, ...rest] = cont.dataStack;
      const result = fn(b[1] as number, a[1] as number);
      return withStack(cont, [["Bool", result], ...rest]);
    }));
  }
}
