// Int type — integer arithmetic and constructor.

import * as Type from "../af_type.js";
import { makeOp } from "../operation.js";
import { withStack } from "../continuation.js";
import { raise } from "../error.js";

export function init(): void {
  Type.registerType({ name: "Int", ops: new Map() });

  // int : Atom -> Int  (explicit constructor)
  Type.addOp("Any", makeOp("int", ["Atom"], ["Int"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const val = parseInt(item[1] as string, 10);
    if (isNaN(val)) raise("type_error", `Cannot convert '${item[1]}' to Int`, cont);
    return withStack(cont, [["Int", val], ...rest]);
  }));

  // literal : Atom -> Int  (auto-detect integer literals)
  Type.addOp("Int", makeOp("literal", ["Atom"], ["Int"], (cont) => {
    const [item] = cont.dataStack;
    const str = item[1] as string;
    if (!/^-?\d+$/.test(str)) throw new Error("not an integer");
    return withStack(cont, [["Int", parseInt(str, 10)], ...cont.dataStack.slice(1)]);
  }));

  // + : Int Int -> Int
  Type.addOp("Int", makeOp("+", ["Int", "Int"], ["Int"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", (b[1] as number) + (a[1] as number)], ...rest]);
  }));

  // - : Int Int -> Int
  Type.addOp("Int", makeOp("-", ["Int", "Int"], ["Int"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", (b[1] as number) - (a[1] as number)], ...rest]);
  }));

  // * : Int Int -> Int
  Type.addOp("Int", makeOp("*", ["Int", "Int"], ["Int"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", (b[1] as number) * (a[1] as number)], ...rest]);
  }));

  // / : Int Int -> Int  (integer division)
  Type.addOp("Int", makeOp("/", ["Int", "Int"], ["Int"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    const divisor = a[1] as number;
    if (divisor === 0) raise("division_by_zero", "Division by zero", cont);
    return withStack(cont, [["Int", Math.trunc((b[1] as number) / divisor)], ...rest]);
  }));

  // mod : Int Int -> Int
  Type.addOp("Int", makeOp("mod", ["Int", "Int"], ["Int"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    const divisor = a[1] as number;
    if (divisor === 0) raise("division_by_zero", "Modulo by zero", cont);
    return withStack(cont, [["Int", (b[1] as number) % divisor], ...rest]);
  }));

  // abs : Int -> Int
  Type.addOp("Int", makeOp("abs", ["Int"], ["Int"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", Math.abs(a[1] as number)], ...rest]);
  }));

  // max : Int Int -> Int
  Type.addOp("Int", makeOp("max", ["Int", "Int"], ["Int"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", Math.max(b[1] as number, a[1] as number)], ...rest]);
  }));

  // min : Int Int -> Int
  Type.addOp("Int", makeOp("min", ["Int", "Int"], ["Int"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", Math.min(b[1] as number, a[1] as number)], ...rest]);
  }));

  // to-string : Int -> String
  Type.addOp("Int", makeOp("to-string", ["Int"], ["String"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["String", String(a[1])], ...rest]);
  }));
}
