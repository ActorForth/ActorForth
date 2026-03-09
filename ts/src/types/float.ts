// Float type — floating point numbers with mixed Int/Float arithmetic.

import * as Type from "../af_type.js";
import { makeOp } from "../operation.js";
import { withStack } from "../continuation.js";

export function init(): void {
  Type.registerType({ name: "Float", ops: new Map() });

  // literal : Atom -> Float  (auto-detect 3.14)
  Type.addOp("Float", makeOp("literal", ["Atom"], ["Float"], (cont) => {
    const [item] = cont.dataStack;
    const str = item[1] as string;
    if (!/^-?\d+\.\d+$/.test(str)) throw new Error("not a float");
    return withStack(cont, [["Float", parseFloat(str)], ...cont.dataStack.slice(1)]);
  }));

  // float : Atom -> Float
  Type.addOp("Any", makeOp("float", ["Atom"], ["Float"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const val = parseFloat(item[1] as string);
    if (isNaN(val)) throw new Error(`Cannot convert '${item[1]}' to Float`);
    return withStack(cont, [["Float", val], ...rest]);
  }));

  // to-float : Int -> Float
  Type.addOp("Int", makeOp("to-float", ["Int"], ["Float"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["Float", a[1] as number], ...rest]);
  }));

  // to-int : Float -> Int
  Type.addOp("Float", makeOp("to-int", ["Float"], ["Int"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", Math.trunc(a[1] as number)], ...rest]);
  }));

  // to-string : Float -> String
  Type.addOp("Float", makeOp("to-string", ["Float"], ["String"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["String", String(a[1])], ...rest]);
  }));

  // Float-Float arithmetic
  for (const [name, fn] of [
    ["+", (a: number, b: number) => b + a],
    ["-", (a: number, b: number) => b - a],
    ["*", (a: number, b: number) => b * a],
    ["/", (a: number, b: number) => b / a],
  ] as const) {
    Type.addOp("Float", makeOp(name, ["Float", "Float"], ["Float"], (cont) => {
      const [a, b, ...rest] = cont.dataStack;
      return withStack(cont, [["Float", fn(a[1] as number, b[1] as number)], ...rest]);
    }));
  }

  // Mixed Int-Float and Float-Int arithmetic
  for (const [name, fn] of [
    ["+", (a: number, b: number) => b + a],
    ["-", (a: number, b: number) => b - a],
    ["*", (a: number, b: number) => b * a],
    ["/", (a: number, b: number) => b / a],
  ] as const) {
    // Int on TOS, Float below
    Type.addOp("Int", makeOp(name, ["Int", "Float"], ["Float"], (cont) => {
      const [a, b, ...rest] = cont.dataStack;
      return withStack(cont, [["Float", fn(a[1] as number, b[1] as number)], ...rest]);
    }));
    // Float on TOS, Int below
    Type.addOp("Float", makeOp(name, ["Float", "Int"], ["Float"], (cont) => {
      const [a, b, ...rest] = cont.dataStack;
      return withStack(cont, [["Float", fn(a[1] as number, b[1] as number)], ...rest]);
    }));
  }
}
