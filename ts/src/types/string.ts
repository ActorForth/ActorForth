// String type — string operations on JS string values.

import * as Type from "../af_type.js";
import { makeOp } from "../operation.js";
import { withStack } from "../continuation.js";
import { raise } from "../error.js";

export function init(): void {
  Type.registerType({ name: "String", ops: new Map() });

  // Constructor: string (Atom -> String)
  Type.addOp("Any", makeOp("string", ["Atom"], ["String"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    return withStack(cont, [["String", String(item[1])], ...rest]);
  }));

  // Constructor pass-through: string (String -> String) — no-op when already String
  Type.addOp("Any", makeOp("string", ["String"], ["String"], (cont) => cont));

  // concat : String String -> String
  Type.addOp("String", makeOp("concat", ["String", "String"], ["String"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["String", (b[1] as string) + (a[1] as string)], ...rest]);
  }));

  // length : String -> Int
  Type.addOp("String", makeOp("length", ["String"], ["Int"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", (a[1] as string).length], ...rest]);
  }));

  // trim : String -> String
  Type.addOp("String", makeOp("trim", ["String"], ["String"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["String", (a[1] as string).trim()], ...rest]);
  }));

  // to-upper : String -> String
  Type.addOp("String", makeOp("to-upper", ["String"], ["String"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["String", (a[1] as string).toUpperCase()], ...rest]);
  }));

  // to-lower : String -> String
  Type.addOp("String", makeOp("to-lower", ["String"], ["String"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["String", (a[1] as string).toLowerCase()], ...rest]);
  }));

  // to-atom : String -> Atom
  Type.addOp("String", makeOp("to-atom", ["String"], ["Atom"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["Atom", a[1] as string], ...rest]);
  }));

  // to-int : String -> Int
  Type.addOp("String", makeOp("to-int", ["String"], ["Int"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    const val = parseInt(a[1] as string, 10);
    if (isNaN(val)) raise("type_error", `Cannot convert '${a[1]}' to Int`, cont);
    return withStack(cont, [["Int", val], ...rest]);
  }));

  // to-string : String -> String  (passthrough)
  Type.addOp("String", makeOp("to-string", ["String"], ["String"], (cont) => cont));

  // reverse : String -> String
  Type.addOp("String", makeOp("reverse", ["String"], ["String"], (cont) => {
    const [a, ...rest] = cont.dataStack;
    return withStack(cont, [["String", [...(a[1] as string)].reverse().join("")], ...rest]);
  }));

  // split : String String -> List
  Type.addOp("String", makeOp("split", ["String", "String"], ["List"], (cont) => {
    const [delim, target, ...rest] = cont.dataStack;
    const parts = (target[1] as string).split(delim[1] as string);
    const items = parts.map(s => ["String", s] as [string, unknown]);
    return withStack(cont, [["List", items], ...rest]);
  }));

  // contains : String String -> Bool  (TOS=haystack, 2nd=needle)
  Type.addOp("String", makeOp("contains", ["String", "String"], ["Bool"], (cont) => {
    const [haystack, needle, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", (haystack[1] as string).includes(needle[1] as string)], ...rest]);
  }));

  // starts-with : String String -> Bool  (TOS=target, 2nd=prefix)
  Type.addOp("String", makeOp("starts-with", ["String", "String"], ["Bool"], (cont) => {
    const [target, prefix, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", (target[1] as string).startsWith(prefix[1] as string)], ...rest]);
  }));

  // ends-with : String String -> Bool  (TOS=target, 2nd=suffix)
  Type.addOp("String", makeOp("ends-with", ["String", "String"], ["Bool"], (cont) => {
    const [target, suffix, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", (target[1] as string).endsWith(suffix[1] as string)], ...rest]);
  }));

  // replace : String String String -> String  (pattern replacement target)
  Type.addOp("String", makeOp("replace", ["String", "String", "String"], ["String"], (cont) => {
    const [target, replacement, pattern, ...rest] = cont.dataStack;
    const result = (target[1] as string).replaceAll(pattern[1] as string, replacement[1] as string);
    return withStack(cont, [["String", result], ...rest]);
  }));
}
