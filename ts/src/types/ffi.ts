// JS FFI — call JavaScript from ActorForth.
// Works in both Node.js and browser via globalThis.
//
// Convention: object is pushed first (deeper), property/method name on top.
//   "Math" js-global "PI" js-get     → 3.14159...
//   "Math" js-global 2 "sqrt" 1 js-call  → 1.414...
//   42 "Math" js-global "x" js-set   → sets globalThis.Math.x = 42

import * as Type from "../af_type.js";
import { makeOp } from "../operation.js";
import { Continuation, withStack } from "../continuation.js";
import { StackItem } from "../stack_item.js";
import { raise } from "../error.js";
import { interpretToken } from "../interpreter.js";
import { makeToken } from "../token.js";

// --- Term conversion: JS ↔ A4 StackItem ---
// Only auto-convert primitives. Objects/arrays stay as JsObject to preserve identity.

export function jsToStackItem(val: unknown): StackItem {
  if (val === null || val === undefined) return ["Atom", "nil"];
  if (typeof val === "number") {
    return Number.isInteger(val) ? ["Int", val] : ["Float", val];
  }
  if (typeof val === "string") return ["String", val];
  if (typeof val === "boolean") return ["Bool", val];
  // Everything else (objects, arrays, functions, etc.) stays as JsObject
  return ["JsObject", val];
}

export function stackItemToJs(item: StackItem): unknown {
  const [type, val] = item;
  switch (type) {
    case "Int":
    case "Float":
    case "String":
    case "Bool":
      return val;
    case "Atom":
      return val === "nil" ? null : String(val);
    case "List": {
      const items = val as StackItem[];
      return items.map(stackItemToJs);
    }
    case "Map": {
      const m = val as Map<string, [StackItem, StackItem]>;
      const obj: Record<string, unknown> = {};
      for (const [, [kItem, vItem]] of m) {
        obj[String(stackItemToJs(kItem))] = stackItemToJs(vItem);
      }
      return obj;
    }
    case "JsObject":
      return val;
    default:
      return val;
  }
}

// Pop N items from the stack as JS args (reversed for left-to-right calling convention)
function popArgs(stack: StackItem[], n: number, cont: Continuation): [unknown[], StackItem[]] {
  if (stack.length < n) raise("stack_underflow", `Need ${n} args, stack has ${stack.length}`, cont);
  const args = stack.slice(0, n).map(stackItemToJs).reverse();
  const rest = stack.slice(n);
  return [args, rest];
}

export function init(): void {
  Type.registerType({ name: "JsObject", ops: new Map() });

  // --- JsObject ops ---

  // typeof : JsObject -> String
  Type.addOp("JsObject", makeOp("typeof", ["JsObject"], ["String"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    return withStack(cont, [["String", typeof item[1]], ...rest]);
  }));

  // to-string : JsObject -> String
  Type.addOp("JsObject", makeOp("to-string", ["JsObject"], ["String"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    return withStack(cont, [["String", String(item[1])], ...rest]);
  }));

  // is-null? : JsObject -> Bool
  Type.addOp("JsObject", makeOp("is-null?", ["JsObject"], ["Bool"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", item[1] === null], ...rest]);
  }));

  // is-undefined? : JsObject -> Bool
  Type.addOp("JsObject", makeOp("is-undefined?", ["JsObject"], ["Bool"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", item[1] === undefined], ...rest]);
  }));

  // --- FFI words (registered in Any) ---

  // js-eval : String -> Any
  // Evaluate a JS expression and push the result.
  Type.addOp("Any", makeOp("js-eval", ["String"], ["Any"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const expr = item[1] as string;
    let result: unknown;
    try {
      result = (0, eval)(expr); // indirect eval for global scope
    } catch (e) {
      raise("js_error", `js-eval failed: ${e}`, cont);
    }
    return withStack(cont, [jsToStackItem(result), ...rest]);
  }));

  // js-global : String -> Any
  // Access a global variable by name. Returns JsObject for objects.
  Type.addOp("Any", makeOp("js-global", ["String"], ["Any"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const name = item[1] as string;
    const val = (globalThis as Record<string, unknown>)[name];
    return withStack(cont, [jsToStackItem(val), ...rest]);
  }));

  // js-get : String JsObject -> Any
  // Get a property from a JS object. ( obj "propName" -- value )
  // TOS = String(propName), 2nd = JsObject(obj)
  Type.addOp("Any", makeOp("js-get", ["Any", "Any"], ["Any"], (cont) => {
    const [propItem, objItem, ...rest] = cont.dataStack;
    const jsObj = stackItemToJs(objItem);
    const propName = stackItemToJs(propItem) as string;
    if (jsObj == null) raise("js_error", `Cannot get property '${propName}' of ${jsObj}`, cont);
    const val = (jsObj as Record<string, unknown>)[propName];
    return withStack(cont, [jsToStackItem(val), ...rest]);
  }));

  // js-set : Any String JsObject ->
  // Set a property on a JS object. ( value obj "propName" -- )
  // TOS = String(propName), 2nd = JsObject(obj), 3rd = Any(value)
  Type.addOp("Any", makeOp("js-set", ["Any", "Any", "Any"], [], (cont) => {
    const [propItem, objItem, valueItem, ...rest] = cont.dataStack;
    const jsObj = stackItemToJs(objItem) as Record<string, unknown>;
    const propName = stackItemToJs(propItem) as string;
    jsObj[propName] = stackItemToJs(valueItem);
    return withStack(cont, rest);
  }));

  // js-call : Int String JsObject -> Any
  // Call a method on a JS object with N args from stack.
  // ( arg1 ... argN obj "methodName" arity -- result )
  // TOS = Int(arity), 2nd = String(methodName), 3rd = JsObject(obj), then args
  Type.addOp("Any", makeOp("js-call", ["Int", "Any", "Any"], ["Any"], (cont) => {
    const [arityItem, methodItem, objItem, ...afterFixed] = cont.dataStack;
    const arity = arityItem[1] as number;
    const methodName = stackItemToJs(methodItem) as string;
    const jsObj = stackItemToJs(objItem) as Record<string, unknown>;
    const [args, rest] = popArgs(afterFixed, arity, cont);
    if (typeof jsObj[methodName] !== "function") {
      raise("js_error", `'${methodName}' is not a function`, cont);
    }
    let result: unknown;
    try {
      result = (jsObj[methodName] as Function).apply(jsObj, args);
    } catch (e) {
      raise("js_error", `js-call '${methodName}' failed: ${e}`, cont);
    }
    return withStack(cont, [jsToStackItem(result), ...rest]);
  }));

  // js-call0 : String JsObject -> Any
  // Call a zero-arg method on a JS object. ( obj "methodName" -- result )
  // TOS = String(methodName), 2nd = JsObject(obj)
  Type.addOp("Any", makeOp("js-call0", ["Any", "Any"], ["Any"], (cont) => {
    const [methodItem, objItem, ...rest] = cont.dataStack;
    const methodName = stackItemToJs(methodItem) as string;
    const jsObj = stackItemToJs(objItem) as Record<string, unknown>;
    if (typeof jsObj[methodName] !== "function") {
      raise("js_error", `'${methodName}' is not a function`, cont);
    }
    let result: unknown;
    try {
      result = (jsObj[methodName] as Function).call(jsObj);
    } catch (e) {
      raise("js_error", `js-call0 '${methodName}' failed: ${e}`, cont);
    }
    return withStack(cont, [jsToStackItem(result), ...rest]);
  }));

  // js-new : Int String -> Any
  // Construct a JS object. ( arg1 ... argN "constructorName" arity -- instance )
  // TOS = Int(arity), 2nd = String(constructorName), then args
  Type.addOp("Any", makeOp("js-new", ["Int", "Any"], ["Any"], (cont) => {
    const [arityItem, nameItem, ...afterFixed] = cont.dataStack;
    const arity = arityItem[1] as number;
    const ctorName = stackItemToJs(nameItem) as string;
    const ctor = (globalThis as Record<string, unknown>)[ctorName] as (new (...args: unknown[]) => unknown) | undefined;
    if (typeof ctor !== "function") {
      raise("js_error", `'${ctorName}' is not a constructor`, cont);
    }
    const [args, rest] = popArgs(afterFixed, arity, cont);
    let result: unknown;
    try {
      result = new ctor(...args);
    } catch (e) {
      raise("js_error", `js-new '${ctorName}' failed: ${e}`, cont);
    }
    return withStack(cont, [jsToStackItem(result), ...rest]);
  }));

  // js-callback : Atom -> JsObject
  // Wrap an A4 word as a JS callback function.
  Type.addOp("Any", makeOp("js-callback", ["Atom"], ["JsObject"], (cont) => {
    const [wordItem, ...rest] = cont.dataStack;
    const wordName = wordItem[1] as string;
    const capturedCont = { ...cont, dataStack: [] as StackItem[] };
    const callback = (...jsArgs: unknown[]): unknown => {
      let c = capturedCont;
      for (const arg of jsArgs) {
        c = withStack(c, [jsToStackItem(arg), ...c.dataStack]);
      }
      const token = makeToken(wordName, 0, 0, "js-callback");
      c = interpretToken(token, c);
      return c.dataStack.length > 0 ? stackItemToJs(c.dataStack[0]) : undefined;
    };
    return withStack(cont, [["JsObject", callback], ...rest]);
  }));

  // js-typeof : Any -> String
  // Get the JavaScript typeof for any value.
  Type.addOp("Any", makeOp("js-typeof", ["Any"], ["String"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const jsVal = stackItemToJs(item);
    return withStack(cont, [["String", typeof jsVal], ...rest]);
  }));

  // js-log : Any ->
  // console.log a value (useful for debugging).
  Type.addOp("Any", makeOp("js-log", ["Any"], [], (cont) => {
    const [item, ...rest] = cont.dataStack;
    console.log(stackItemToJs(item));
    return withStack(cont, rest);
  }));
}
