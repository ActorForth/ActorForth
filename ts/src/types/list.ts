// List type — list operations with higher-order map/filter/reduce/each.
// Lists are arrays of StackItems: [["Int", 1], ["Int", 2], ...]

import * as Type from "../af_type.js";
import { makeOp } from "../operation.js";
import { Continuation, withStack } from "../continuation.js";
import { StackItem } from "../stack_item.js";
import { raise } from "../error.js";
import { interpretToken } from "../interpreter.js";
import { makeToken } from "../token.js";

export function init(): void {
  Type.registerType({ name: "List", ops: new Map() });

  // nil : -> List
  Type.addOp("Any", makeOp("nil", [], ["List"], (cont) => {
    return withStack(cont, [["List", []], ...cont.dataStack]);
  }));

  // cons : Any List -> List  (TOS is Any, not List — register in Any)
  Type.addOp("Any", makeOp("cons", ["Any", "List"], ["List"], (cont) => {
    const [item, list, ...rest] = cont.dataStack;
    const items = list[1] as StackItem[];
    return withStack(cont, [["List", [item, ...items]], ...rest]);
  }));

  // length : List -> Int
  Type.addOp("List", makeOp("length", ["List"], ["Int"], (cont) => {
    const [list, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", (list[1] as StackItem[]).length], ...rest]);
  }));

  // head : List -> Any
  Type.addOp("List", makeOp("head", ["List"], ["Any"], (cont) => {
    const [list, ...rest] = cont.dataStack;
    const items = list[1] as StackItem[];
    if (items.length === 0) raise("empty_list", "head of empty list", cont);
    return withStack(cont, [items[0], ...rest]);
  }));

  // tail : List -> List
  Type.addOp("List", makeOp("tail", ["List"], ["List"], (cont) => {
    const [list, ...rest] = cont.dataStack;
    const items = list[1] as StackItem[];
    if (items.length === 0) raise("empty_list", "tail of empty list", cont);
    return withStack(cont, [["List", items.slice(1)], ...rest]);
  }));

  // append : List List -> List
  Type.addOp("List", makeOp("append", ["List", "List"], ["List"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    return withStack(cont, [["List", [...(b[1] as StackItem[]), ...(a[1] as StackItem[])]], ...rest]);
  }));

  // reverse : List -> List
  Type.addOp("List", makeOp("reverse", ["List"], ["List"], (cont) => {
    const [list, ...rest] = cont.dataStack;
    return withStack(cont, [["List", [...(list[1] as StackItem[])].reverse()], ...rest]);
  }));

  // nth : Int List -> Any  (TOS is Int — register in Int)
  Type.addOp("Int", makeOp("nth", ["Int", "List"], ["Any"], (cont) => {
    const [idx, list, ...rest] = cont.dataStack;
    const items = list[1] as StackItem[];
    const i = idx[1] as number;
    if (i < 0 || i >= items.length) raise("index_error", `Index ${i} out of bounds`, cont);
    return withStack(cont, [items[i], ...rest]);
  }));

  // last : List -> Any
  Type.addOp("List", makeOp("last", ["List"], ["Any"], (cont) => {
    const [list, ...rest] = cont.dataStack;
    const items = list[1] as StackItem[];
    if (items.length === 0) raise("empty_list", "last of empty list", cont);
    return withStack(cont, [items[items.length - 1], ...rest]);
  }));

  // take : Int List -> List  (TOS is Int — register in Int)
  Type.addOp("Int", makeOp("take", ["Int", "List"], ["List"], (cont) => {
    const [n, list, ...rest] = cont.dataStack;
    const items = list[1] as StackItem[];
    return withStack(cont, [["List", items.slice(0, n[1] as number)], ...rest]);
  }));

  // drop : Int List -> List  (TOS is Int — register in Int)
  Type.addOp("Int", makeOp("drop", ["Int", "List"], ["List"], (cont) => {
    const [n, list, ...rest] = cont.dataStack;
    const items = list[1] as StackItem[];
    return withStack(cont, [["List", items.slice(n[1] as number)], ...rest]);
  }));

  // empty? : List -> Bool
  Type.addOp("List", makeOp("empty?", ["List"], ["Bool"], (cont) => {
    const [list, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", (list[1] as StackItem[]).length === 0], ...rest]);
  }));

  // contains? : Any List -> Bool  (TOS is Any — register in Any)
  Type.addOp("Any", makeOp("contains?", ["Any", "List"], ["Bool"], (cont) => {
    const [item, list, ...rest] = cont.dataStack;
    const items = list[1] as StackItem[];
    const found = items.some(i => i[0] === item[0] && i[1] === item[1]);
    return withStack(cont, [["Bool", found], ...rest]);
  }));

  // --- Higher-order operations ---
  // Registered in Atom dict so "wordname map" dispatches (Atom on TOS, List below)

  // map : Atom List -> List
  Type.addOp("Atom", makeOp("map", ["Atom", "List"], ["List"], (cont) => {
    const [wordItem, listItem, ...rest] = cont.dataStack;
    const wordName = wordItem[1] as string;
    const items = listItem[1] as StackItem[];
    const mapped = items.map(item => {
      const result = applyWord(wordName, [item], cont);
      return result.dataStack[0];
    });
    return withStack(cont, [["List", mapped], ...rest]);
  }));

  // filter : Atom List -> List
  Type.addOp("Atom", makeOp("filter", ["Atom", "List"], ["List"], (cont) => {
    const [wordItem, listItem, ...rest] = cont.dataStack;
    const wordName = wordItem[1] as string;
    const items = listItem[1] as StackItem[];
    const filtered = items.filter(item => {
      const result = applyWord(wordName, [item], cont);
      return result.dataStack[0][1] === true;
    });
    return withStack(cont, [["List", filtered], ...rest]);
  }));

  // reduce : Atom Any List -> Any  (word accumulator list)
  Type.addOp("Atom", makeOp("reduce", ["Atom", "Any", "List"], ["Any"], (cont) => {
    const [wordItem, accItem, listItem, ...rest] = cont.dataStack;
    const wordName = wordItem[1] as string;
    const items = listItem[1] as StackItem[];
    let acc = accItem;
    for (const item of items) {
      const result = applyWord(wordName, [item, acc], cont);
      acc = result.dataStack[0];
    }
    return withStack(cont, [acc, ...rest]);
  }));

  // each : Atom List ->  (apply for side effects)
  Type.addOp("Atom", makeOp("each", ["Atom", "List"], [], (cont) => {
    const [wordItem, listItem, ...rest] = cont.dataStack;
    const wordName = wordItem[1] as string;
    const items = listItem[1] as StackItem[];
    for (const item of items) {
      applyWord(wordName, [item], cont);
    }
    return withStack(cont, rest);
  }));
}

// Dispatch a word by name on a temporary stack via the interpreter
function applyWord(wordName: string, stackItems: StackItem[], cont: Continuation): Continuation {
  const tempCont: Continuation = {
    ...cont,
    dataStack: stackItems,
  };
  const token = makeToken(wordName, 0, 0, "list-op");
  return interpretToken(token, tempCont);
}
