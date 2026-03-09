// Map type — key-value store. Keys and values are full StackItems.
// Internal representation: Map<string, [StackItem, StackItem]> keyed by JSON of the key item.

import * as Type from "../af_type.js";
import { makeOp } from "../operation.js";
import { withStack } from "../continuation.js";
import { StackItem } from "../stack_item.js";
import { raise } from "../error.js";

// Internal map representation: JS Map with serialized key -> [keyItem, valueItem]
type AFMap = Map<string, [StackItem, StackItem]>;

function keyStr(item: StackItem): string {
  return JSON.stringify(item);
}

export function init(): void {
  Type.registerType({ name: "Map", ops: new Map() });

  // map-new : -> Map
  Type.addOp("Any", makeOp("map-new", [], ["Map"], (cont) => {
    return withStack(cont, [["Map", new Map() as AFMap], ...cont.dataStack]);
  }));

  // map-put : Any Any Map -> Map  (key on TOS, value below, map below that)
  Type.addOp("Any", makeOp("map-put", ["Any", "Any", "Map"], ["Map"], (cont) => {
    const [key, value, map, ...rest] = cont.dataStack;
    const m = new Map(map[1] as AFMap);
    m.set(keyStr(key), [key, value]);
    return withStack(cont, [["Map", m], ...rest]);
  }));

  // map-get : Any Map -> Any  (key map — TOS is Any, register in Any)
  Type.addOp("Any", makeOp("map-get", ["Any", "Map"], ["Any"], (cont) => {
    const [key, map, ...rest] = cont.dataStack;
    const m = map[1] as AFMap;
    const entry = m.get(keyStr(key));
    if (!entry) raise("key_error", `Key not found: ${JSON.stringify(key)}`, cont);
    return withStack(cont, [entry[1], ...rest]);
  }));

  // map-delete : Any Map -> Map  (TOS is Any, register in Any)
  Type.addOp("Any", makeOp("map-delete", ["Any", "Map"], ["Map"], (cont) => {
    const [key, map, ...rest] = cont.dataStack;
    const m = new Map(map[1] as AFMap);
    m.delete(keyStr(key));
    return withStack(cont, [["Map", m], ...rest]);
  }));

  // map-has? : Any Map -> Bool  (TOS is Any, register in Any)
  Type.addOp("Any", makeOp("map-has?", ["Any", "Map"], ["Bool"], (cont) => {
    const [key, map, ...rest] = cont.dataStack;
    return withStack(cont, [["Bool", (map[1] as AFMap).has(keyStr(key))], ...rest]);
  }));

  // map-keys : Map -> List
  Type.addOp("Map", makeOp("map-keys", ["Map"], ["List"], (cont) => {
    const [map, ...rest] = cont.dataStack;
    const keys = Array.from((map[1] as AFMap).values()).map(([k]) => k);
    return withStack(cont, [["List", keys], ...rest]);
  }));

  // map-values : Map -> List
  Type.addOp("Map", makeOp("map-values", ["Map"], ["List"], (cont) => {
    const [map, ...rest] = cont.dataStack;
    const values = Array.from((map[1] as AFMap).values()).map(([, v]) => v);
    return withStack(cont, [["List", values], ...rest]);
  }));

  // map-size : Map -> Int
  Type.addOp("Map", makeOp("map-size", ["Map"], ["Int"], (cont) => {
    const [map, ...rest] = cont.dataStack;
    return withStack(cont, [["Int", (map[1] as AFMap).size], ...rest]);
  }));

  // map-merge : Map Map -> Map
  Type.addOp("Map", makeOp("map-merge", ["Map", "Map"], ["Map"], (cont) => {
    const [a, b, ...rest] = cont.dataStack;
    const merged = new Map(b[1] as AFMap);
    for (const [k, v] of (a[1] as AFMap)) merged.set(k, v);
    return withStack(cont, [["Map", merged], ...rest]);
  }));
}
