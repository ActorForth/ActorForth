// Operation — a named, typed function in a type's dictionary.
// sig_in/sig_out are TOS-first (head of list = top of stack).

import { Continuation } from "./continuation.js";

// Type constraints in signatures:
//   "Int"             — matches type Int
//   "Any"             — matches any type
//   "_" | "_a" | "_b" — type variables (match any, positional binding)
//   ["Int", 0]        — value constraint: matches only Int(0)
export type SigEntry = string | [string, unknown];

export type OpImpl = (cont: Continuation) => Continuation;

export interface Operation {
  readonly name: string;
  readonly sigIn: SigEntry[];
  readonly sigOut: SigEntry[];
  readonly impl: OpImpl;
  readonly source?: OperationSource;
}

export type OperationSource =
  | { kind: "builtin" }
  | { kind: "compiled"; body: Operation[] }
  | { kind: "auto" }
  | { kind: "native"; module: string };

export function makeOp(
  name: string,
  sigIn: SigEntry[],
  sigOut: SigEntry[],
  impl: OpImpl,
  source?: OperationSource,
): Operation {
  return { name, sigIn, sigOut, impl, source };
}

// Check if a sig entry is a type variable (_a, _b, _c, etc.)
export function isTypeVariable(entry: SigEntry): boolean {
  return typeof entry === "string" && entry.startsWith("_");
}

// Check if a sig entry is a value constraint tuple
export function isValueConstraint(entry: SigEntry): entry is [string, unknown] {
  return Array.isArray(entry);
}
