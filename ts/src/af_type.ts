// AF_Type — the central type registry.
// Types own their operation dictionaries; dispatch is by operation name + input stack signature.

import { Operation, SigEntry, isTypeVariable, isValueConstraint } from "./operation.js";
import { StackItem, itemType } from "./stack_item.js";

export type Handler = (tokenValue: string, cont: import("./continuation.js").Continuation) =>
  import("./continuation.js").Continuation;

export interface TypeDef {
  readonly name: string;
  ops: Map<string, Operation[]>;
  handler?: Handler;
}

// The global type registry (replaces Erlang's ETS table)
const registry = new Map<string, TypeDef>();

export function init(): void {
  registry.clear();
  // Any is always registered
  registerType({ name: "Any", ops: new Map() });
  registerType({ name: "Atom", ops: new Map() });
}

export function registerType(typeDef: TypeDef): void {
  registry.set(typeDef.name, typeDef);
}

export function getType(name: string): TypeDef | undefined {
  return registry.get(name);
}

export function allTypes(): TypeDef[] {
  return Array.from(registry.values());
}

export function addOp(typeName: string, op: Operation): void {
  const typeDef = registry.get(typeName);
  if (!typeDef) return; // silently ignore if type doesn't exist (matches Erlang)
  const existing = typeDef.ops.get(op.name) ?? [];
  typeDef.ops.set(op.name, [...existing, op]);
}

export function replaceOps(typeName: string, opName: string, ops: Operation[]): void {
  const typeDef = registry.get(typeName);
  if (!typeDef) return;
  typeDef.ops.set(opName, ops);
}

// --- Dispatch: find an operation matching token + stack ---

export function findOpInTos(tokenName: string, stack: StackItem[]): Operation | undefined {
  if (stack.length === 0) return undefined;
  const tosType = itemType(stack[0]);
  return findOpByName(tosType, tokenName, stack);
}

export function findOpInAny(tokenName: string, stack: StackItem[]): Operation | undefined {
  return findOpByName("Any", tokenName, stack);
}

function findOpByName(typeName: string, opName: string, stack: StackItem[]): Operation | undefined {
  const typeDef = registry.get(typeName);
  if (!typeDef) return undefined;
  const ops = typeDef.ops.get(opName);
  if (!ops) return undefined;
  for (const op of ops) {
    if (matchSig(op.sigIn, stack)) return op;
  }
  return undefined;
}

// --- Signature matching ---
// Match a signature (TOS-first) against the stack.
// Handles type names, type variables, value constraints, and 'Any'.

export function matchSig(sig: SigEntry[], stack: StackItem[]): boolean {
  if (sig.length === 0) return true;
  if (sig.length > stack.length) return false;

  for (let i = 0; i < sig.length; i++) {
    const entry = sig[i];
    const item = stack[i];

    if (isValueConstraint(entry)) {
      // Value constraint: must match both type and value
      if (itemType(item) !== entry[0] || item[1] !== entry[1]) return false;
    } else if (entry === "Any" || entry === "_") {
      // Wildcard: matches anything
      continue;
    } else if (isTypeVariable(entry)) {
      // Type variable (_a, _b): matches anything (binding tracked elsewhere)
      continue;
    } else {
      // Concrete type name: must match
      if (itemType(item) !== entry) return false;
    }
  }
  return true;
}

// Get the handler for the TOS type
export function getTosHandler(stack: StackItem[]): Handler | undefined {
  if (stack.length === 0) return undefined;
  const tosType = itemType(stack[0]);
  const typeDef = registry.get(tosType);
  return typeDef?.handler;
}
