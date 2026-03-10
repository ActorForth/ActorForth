// Word definition compiler — the : name Types -> Types ; body . syntax.
//
// NOT a separate pass: it's a state machine using compiler state types.
// States: WordDefinition → InputTypeSignature → OutputTypeSignature → CodeCompile
// Sub-clauses: CodeCompile ':'→ SubClauseInput → SubClauseOutput → CodeCompile

import * as Type from "../af_type.js";
import { makeOp, Operation, SigEntry, OpImpl } from "../operation.js";
import { Continuation, withStack, pushTrace, popTrace } from "../continuation.js";
import { StackItem } from "../stack_item.js";
import { raise } from "../error.js";
import { interpretToken } from "../interpreter.js";
import { makeToken, Token } from "../token.js";

interface CompilerState {
  name?: string;
  sigIn: SigEntry[];          // accumulated left-to-right (Forth convention)
  sigOut: SigEntry[];
  body: Operation[];
  clauses: Array<{ sigIn: SigEntry[]; sigOut: SigEntry[]; body: Operation[] }>;
  pendingValue?: unknown;
  pendingValueType?: string;
  // Sub-clause support
  masterSigIn?: SigEntry[];   // un-reversed master sig for right-aligned resolution
  masterSigOut?: SigEntry[];
  subRawIn?: Array<{ value: string; quoted: boolean }>;
  subRawOut?: Array<{ value: string; quoted: boolean }>;
  currentSub?: { subSigIn: SigEntry[]; subSigOut: SigEntry[] };
}

export function init(): void {
  // Register compiler state types
  Type.registerType({ name: "WordDefinition", ops: new Map(), handler: handleWordDefinition });
  Type.registerType({ name: "InputTypeSignature", ops: new Map(), handler: handleInputSig });
  Type.registerType({ name: "OutputTypeSignature", ops: new Map(), handler: handleOutputSig });
  Type.registerType({ name: "CodeCompile", ops: new Map(), handler: handleCodeCompile });
  Type.registerType({ name: "SubClauseInput", ops: new Map(), handler: handleSubClauseInput });
  Type.registerType({ name: "SubClauseOutput", ops: new Map(), handler: handleSubClauseOutput });

  // : starts word definition — registered in Any
  Type.addOp("Any", makeOp(":", [], ["WordDefinition"], (cont) => {
    const state: CompilerState = { sigIn: [], sigOut: [], body: [], clauses: [] };
    return withStack(cont, [["WordDefinition", state], ...cont.dataStack]);
  }));

  // -> transitions from InputTypeSignature to OutputTypeSignature
  Type.addOp("InputTypeSignature", makeOp("->", ["InputTypeSignature"], ["OutputTypeSignature"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const state = flushPending(item[1] as CompilerState);
    return withStack(cont, [["OutputTypeSignature", state], ...rest]);
  }));

  // ; transitions from OutputTypeSignature to CodeCompile
  // Sigs stay in Forth order (left-to-right) — reversal happens at registration time.
  Type.addOp("OutputTypeSignature", makeOp(";", ["OutputTypeSignature"], ["CodeCompile"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const state = flushPending(item[1] as CompilerState);
    return withStack(cont, [["CodeCompile", state], ...rest]);
  }));

  // . finishes word definition
  Type.addOp("CodeCompile", makeOp(".", ["CodeCompile"], [], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const state = item[1] as CompilerState;
    const cont1 = withStack(cont, rest);

    // Complete any active sub-clause
    let allClauses = state.clauses;
    if (state.currentSub) {
      allClauses = [
        ...allClauses,
        { sigIn: state.currentSub.subSigIn, sigOut: state.currentSub.subSigOut, body: state.body },
      ];
    }

    if (allClauses.length === 0) {
      // Single-clause word — reverse sigs at registration
      const sigIn = [...state.sigIn].reverse();
      const sigOut = [...state.sigOut].reverse();
      registerSingleWord(state.name!, sigIn, sigOut, state.body);
    } else {
      // Multi-clause: if no currentSub, add final clause from state
      if (!state.currentSub) {
        allClauses = [
          ...allClauses,
          { sigIn: state.sigIn, sigOut: state.sigOut, body: state.body },
        ];
      }
      registerMultiWord(state.name!, allClauses);
    }
    return cont1;
  }));

  // -> in SubClauseInput: resolve raw tokens right-aligned, transition to SubClauseOutput
  Type.addOp("SubClauseInput", makeOp("->", ["SubClauseInput"], ["SubClauseOutput"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const state = item[1] as CompilerState;
    const resolvedSigIn = resolveSubTokensRightAligned(state.subRawIn ?? [], state.masterSigIn ?? []);
    return withStack(cont, [["SubClauseOutput", {
      ...state,
      subRawIn: undefined,
      subRawOut: [],
      currentSub: { ...state.currentSub, subSigIn: resolvedSigIn, subSigOut: [] },
    }], ...rest]);
  }));

  // ; in SubClauseOutput: resolve output tokens, transition back to CodeCompile
  Type.addOp("SubClauseOutput", makeOp(";", ["SubClauseOutput"], ["CodeCompile"], (cont) => {
    const [item, ...rest] = cont.dataStack;
    const state = item[1] as CompilerState;
    const resolvedSigOut = resolveSubTokensRightAligned(state.subRawOut ?? [], state.masterSigOut ?? []);
    return withStack(cont, [["CodeCompile", {
      ...state,
      subRawOut: undefined,
      currentSub: { subSigIn: state.currentSub!.subSigIn, subSigOut: resolvedSigOut },
      body: [],
    }], ...rest]);
  }));
}

// --- Handlers ---

function handleWordDefinition(tokenValue: string, cont: Continuation): Continuation {
  const [item, ...rest] = cont.dataStack;
  const state = item[1] as CompilerState;
  return withStack(cont, [
    ["InputTypeSignature", { ...state, name: tokenValue }],
    ...rest,
  ]);
}

function handleInputSig(tokenValue: string, cont: Continuation): Continuation {
  const [item, ...rest] = cont.dataStack;
  const state = item[1] as CompilerState;

  // Check if this is a value for a value constraint
  const literalValue = tryParseLiteral(tokenValue);
  if (literalValue !== undefined && state.pendingValue === undefined) {
    const newState: CompilerState = {
      ...state,
      pendingValue: literalValue,
      pendingValueType: undefined,
    };
    return withStack(cont, [["InputTypeSignature", newState], ...rest]);
  }

  // If we have a pending value, this token is the type name
  if (state.pendingValue !== undefined) {
    const constraint: SigEntry = [tokenValue, state.pendingValue];
    const newState: CompilerState = {
      ...state,
      sigIn: [...state.sigIn, constraint],
      pendingValue: undefined,
    };
    return withStack(cont, [["InputTypeSignature", newState], ...rest]);
  }

  // Regular type name
  return withStack(cont, [
    ["InputTypeSignature", { ...state, sigIn: [...state.sigIn, tokenValue] }],
    ...rest,
  ]);
}

function handleOutputSig(tokenValue: string, cont: Continuation): Continuation {
  const [item, ...rest] = cont.dataStack;
  const state = item[1] as CompilerState;
  return withStack(cont, [
    ["OutputTypeSignature", { ...state, sigOut: [...state.sigOut, tokenValue] }],
    ...rest,
  ]);
}

function handleCodeCompile(tokenValue: string, cont: Continuation): Continuation {
  const [item, ...rest] = cont.dataStack;
  const state = item[1] as CompilerState;

  // : inside CodeCompile starts a sub-clause
  if (tokenValue === ":") {
    // Complete any active sub-clause
    let newClauses = state.clauses;
    if (state.currentSub) {
      newClauses = [
        ...newClauses,
        { sigIn: state.currentSub.subSigIn, sigOut: state.currentSub.subSigOut, body: state.body },
      ];
    }
    const newState: CompilerState = {
      ...state,
      clauses: newClauses,
      currentSub: undefined,
      body: [],
      subRawIn: [],
      masterSigIn: state.masterSigIn ?? state.sigIn,
      masterSigOut: state.masterSigOut ?? state.sigOut,
    };
    return withStack(cont, [["SubClauseInput", newState], ...rest]);
  }

  // Each token in the body becomes a late-binding operation
  // Preserve quoted flag from original token
  const isQuoted = cont.currentToken?.quoted ?? false;
  const bodyOp = makeLateBindingOp(tokenValue, isQuoted);
  return withStack(cont, [
    ["CodeCompile", { ...state, body: [...state.body, bodyOp] }],
    ...rest,
  ]);
}

function handleSubClauseInput(tokenValue: string, cont: Continuation): Continuation {
  const [item, ...rest] = cont.dataStack;
  const state = item[1] as CompilerState;
  const isQuoted = cont.currentToken?.quoted ?? false;
  return withStack(cont, [["SubClauseInput", {
    ...state,
    subRawIn: [...(state.subRawIn ?? []), { value: tokenValue, quoted: isQuoted }],
  }], ...rest]);
}

function handleSubClauseOutput(tokenValue: string, cont: Continuation): Continuation {
  const [item, ...rest] = cont.dataStack;
  const state = item[1] as CompilerState;
  const isQuoted = cont.currentToken?.quoted ?? false;
  return withStack(cont, [["SubClauseOutput", {
    ...state,
    subRawOut: [...(state.subRawOut ?? []), { value: tokenValue, quoted: isQuoted }],
  }], ...rest]);
}

// --- Flush pending value constraint ---

function flushPending(state: CompilerState): CompilerState {
  if (state.pendingValue !== undefined) {
    return {
      ...state,
      sigIn: [...state.sigIn, String(state.pendingValue)],
      pendingValue: undefined,
    };
  }
  return state;
}

// --- Right-aligned sub-clause token resolution ---
// Match Erlang's resolve_sub_tokens_right_aligned/2.
// Raw tokens are aligned to the rightmost positions of the master sig.
// Leftmost unspecified positions are filled from the master sig.

function resolveSubTokensRightAligned(
  rawTokens: Array<{ value: string; quoted: boolean }>,
  masterSig: SigEntry[]
): SigEntry[] {
  const subLen = rawTokens.length;
  const masterLen = masterSig.length;
  const offset = masterLen - subLen;

  const resolved: SigEntry[] = rawTokens.map((raw, idx) => {
    const masterPos = offset + idx;
    if (masterPos >= 0 && masterPos < masterLen) {
      return resolveSingleSubToken(raw.value, raw.quoted, masterSig[masterPos]);
    }
    return raw.value; // fallback: treat as type name
  });

  // Pad left with master types for unspecified positions
  if (offset > 0) {
    const padding = masterSig.slice(0, offset);
    return [...padding, ...resolved];
  }
  return resolved;
}

function resolveSingleSubToken(tokenValue: string, quoted: boolean, masterType: SigEntry): SigEntry {
  // Quoted string with String master type → value constraint
  if (quoted && masterType === "String") {
    return ["String", tokenValue];
  }

  // Try to parse as literal of the master type
  const typeName = Array.isArray(masterType) ? masterType[0] : masterType;
  const literalValue = tryParseLiteral(tokenValue);
  if (literalValue !== undefined) {
    return [typeName, literalValue];
  }

  // Otherwise treat as type name
  return tokenValue;
}

// --- Late binding: each body token re-dispatches at runtime ---

function makeLateBindingOp(tokenValue: string, quoted: boolean): Operation {
  return makeOp(tokenValue, [], [], (cont) => {
    const token = makeToken(tokenValue, 0, 0, "compiled-word", quoted);
    return interpretToken(token, cont);
  });
}

// --- Word registration ---

function registerSingleWord(name: string, sigIn: SigEntry[], sigOut: SigEntry[], body: Operation[]): void {
  const targetType = getTargetType(sigIn);
  const impl = makeCompiledImpl(body, name);
  const op = makeOp(name, sigIn, sigOut, impl, { kind: "compiled", body });
  Type.addOp(targetType, op);
}

function registerMultiWord(
  name: string,
  clauses: Array<{ sigIn: SigEntry[]; sigOut: SigEntry[]; body: Operation[] }>
): void {
  // Reverse sigs (Forth order → TOS-first) and register each clause.
  // Value-constrained clauses first for more specific matching.
  const prepared = clauses.map(c => ({
    sigIn: [...c.sigIn].reverse(),
    sigOut: [...c.sigOut].reverse(),
    body: c.body,
  }));

  const sorted = [...prepared].sort((a, b) => {
    const aHasValue = a.sigIn.some(s => Array.isArray(s));
    const bHasValue = b.sigIn.some(s => Array.isArray(s));
    if (aHasValue && !bHasValue) return -1;
    if (!aHasValue && bHasValue) return 1;
    return 0;
  });

  for (const clause of sorted) {
    const targetType = getTargetType(clause.sigIn);
    const impl = makeCompiledImpl(clause.body, name);
    const op = makeOp(name, clause.sigIn, clause.sigOut, impl, { kind: "compiled", body: clause.body });
    Type.addOp(targetType, op);
  }
}

function getTargetType(sigIn: SigEntry[]): string {
  if (sigIn.length === 0) return "Any";
  const first = sigIn[0]; // TOS-first
  if (Array.isArray(first)) return first[0]; // value constraint: use type
  if (typeof first === "string" && !first.startsWith("_") && first !== "Any") return first;
  return "Any";
}

function makeCompiledImpl(body: Operation[], wordName: string): OpImpl {
  return (cont: Continuation): Continuation => {
    const token = cont.currentToken ?? makeToken(wordName, 0, 0, "compiled-word");
    const cont1 = pushTrace(cont, wordName, token);
    let c = cont1;
    for (const op of body) {
      c = op.impl(c);
    }
    return popTrace(c);
  };
}

// --- Literal parsing for value constraints ---

function tryParseLiteral(value: string): unknown {
  if (/^-?\d+$/.test(value)) return parseInt(value, 10);
  if (value === "true") return true;
  if (value === "false") return false;
  return undefined;
}
