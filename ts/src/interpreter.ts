// Interpreter — the outer interpreter implementing the 4-step dispatch.
//
// For each token:
//   1. Search TOS type's dictionary for matching operation
//   2. Check TOS type's handler (compiler states intercept here)
//   3. Search Any (global) dictionary
//   4. Try literal handlers, then push as Atom

import { Token } from "./token.js";
import { Continuation, withStack, withToken, newContinuation } from "./continuation.js";
import { StackItem } from "./stack_item.js";
import * as Type from "./af_type.js";

export function interpretTokens(tokens: Token[], cont: Continuation): Continuation {
  let c = cont;
  for (const token of tokens) {
    c = interpretToken(token, c);
  }
  return c;
}

export function interpretToken(token: Token, cont: Continuation): Continuation {
  const stack = cont.dataStack;
  const cont1 = withToken(cont, token);
  const value = token.value;

  // Step 1: Search TOS type's dictionary
  const tosOp = Type.findOpInTos(value, stack);
  if (tosOp) {
    debugTrace(cont.debug, value, stack, `TOS.${tosOp.name}`);
    return tosOp.impl(cont1);
  }

  // Step 2: Check TOS type's handler
  const handler = Type.getTosHandler(stack);
  if (handler) {
    debugTrace(cont.debug, value, stack, "handler");
    return handler(value, cont1);
  }

  // Step 3: Search Any (global) dictionary
  const anyOp = Type.findOpInAny(value, stack);
  if (anyOp) {
    debugTrace(cont.debug, value, stack, `Any.${anyOp.name}`);
    return anyOp.impl(cont1);
  }

  // Step 4a: Quoted strings become String values directly
  if (token.quoted) {
    const item: StackItem = ["String", value];
    debugTrace(cont.debug, value, stack, `literal->String`);
    return withStack(cont1, [item, ...stack]);
  }

  // Step 4b: Try literal handlers (Int, Float, Bool auto-detection)
  const literal = tryLiterals(value);
  if (literal) {
    debugTrace(cont.debug, value, stack, `literal->${literal[0]}`);
    return withStack(cont1, [literal, ...stack]);
  }

  // Step 5: Push as Atom
  debugTrace(cont.debug, value, stack, "->Atom");
  return withStack(cont1, [["Atom", value], ...stack]);
}

// Try each type's "literal" operation to auto-detect typed values
function tryLiterals(tokenValue: string): StackItem | undefined {
  for (const typeDef of Type.allTypes()) {
    const literalOps = typeDef.ops.get("literal");
    if (!literalOps || literalOps.length === 0) continue;

    for (const op of literalOps) {
      try {
        const tempCont: Continuation = {
          dataStack: [["Atom", tokenValue]],
          returnStack: [],
          currentToken: undefined,
          debug: false,
          wordTrace: [],
        };
        const result = op.impl(tempCont);
        if (result.dataStack.length > 0) {
          return result.dataStack[0];
        }
      } catch {
        // literal handler failed, try next
      }
    }
  }
  return undefined;
}

function debugTrace(debug: boolean, value: string, stack: StackItem[], dispatch: string): void {
  if (!debug) return;
  const stackStr = stack
    .map(([type, val]) => `${type}(${JSON.stringify(val)})`)
    .join(" ");
  console.log(`[${stackStr}] ${value} -> ${dispatch}`);
}

export { newContinuation };
