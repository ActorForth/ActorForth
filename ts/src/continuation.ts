// Continuation — the execution state threaded through all interpretation.
// Immutable by convention: operations return new continuations.

import { Token } from "./token.js";
import { StackItem } from "./stack_item.js";

export interface Continuation {
  readonly dataStack: StackItem[];
  readonly returnStack: unknown[];
  readonly currentToken: Token | undefined;
  readonly debug: boolean;
  readonly wordTrace: Array<[string, Token]>;
}

export function newContinuation(): Continuation {
  return {
    dataStack: [],
    returnStack: [],
    currentToken: undefined,
    debug: false,
    wordTrace: [],
  };
}

// Functional update helpers — mirror Erlang's Cont#continuation{field = val}
export function withStack(cont: Continuation, dataStack: StackItem[]): Continuation {
  return { ...cont, dataStack };
}

export function withToken(cont: Continuation, currentToken: Token): Continuation {
  return { ...cont, currentToken };
}

export function withDebug(cont: Continuation, debug: boolean): Continuation {
  return { ...cont, debug };
}

export function pushTrace(cont: Continuation, name: string, token: Token): Continuation {
  return { ...cont, wordTrace: [[name, token], ...cont.wordTrace] };
}

export function popTrace(cont: Continuation): Continuation {
  return { ...cont, wordTrace: cont.wordTrace.slice(1) };
}
