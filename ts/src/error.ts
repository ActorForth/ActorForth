// AF_Error — structured error with location, stack snapshot, and word trace.

import { Token } from "./token.js";
import { StackItem, formatItemTyped } from "./stack_item.js";
import { Continuation } from "./continuation.js";

export class AF_Error extends Error {
  readonly type: string;
  readonly token: Token | undefined;
  readonly dataStack: StackItem[];
  readonly wordTrace: Array<[string, Token]>;

  constructor(type: string, message: string, cont: Continuation) {
    super(message);
    this.name = "AF_Error";
    this.type = type;
    this.token = cont.currentToken;
    this.dataStack = cont.dataStack.slice(0, 5); // snapshot first 5
    this.wordTrace = cont.wordTrace;
  }

  format(): string {
    const parts: string[] = [];

    parts.push(`Error: ${this.type}`);
    parts.push(`  ${this.message}`);

    if (this.token) {
      parts.push(`  at ${this.token.file}:${this.token.line}:${this.token.column} token="${this.token.value}"`);
    }

    if (this.dataStack.length > 0) {
      parts.push("  Stack:");
      this.dataStack.forEach((item, i) => {
        parts.push(`    ${i}) ${formatItemTyped(item)}`);
      });
    }

    if (this.wordTrace.length > 0) {
      parts.push("  Word trace:");
      this.wordTrace.forEach(([name, tok]) => {
        parts.push(`    in ${name} at ${tok.file}:${tok.line}`);
      });
    }

    return parts.join("\n");
  }
}

export function raise(type: string, message: string, cont: Continuation): never {
  throw new AF_Error(type, message, cont);
}
