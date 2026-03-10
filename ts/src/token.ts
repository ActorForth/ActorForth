// Token — the fundamental unit of A4 source code.
// Produced by the parser, consumed by the interpreter.

export interface Token {
  readonly value: string;
  readonly line: number;
  readonly column: number;
  readonly file: string;
  readonly quoted: boolean;
}

export function makeToken(
  value: string, line = 0, column = 0, file = "eval", quoted = false
): Token {
  return { value, line, column, file, quoted };
}
