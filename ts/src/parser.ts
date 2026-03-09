// Parser — pure tokenizer for A4 source code.
// parse(input, filename) → Token[]
//
// Handles: whitespace, # comments, "..." strings, : ; . as self-delimiting,
// . inside float literals (3.14), and accumulation of all other chars.

import { Token, makeToken } from "./token.js";

export function parse(input: string, file = "eval"): Token[] {
  const tokens: Token[] = [];
  let pos = 0;
  let line = 1;
  let col = 1;
  let current = "";
  let tokenLine = line;
  let tokenCol = col;

  function emit() {
    if (current.length > 0) {
      tokens.push(makeToken(current, tokenLine, tokenCol, file, false));
      current = "";
    }
  }

  function peek(): string {
    return pos < input.length ? input[pos] : "";
  }

  function advance(): string {
    const ch = input[pos++];
    if (ch === "\n") { line++; col = 1; }
    else { col++; }
    return ch;
  }

  while (pos < input.length) {
    const ch = input[pos];

    // Whitespace: emit current token
    if (ch === " " || ch === "\t" || ch === "\n" || ch === "\r") {
      emit();
      advance();
      continue;
    }

    // Comment: skip to end of line
    if (ch === "#") {
      emit();
      while (pos < input.length && input[pos] !== "\n") advance();
      continue;
    }

    // String literal
    if (ch === '"') {
      emit();
      const startLine = line;
      const startCol = col;
      advance(); // skip opening quote
      let str = "";
      while (pos < input.length && input[pos] !== '"') {
        str += advance();
      }
      if (pos < input.length) advance(); // skip closing quote
      tokens.push(makeToken(str, startLine, startCol, file, true));
      continue;
    }

    // Self-delimiting: : and ;
    if (ch === ":" || ch === ";") {
      emit();
      tokens.push(makeToken(ch, line, col, file, false));
      advance();
      continue;
    }

    // Dot: self-delimiting UNLESS it's a float (digits.digits)
    if (ch === ".") {
      if (isFloatDot(current, pos + 1, input)) {
        // Part of a float literal — accumulate
        current += advance();
        continue;
      }
      emit();
      tokens.push(makeToken(".", line, col, file, false));
      advance();
      continue;
    }

    // Arrow: -> as single token
    if (ch === "-" && pos + 1 < input.length && input[pos + 1] === ">") {
      emit();
      tokens.push(makeToken("->", line, col, file, false));
      advance();
      advance();
      continue;
    }

    // Regular character: accumulate
    if (current.length === 0) {
      tokenLine = line;
      tokenCol = col;
    }
    current += advance();
  }

  emit();
  return tokens;
}

// Check if a dot at this position is part of a float literal.
// True if: current token is all digits (possibly leading -), and next char is a digit.
function isFloatDot(current: string, nextPos: number, input: string): boolean {
  if (current.length === 0) return false;
  if (nextPos >= input.length) return false;

  const nextCh = input[nextPos];
  if (nextCh < "0" || nextCh > "9") return false;

  // Check current is all digits (possibly with leading -)
  const start = current[0] === "-" ? 1 : 0;
  if (start >= current.length) return false;
  for (let i = start; i < current.length; i++) {
    if (current[i] < "0" || current[i] > "9") return false;
  }
  return true;
}
