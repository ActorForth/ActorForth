// ActorForth TypeScript — main entry point.
// Initializes the type system and exports the public API.

import * as Type from "./af_type.js";
import * as AnyType from "./types/any.js";
import * as IntType from "./types/int.js";
import * as BoolType from "./types/bool.js";
import * as StringType from "./types/string.js";
import * as FloatType from "./types/float.js";
import * as ListType from "./types/list.js";
import * as MapType from "./types/map.js";
import * as ProductType from "./types/product.js";
import * as CompilerType from "./types/compiler.js";
import * as FfiType from "./types/ffi.js";
import { parse } from "./parser.js";
import { interpretTokens, interpretToken, newContinuation } from "./interpreter.js";
import { Continuation, withStack } from "./continuation.js";
import { StackItem, formatItem, formatItemTyped } from "./stack_item.js";
import { AF_Error } from "./error.js";
import { Token } from "./token.js";

export function initTypes(): void {
  Type.init();
  AnyType.init();
  IntType.init();
  BoolType.init();
  CompilerType.init();
  ProductType.init();
  StringType.init();
  MapType.init();
  ListType.init();
  FloatType.init();
  FfiType.init();
}

export function evaluate(input: string, cont?: Continuation, file?: string): Continuation {
  const c = cont ?? newContinuation();
  const tokens = parse(input, file ?? "eval");
  return interpretTokens(tokens, c);
}

export {
  parse,
  interpretTokens,
  interpretToken,
  newContinuation,
  Type,
  Continuation,
  StackItem,
  Token,
  AF_Error,
  formatItem,
  formatItemTyped,
  withStack,
};
