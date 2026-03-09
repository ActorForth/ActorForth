// StackItem — a typed value on the data stack.
// Every value in A4 carries its type: [TypeName, Value].
// The type name is always a string (e.g., "Int", "Bool", "String").

export type StackItem = [string, unknown];

export function itemType(item: StackItem): string { return item[0]; }
export function itemValue(item: StackItem): unknown { return item[1]; }

export function formatItem(item: StackItem): string {
  const [type, value] = item;
  if (type === "String") return `"${value}"`;
  return `${value}`;
}

export function formatItemTyped(item: StackItem): string {
  const [type, value] = item;
  if (type === "String") return `String("${value}")`;
  return `${type}(${value})`;
}
