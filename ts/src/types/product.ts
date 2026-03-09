// Product type — user-defined composite types.
// Syntax: type Point x Int y Int .
// Auto-generates constructor (point), non-destructive getters (x, y), setters (x!, y!).

import * as Type from "../af_type.js";
import { makeOp } from "../operation.js";
import { Continuation, withStack } from "../continuation.js";
import { StackItem } from "../stack_item.js";

interface ProductState {
  name?: string;
  fields: Array<[string, string]>; // [fieldName, fieldType]
  currentField?: string;
}

export function init(): void {
  // type : -> TypeDefinition
  Type.addOp("Any", makeOp("type", [], ["TypeDefinition"], (cont) => {
    const state: ProductState = { fields: [] };
    return withStack(cont, [["TypeDefinition", state], ...cont.dataStack]);
  }));

  Type.registerType({ name: "TypeDefinition", ops: new Map(), handler: handleTypeDefinition });
  Type.registerType({ name: "TypeFieldName", ops: new Map(), handler: handleFieldName });
  Type.registerType({ name: "TypeFieldType", ops: new Map(), handler: handleFieldType });

  // . finishes product type definition (registered in all field-state types)
  for (const stateName of ["TypeFieldName", "TypeFieldType"]) {
    Type.addOp(stateName, makeOp(".", [stateName], [], (cont) => {
      const [item, ...rest] = cont.dataStack;
      const state = item[1] as ProductState;
      registerProductType(state.name!, state.fields);
      return withStack(cont, rest);
    }));
  }
}

function handleTypeDefinition(tokenValue: string, cont: Continuation): Continuation {
  const [item, ...rest] = cont.dataStack;
  const state = item[1] as ProductState;
  return withStack(cont, [["TypeFieldName", { ...state, name: tokenValue }], ...rest]);
}

function handleFieldName(tokenValue: string, cont: Continuation): Continuation {
  const [item, ...rest] = cont.dataStack;
  const state = item[1] as ProductState;
  return withStack(cont, [["TypeFieldType", { ...state, currentField: tokenValue }], ...rest]);
}

function handleFieldType(tokenValue: string, cont: Continuation): Continuation {
  const [item, ...rest] = cont.dataStack;
  const state = item[1] as ProductState;
  const newFields: Array<[string, string]> = [...state.fields, [state.currentField!, tokenValue]];
  return withStack(cont, [
    ["TypeFieldName", { ...state, fields: newFields, currentField: undefined }],
    ...rest,
  ]);
}

function registerProductType(typeName: string, fields: Array<[string, string]>): void {
  // Register the type
  Type.registerType({ name: typeName, ops: new Map() });

  // Constructor: lowercase type name, registered in Any
  const constructorName = typeName.toLowerCase();
  const sigIn = fields.map(([, fType]) => fType as string).reverse(); // TOS-first (last field on top)
  Type.addOp("Any", makeOp(constructorName, sigIn, [typeName], (cont) => {
    const numFields = fields.length;
    const values = cont.dataStack.slice(0, numFields);
    const rest = cont.dataStack.slice(numFields);

    // Values are in stack order (TOS first), fields are in definition order
    const reversedValues = [...values].reverse();
    const fieldMap = new Map<string, StackItem>();
    fields.forEach(([fName], i) => {
      fieldMap.set(fName, reversedValues[i]);
    });

    const instance: StackItem = [typeName, fieldMap];
    return withStack(cont, [instance, ...rest]);
  }, { kind: "auto" }));

  // Non-destructive getters: push value AND keep instance on stack
  for (const [fName, fType] of fields) {
    Type.addOp(typeName, makeOp(fName, [typeName], [fType, typeName], (cont) => {
      const [instance, ...rest] = cont.dataStack;
      const fieldMap = instance[1] as Map<string, StackItem>;
      const value = fieldMap.get(fName)!;
      return withStack(cont, [value, instance, ...rest]);
    }, { kind: "auto" }));
  }

  // Setters: x! takes value and instance, returns updated instance
  // TOS is the new value (Any type), register in Any dict
  for (const [fName] of fields) {
    Type.addOp("Any", makeOp(fName + "!", ["Any", typeName], [typeName], (cont) => {
      const [newValue, instance, ...rest] = cont.dataStack;
      const fieldMap = new Map(instance[1] as Map<string, StackItem>);
      fieldMap.set(fName, newValue);
      return withStack(cont, [[typeName, fieldMap], ...rest]);
    }, { kind: "auto" }));
  }
}
