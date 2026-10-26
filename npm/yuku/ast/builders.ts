import type { Node, NodeOfType, StandardNodeType } from "../types";
import { NODE_TYPES } from "./generated";

type RequiredKeys<T> = { [K in keyof T]-?: {} extends Pick<T, K> ? never : K }[keyof T];

type Empty<V> = [V] extends [true]
  ? true
  : [V] extends [boolean]
    ? false
    : null extends V
      ? null
      : [V] extends [readonly unknown[]]
        ? []
        : never;

type VariantDefaults<T> = T extends unknown
  ? {
      [K in Exclude<RequiredKeys<T>, "type" | "start" | "end"> as [Empty<T[K]>] extends [never]
        ? never
        : K]: Empty<T[K]>;
    }
  : never;

type Intersection<U> = (U extends unknown ? (value: U) => void : never) extends (
  value: infer I,
) => void
  ? I
  : never;

type IsUnion<T> = [T] extends [Intersection<T>] ? false : true;

type Defaults<T> = {
  [K in keyof VariantDefaults<T> as IsUnion<VariantDefaults<T>[K]> extends true
    ? never
    : K]: VariantDefaults<T>[K];
};

type Defaulted<K extends StandardNodeType> = keyof Defaults<NodeOfType<K>>;

const DEFAULTS: Partial<Record<string, object>> = {
  Program: { body: [], hashbang: null },
  BlockStatement: { body: [] },
  ReturnStatement: { argument: null },
  BreakStatement: { label: null },
  ContinueStatement: { label: null },
  IfStatement: { alternate: null },
  SwitchStatement: { cases: [] },
  TryStatement: { finalizer: null, handler: null },
  ForStatement: { init: null, test: null, update: null },
  ForOfStatement: { await: false },
  FunctionDeclaration: {
    async: false,
    body: null,
    expression: false,
    generator: false,
    id: null,
    params: [],
  },
  ClassDeclaration: { decorators: [], id: null, superClass: null },
  VariableDeclaration: { declarations: [] },
  TSDeclareFunction: {
    async: false,
    body: null,
    declare: false,
    expression: false,
    generator: false,
    id: null,
    params: [],
    returnType: null,
    typeParameters: null,
  },
  TSTypeAliasDeclaration: { declare: false, typeParameters: null },
  TSInterfaceDeclaration: { declare: false, extends: [], typeParameters: null },
  TSEnumDeclaration: { const: false, declare: false },
  TSModuleDeclaration: { declare: false, global: false },
  ArrayExpression: { elements: [] },
  ObjectExpression: { properties: [] },
  FunctionExpression: {
    async: false,
    body: null,
    expression: false,
    generator: false,
    id: null,
    params: [],
  },
  ArrowFunctionExpression: {
    async: false,
    expression: false,
    generator: false,
    id: null,
    params: [],
  },
  ClassExpression: { decorators: [], id: null, superClass: null },
  TemplateLiteral: { expressions: [], quasis: [] },
  MemberExpression: { optional: false },
  CallExpression: { arguments: [], optional: false },
  NewExpression: { arguments: [] },
  SequenceExpression: { expressions: [] },
  UnaryExpression: { prefix: true },
  UpdateExpression: { prefix: false },
  YieldExpression: { argument: null, delegate: false },
  ImportExpression: { options: null, phase: null },
  JSXElement: { children: [], closingElement: null },
  JSXFragment: { children: [] },
  ImportDeclaration: { attributes: [], phase: null, specifiers: [] },
  ExportNamedDeclaration: { attributes: [], declaration: null, source: null, specifiers: [] },
  ExportAllDeclaration: { attributes: [], exported: null },
  Property: { computed: false, method: false, shorthand: false },
  TemplateElement: { tail: false },
  VariableDeclarator: { init: null },
  CatchClause: { param: null },
  SwitchCase: { consequent: [], test: null },
  ArrayPattern: { elements: [] },
  ObjectPattern: { properties: [] },
  ClassBody: { body: [] },
  MethodDefinition: { computed: false, decorators: [], static: false },
  TSAbstractMethodDefinition: { computed: false, decorators: [], static: false },
  PropertyDefinition: { computed: false, decorators: [], static: false, value: null },
  TSAbstractPropertyDefinition: { computed: false, decorators: [], static: false, value: null },
  AccessorProperty: { computed: false, decorators: [], static: false, value: null },
  TSAbstractAccessorProperty: { computed: false, decorators: [], static: false, value: null },
  StaticBlock: { body: [] },
  TSEmptyBodyFunctionExpression: {
    async: false,
    body: null,
    declare: false,
    expression: false,
    generator: false,
    id: null,
    params: [],
    returnType: null,
    typeParameters: null,
  },
  JSXOpeningElement: { attributes: [], selfClosing: false },
  JSXAttribute: { value: null },
  TSTypeReference: { typeArguments: null },
  TSTypeQuery: { typeArguments: null },
  TSImportType: { options: null, qualifier: null, typeArguments: null },
  TSTemplateLiteralType: { quasis: [], types: [] },
  TSTupleType: { elementTypes: [] },
  TSNamedTupleMember: { optional: false },
  TSJSDocNullableType: { postfix: false },
  TSJSDocNonNullableType: { postfix: false },
  TSUnionType: { types: [] },
  TSIntersectionType: { types: [] },
  TSFunctionType: { params: [], returnType: null, typeParameters: null },
  TSConstructorType: { abstract: false, params: [], returnType: null, typeParameters: null },
  TSTypePredicate: { asserts: false, typeAnnotation: null },
  TSTypeLiteral: { members: [] },
  TSMappedType: { nameType: null, readonly: null, typeAnnotation: null },
  TSTypeParameter: { const: false, constraint: null, default: null, in: false, out: false },
  TSTypeParameterDeclaration: { params: [] },
  TSTypeParameterInstantiation: { params: [] },
  TSPropertySignature: {
    accessibility: null,
    computed: false,
    optional: false,
    readonly: false,
    static: false,
    typeAnnotation: null,
  },
  TSMethodSignature: {
    accessibility: null,
    computed: false,
    optional: false,
    params: [],
    readonly: false,
    returnType: null,
    static: false,
    typeParameters: null,
  },
  TSCallSignatureDeclaration: { params: [], returnType: null, typeParameters: null },
  TSConstructSignatureDeclaration: { params: [], returnType: null, typeParameters: null },
  TSIndexSignature: { accessibility: null, parameters: [], readonly: false, static: false },
  TSInterfaceBody: { body: [] },
  TSInterfaceHeritage: { typeArguments: null },
  TSClassImplements: { typeArguments: null },
  TSEnumBody: { members: [] },
  TSEnumMember: { computed: false, initializer: null },
  TSModuleBlock: { body: [] },
  TSParameterProperty: {
    accessibility: null,
    decorators: [],
    override: false,
    readonly: false,
    static: false,
  },
} satisfies {
  [K in StandardNodeType as [Defaulted<K>] extends [never] ? never : K]: Defaults<NodeOfType<K>>;
};

// distributes over unions, so b.Literal accepts each literal shape
type BuilderInput<T, D extends PropertyKey> = T extends Node
  ? Omit<T, "type" | "start" | "end" | D> &
      Partial<Pick<T, D & keyof T>> & { start?: number; end?: number }
  : never;

type Builders = {
  [K in StandardNodeType]: (fields: BuilderInput<NodeOfType<K>, Defaulted<K>>) => NodeOfType<K>;
};

/** A node constructor per `type`, defaulting each field that may be false, null, or empty. */
export const b = Object.fromEntries(
  NODE_TYPES.map((type) => {
    const defaults = Object.entries(DEFAULTS[type] ?? {});
    const build = (fields: object): object => {
      const node: Record<string, unknown> = { type, start: 0, end: 0 };
      for (const [key, value] of defaults) node[key] = Array.isArray(value) ? [] : value;
      return Object.assign(node, fields);
    };
    return [type, build];
  }),
) as Builders;
