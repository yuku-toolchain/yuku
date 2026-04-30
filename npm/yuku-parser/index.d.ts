/** How the source code should be parsed. */
type SourceType = "script" | "module";

/** Language variant of the source code. */
type SourceLang = "js" | "ts" | "jsx" | "tsx" | "dts";

/** Options for configuring the parser. */
interface ParseOptions {
  /**
   * Parse as a classic script or an ES module.
   * Module mode enables `import`/`export`, `import.meta`, top-level `await`,
   * and strict mode.
   * @default "module"
   */
  sourceType?: SourceType;
  /**
   * Language variant controls which syntax extensions are enabled.
   * @default "js"
   */
  lang?: SourceLang;
  /**
   * When true, parenthesized expressions are represented as
   * `ParenthesizedExpression` nodes in the AST. When false,
   * parentheses are stripped and only the inner expression is kept.
   * @default true
   */
  preserveParens?: boolean;
  /**
   * Allow `return` statements outside of functions, at the top level.
   * @default false
   */
  allowReturnOutsideFunction?: boolean;
  /**
   * Run semantic analysis after parsing and include semantic errors
   * (e.g. duplicate declarations, invalid `break`/`continue` targets)
   * alongside syntax errors. This requires a separate AST pass and may
   * affect performance slightly.
   * @default false
   */
  semanticErrors?: boolean;
}

/** A source code comment. */
interface Comment {
  type: "Line" | "Block";
  /** Comment text without the delimiters. */
  value: string;
  /** Byte offset. */
  start: number;
  /** Byte offset. */
  end: number;
}

/** A labeled source span attached to a {@link Diagnostic}. */
interface DiagnosticLabel {
  /** Byte offset. */
  start: number;
  /** Byte offset. */
  end: number;
  message: string;
}

/**
 * A diagnostic produced during parsing or semantic analysis.
 * The parser is error tolerant: an AST is always produced even when diagnostics exist.
 */
interface Diagnostic {
  severity: "error" | "warning" | "hint" | "info";
  message: string;
  /** Fix suggestion, or `null` if unavailable. */
  help: string | null;
  /** Byte offset. */
  start: number;
  /** Byte offset. */
  end: number;
  /** Additional source spans providing context. */
  labels: DiagnosticLabel[];
}

/** The result returned by the parser. */
interface ParseResult {
  /** Root ESTree/TypeScript-ESTree AST node. */
  program: Program;
  /** All comments in source order. */
  comments: Comment[];
  /** Syntax diagnostics, and semantic diagnostics when {@link ParseOptions.semanticErrors} is enabled. */
  diagnostics: Diagnostic[];
}

/**
 * Parse JS/TS source code and return an ESTree / TypeScript-ESTree compatible AST.
 */
export function parse(source: string, options?: ParseOptions): ParseResult;

// AST node types

interface BaseNode {
  start: number;
  end: number;
}

type BinaryOperator =
  | "=="
  | "!="
  | "==="
  | "!=="
  | "<"
  | "<="
  | ">"
  | ">="
  | "+"
  | "-"
  | "*"
  | "/"
  | "%"
  | "**"
  | "|"
  | "^"
  | "&"
  | "<<"
  | ">>"
  | ">>>"
  | "in"
  | "instanceof";

type LogicalOperator = "&&" | "||" | "??";

type UnaryOperator = "-" | "+" | "!" | "~" | "typeof" | "void" | "delete";

type UpdateOperator = "++" | "--";

type AssignmentOperator =
  | "="
  | "+="
  | "-="
  | "*="
  | "/="
  | "%="
  | "**="
  | "<<="
  | ">>="
  | ">>>="
  | "|="
  | "^="
  | "&="
  | "||="
  | "&&="
  | "??=";

interface Identifier extends BaseNode {
  type: "Identifier";
  name: string;
  decorators?: Decorator[];
  optional?: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
}

type IdentifierName = Identifier;

type IdentifierReference = Identifier;

type BindingIdentifier = Identifier;

type LabelIdentifier = Identifier;

interface PrivateIdentifier extends BaseNode {
  type: "PrivateIdentifier";
  name: string;
}

interface StringLiteral extends BaseNode {
  type: "Literal";
  value: string;
  raw: string;
}

interface NumericLiteral extends BaseNode {
  type: "Literal";
  value: number | null;
  raw: string;
}

interface BigIntLiteral extends BaseNode {
  type: "Literal";
  value: bigint;
  raw: string;
  bigint: string;
}

interface BooleanLiteral extends BaseNode {
  type: "Literal";
  value: boolean;
  raw: string;
}

interface NullLiteral extends BaseNode {
  type: "Literal";
  value: null;
  raw: "null";
}

interface RegExpLiteral extends BaseNode {
  type: "Literal";
  value: RegExp | null;
  raw: string;
  regex: {
    pattern: string;
    flags: string;
  };
}

type Literal =
  | StringLiteral
  | NumericLiteral
  | BigIntLiteral
  | BooleanLiteral
  | NullLiteral
  | RegExpLiteral;

interface ArrayPattern extends BaseNode {
  type: "ArrayPattern";
  elements: Array<BindingPattern | RestElement | null>;
  decorators?: Decorator[];
  optional?: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
}

interface ObjectPattern extends BaseNode {
  type: "ObjectPattern";
  properties: Array<BindingProperty | RestElement>;
  decorators?: Decorator[];
  optional?: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
}

interface AssignmentPattern extends BaseNode {
  type: "AssignmentPattern";
  left: BindingPattern;
  right: Expression;
  decorators?: Decorator[];
  optional?: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
}

interface RestElement extends BaseNode {
  type: "RestElement";
  argument: BindingPattern;
  decorators?: Decorator[];
  optional?: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
  value?: null;
}

type BindingPattern = BindingIdentifier | ArrayPattern | ObjectPattern | AssignmentPattern;

type FunctionParameter = BindingPattern | RestElement | TSParameterProperty;

interface ObjectProperty extends BaseNode {
  type: "Property";
  kind: "init" | "get" | "set";
  key: Expression;
  value: Expression;
  method: boolean;
  shorthand: boolean;
  computed: boolean;
  optional?: false;
}

interface BindingProperty extends BaseNode {
  type: "Property";
  kind: "init";
  key: Expression;
  value: BindingPattern;
  method: false;
  shorthand: boolean;
  computed: boolean;
  optional?: false;
}

type Property = ObjectProperty | BindingProperty;

interface SequenceExpression extends BaseNode {
  type: "SequenceExpression";
  expressions: Expression[];
}

interface ParenthesizedExpression extends BaseNode {
  type: "ParenthesizedExpression";
  expression: Expression;
}

interface BinaryExpression extends BaseNode {
  type: "BinaryExpression";
  left: Expression | PrivateIdentifier;
  operator: BinaryOperator;
  right: Expression;
}

interface LogicalExpression extends BaseNode {
  type: "LogicalExpression";
  left: Expression;
  operator: LogicalOperator;
  right: Expression;
}

interface ConditionalExpression extends BaseNode {
  type: "ConditionalExpression";
  test: Expression;
  consequent: Expression;
  alternate: Expression;
}

interface UnaryExpression extends BaseNode {
  type: "UnaryExpression";
  operator: UnaryOperator;
  prefix: true;
  argument: Expression;
}

interface UpdateExpression extends BaseNode {
  type: "UpdateExpression";
  operator: UpdateOperator;
  prefix: boolean;
  argument: Expression;
}

type SimpleAssignmentTarget =
  | IdentifierReference
  | MemberExpression
  | TSAsExpression
  | TSSatisfiesExpression
  | TSNonNullExpression
  | TSTypeAssertion;

type AssignmentTargetPattern = ArrayPattern | ObjectPattern;

type AssignmentTarget = SimpleAssignmentTarget | AssignmentTargetPattern;

interface AssignmentExpression extends BaseNode {
  type: "AssignmentExpression";
  operator: AssignmentOperator;
  left: AssignmentTarget;
  right: Expression;
}

interface YieldExpression extends BaseNode {
  type: "YieldExpression";
  delegate: boolean;
  argument: Expression | null;
}

interface AwaitExpression extends BaseNode {
  type: "AwaitExpression";
  argument: Expression;
}

type ArrayExpressionElement = Expression | SpreadElement | null;

type ObjectPropertyKind = ObjectProperty | SpreadElement;

type Argument = Expression | SpreadElement;

interface ArrayExpression extends BaseNode {
  type: "ArrayExpression";
  elements: ArrayExpressionElement[];
}

interface ObjectExpression extends BaseNode {
  type: "ObjectExpression";
  properties: ObjectPropertyKind[];
}

interface SpreadElement extends BaseNode {
  type: "SpreadElement";
  argument: Expression;
}

interface ComputedMemberExpression extends BaseNode {
  type: "MemberExpression";
  object: Expression | Super;
  property: Expression;
  computed: true;
  optional: boolean;
}

interface StaticMemberExpression extends BaseNode {
  type: "MemberExpression";
  object: Expression | Super;
  property: IdentifierName;
  computed: false;
  optional: boolean;
}

interface PrivateFieldExpression extends BaseNode {
  type: "MemberExpression";
  object: Expression | Super;
  property: PrivateIdentifier;
  computed: false;
  optional: boolean;
}

type MemberExpression = ComputedMemberExpression | StaticMemberExpression | PrivateFieldExpression;

interface CallExpression extends BaseNode {
  type: "CallExpression";
  callee: Expression | Super;
  arguments: Argument[];
  optional: boolean;
  typeArguments?: TSTypeParameterInstantiation | null;
}

interface ChainExpression extends BaseNode {
  type: "ChainExpression";
  expression: CallExpression | MemberExpression | TSNonNullExpression;
}

interface TaggedTemplateExpression extends BaseNode {
  type: "TaggedTemplateExpression";
  tag: Expression;
  quasi: TemplateLiteral;
  typeArguments?: TSTypeParameterInstantiation | null;
}

interface NewExpression extends BaseNode {
  type: "NewExpression";
  callee: Expression;
  arguments: Argument[];
  typeArguments?: TSTypeParameterInstantiation | null;
}

interface MetaProperty extends BaseNode {
  type: "MetaProperty";
  meta: Identifier;
  property: Identifier;
}

interface ImportExpression extends BaseNode {
  type: "ImportExpression";
  source: Expression;
  options: Expression | null;
  phase: "source" | "defer" | null;
}

interface TemplateLiteral extends BaseNode {
  type: "TemplateLiteral";
  quasis: TemplateElement[];
  expressions: Expression[];
}

interface TemplateElement extends BaseNode {
  type: "TemplateElement";
  value: {
    raw: string;
    cooked: string | null;
  };
  tail: boolean;
}

interface Super extends BaseNode {
  type: "Super";
}

interface ThisExpression extends BaseNode {
  type: "ThisExpression";
}

interface ExpressionStatement extends BaseNode {
  type: "ExpressionStatement";
  expression: Expression;
  directive?: null;
}

interface Directive extends BaseNode {
  type: "ExpressionStatement";
  expression: StringLiteral;
  directive: string;
}

interface BlockStatement extends BaseNode {
  type: "BlockStatement";
  body: Statement[];
}

interface IfStatement extends BaseNode {
  type: "IfStatement";
  test: Expression;
  consequent: Statement;
  alternate: Statement | null;
}

interface SwitchStatement extends BaseNode {
  type: "SwitchStatement";
  discriminant: Expression;
  cases: SwitchCase[];
}

interface SwitchCase extends BaseNode {
  type: "SwitchCase";
  test: Expression | null;
  consequent: Statement[];
}

type ForStatementInit = VariableDeclaration | Expression;
type ForStatementLeft = VariableDeclaration | AssignmentTarget;

interface ForStatement extends BaseNode {
  type: "ForStatement";
  init: ForStatementInit | null;
  test: Expression | null;
  update: Expression | null;
  body: Statement;
}

interface ForInStatement extends BaseNode {
  type: "ForInStatement";
  left: ForStatementLeft;
  right: Expression;
  body: Statement;
}

interface ForOfStatement extends BaseNode {
  type: "ForOfStatement";
  left: ForStatementLeft;
  right: Expression;
  body: Statement;
  await: boolean;
}

interface WhileStatement extends BaseNode {
  type: "WhileStatement";
  test: Expression;
  body: Statement;
}

interface DoWhileStatement extends BaseNode {
  type: "DoWhileStatement";
  body: Statement;
  test: Expression;
}

interface BreakStatement extends BaseNode {
  type: "BreakStatement";
  label: LabelIdentifier | null;
}

interface ContinueStatement extends BaseNode {
  type: "ContinueStatement";
  label: LabelIdentifier | null;
}

interface LabeledStatement extends BaseNode {
  type: "LabeledStatement";
  label: LabelIdentifier;
  body: Statement;
}

interface WithStatement extends BaseNode {
  type: "WithStatement";
  object: Expression;
  body: Statement;
}

interface ReturnStatement extends BaseNode {
  type: "ReturnStatement";
  argument: Expression | null;
}

interface ThrowStatement extends BaseNode {
  type: "ThrowStatement";
  argument: Expression;
}

interface TryStatement extends BaseNode {
  type: "TryStatement";
  block: BlockStatement;
  handler: CatchClause | null;
  finalizer: BlockStatement | null;
}

interface CatchClause extends BaseNode {
  type: "CatchClause";
  param: BindingPattern | null;
  body: BlockStatement;
}

interface DebuggerStatement extends BaseNode {
  type: "DebuggerStatement";
}

interface EmptyStatement extends BaseNode {
  type: "EmptyStatement";
}

interface VariableDeclaration extends BaseNode {
  type: "VariableDeclaration";
  kind: "var" | "let" | "const" | "using" | "await using";
  declarations: VariableDeclarator[];
  declare?: boolean;
}

interface VariableDeclarator extends BaseNode {
  type: "VariableDeclarator";
  id: BindingPattern;
  init: Expression | null;
  definite?: boolean;
}

interface FunctionDeclaration extends BaseNode {
  type: "FunctionDeclaration";
  id: BindingIdentifier | null;
  generator: boolean;
  async: boolean;
  params: FunctionParameter[];
  body: BlockStatement | null;
  expression: false;
  declare?: boolean;
  typeParameters?: TSTypeParameterDeclaration | null;
  returnType?: TSTypeAnnotation | null;
}

interface FunctionExpression extends BaseNode {
  type: "FunctionExpression";
  id: BindingIdentifier | null;
  generator: boolean;
  async: boolean;
  params: FunctionParameter[];
  body: BlockStatement | null;
  expression: false;
  declare?: boolean;
  typeParameters?: TSTypeParameterDeclaration | null;
  returnType?: TSTypeAnnotation | null;
}

interface TSDeclareFunction extends BaseNode {
  type: "TSDeclareFunction";
  id: BindingIdentifier | null;
  generator: boolean;
  async: boolean;
  params: FunctionParameter[];
  body: null;
  expression: false;
  declare: boolean;
  typeParameters: TSTypeParameterDeclaration | null;
  returnType: TSTypeAnnotation | null;
}

interface TSEmptyBodyFunctionExpression extends BaseNode {
  type: "TSEmptyBodyFunctionExpression";
  id: BindingIdentifier | null;
  generator: boolean;
  async: boolean;
  params: FunctionParameter[];
  body: null;
  expression: false;
  declare: boolean;
  typeParameters: TSTypeParameterDeclaration | null;
  returnType: TSTypeAnnotation | null;
}

type Function =
  | FunctionDeclaration
  | FunctionExpression
  | TSDeclareFunction
  | TSEmptyBodyFunctionExpression;

interface ArrowFunctionExpression extends BaseNode {
  type: "ArrowFunctionExpression";
  id: null;
  generator: false;
  async: boolean;
  params: FunctionParameter[];
  body: BlockStatement | Expression;
  expression: boolean;
  typeParameters?: TSTypeParameterDeclaration | null;
  returnType?: TSTypeAnnotation | null;
}

interface ClassDeclaration extends BaseNode {
  type: "ClassDeclaration";
  decorators: Decorator[];
  id: BindingIdentifier | null;
  superClass: Expression | null;
  body: ClassBody;
  typeParameters?: TSTypeParameterDeclaration | null;
  superTypeArguments?: TSTypeParameterInstantiation | null;
  implements?: TSClassImplements[];
  abstract?: boolean;
  declare?: boolean;
}

interface ClassExpression extends BaseNode {
  type: "ClassExpression";
  decorators: Decorator[];
  id: BindingIdentifier | null;
  superClass: Expression | null;
  body: ClassBody;
  typeParameters?: TSTypeParameterDeclaration | null;
  superTypeArguments?: TSTypeParameterInstantiation | null;
  implements?: TSClassImplements[];
  abstract?: boolean;
  declare?: boolean;
}

type Class = ClassDeclaration | ClassExpression;

interface ClassBody extends BaseNode {
  type: "ClassBody";
  body: ClassElement[];
}

interface MethodDefinition extends BaseNode {
  type: "MethodDefinition";
  decorators: Decorator[];
  key: Expression | PrivateIdentifier;
  value: FunctionExpression | TSEmptyBodyFunctionExpression;
  kind: "constructor" | "method" | "get" | "set";
  computed: boolean;
  static: boolean;
  override?: boolean;
  optional?: boolean;
  accessibility?: "public" | "private" | "protected" | null;
}

interface TSAbstractMethodDefinition extends BaseNode {
  type: "TSAbstractMethodDefinition";
  decorators: Decorator[];
  key: Expression | PrivateIdentifier;
  value: FunctionExpression | TSEmptyBodyFunctionExpression;
  kind: "constructor" | "method" | "get" | "set";
  computed: boolean;
  static: boolean;
  override?: boolean;
  optional?: boolean;
  accessibility?: "public" | "private" | "protected" | null;
}

interface PropertyDefinition extends BaseNode {
  type: "PropertyDefinition";
  decorators: Decorator[];
  key: Expression | PrivateIdentifier;
  value: Expression | null;
  computed: boolean;
  static: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
  declare?: boolean;
  override?: boolean;
  optional?: boolean;
  definite?: boolean;
  readonly?: boolean;
  accessibility?: "public" | "private" | "protected" | null;
}

interface TSAbstractPropertyDefinition extends BaseNode {
  type: "TSAbstractPropertyDefinition";
  decorators: Decorator[];
  key: Expression | PrivateIdentifier;
  value: Expression | null;
  computed: boolean;
  static: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
  declare?: boolean;
  override?: boolean;
  optional?: boolean;
  definite?: boolean;
  readonly?: boolean;
  accessibility?: "public" | "private" | "protected" | null;
}

interface AccessorProperty extends BaseNode {
  type: "AccessorProperty";
  decorators: Decorator[];
  key: Expression | PrivateIdentifier;
  value: Expression | null;
  computed: boolean;
  static: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
  declare?: boolean;
  override?: boolean;
  optional?: boolean;
  definite?: boolean;
  readonly?: boolean;
  accessibility?: "public" | "private" | "protected" | null;
}

interface TSAbstractAccessorProperty extends BaseNode {
  type: "TSAbstractAccessorProperty";
  decorators: Decorator[];
  key: Expression | PrivateIdentifier;
  value: Expression | null;
  computed: boolean;
  static: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
  declare?: boolean;
  override?: boolean;
  optional?: boolean;
  definite?: boolean;
  readonly?: boolean;
  accessibility?: "public" | "private" | "protected" | null;
}

interface StaticBlock extends BaseNode {
  type: "StaticBlock";
  body: Statement[];
}

interface Decorator extends BaseNode {
  type: "Decorator";
  expression: Expression;
}

type ClassElement =
  | MethodDefinition
  | TSAbstractMethodDefinition
  | PropertyDefinition
  | TSAbstractPropertyDefinition
  | AccessorProperty
  | TSAbstractAccessorProperty
  | StaticBlock
  | TSIndexSignature;

interface ImportDeclaration extends BaseNode {
  type: "ImportDeclaration";
  specifiers: Array<ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier>;
  source: StringLiteral;
  phase: "source" | "defer" | null;
  attributes: ImportAttribute[];
  importKind?: "value" | "type";
}

interface ImportSpecifier extends BaseNode {
  type: "ImportSpecifier";
  imported: IdentifierName | StringLiteral;
  local: BindingIdentifier;
  importKind?: "value" | "type";
}

interface ImportDefaultSpecifier extends BaseNode {
  type: "ImportDefaultSpecifier";
  local: BindingIdentifier;
}

interface ImportNamespaceSpecifier extends BaseNode {
  type: "ImportNamespaceSpecifier";
  local: BindingIdentifier;
}

interface ImportAttribute extends BaseNode {
  type: "ImportAttribute";
  key: IdentifierName | StringLiteral;
  value: StringLiteral;
}

interface ExportNamedDeclaration extends BaseNode {
  type: "ExportNamedDeclaration";
  declaration: Declaration | null;
  specifiers: ExportSpecifier[];
  source: StringLiteral | null;
  attributes: ImportAttribute[];
  exportKind?: "value" | "type";
}

interface ExportDefaultDeclaration extends BaseNode {
  type: "ExportDefaultDeclaration";
  declaration: Declaration | Expression;
  exportKind?: "value";
}

interface ExportAllDeclaration extends BaseNode {
  type: "ExportAllDeclaration";
  exported: IdentifierName | StringLiteral | null;
  source: StringLiteral;
  attributes: ImportAttribute[];
  exportKind?: "value" | "type";
}

interface ExportSpecifier extends BaseNode {
  type: "ExportSpecifier";
  local: IdentifierReference | IdentifierName | StringLiteral;
  exported: IdentifierName | StringLiteral;
  exportKind?: "value" | "type";
}

interface JSXElement extends BaseNode {
  type: "JSXElement";
  openingElement: JSXOpeningElement;
  children: JSXChild[];
  closingElement: JSXClosingElement | null;
}

interface JSXOpeningElement extends BaseNode {
  type: "JSXOpeningElement";
  name: JSXTagName;
  attributes: Array<JSXAttribute | JSXSpreadAttribute>;
  selfClosing: boolean;
  typeArguments?: TSTypeParameterInstantiation | null;
}

interface JSXClosingElement extends BaseNode {
  type: "JSXClosingElement";
  name: JSXTagName;
}

interface JSXFragment extends BaseNode {
  type: "JSXFragment";
  openingFragment: JSXOpeningFragment;
  children: JSXChild[];
  closingFragment: JSXClosingFragment;
}

interface JSXOpeningFragment extends BaseNode {
  type: "JSXOpeningFragment";
}

interface JSXClosingFragment extends BaseNode {
  type: "JSXClosingFragment";
}

interface JSXIdentifier extends BaseNode {
  type: "JSXIdentifier";
  name: string;
}

interface JSXNamespacedName extends BaseNode {
  type: "JSXNamespacedName";
  namespace: JSXIdentifier;
  name: JSXIdentifier;
}

interface JSXMemberExpression extends BaseNode {
  type: "JSXMemberExpression";
  object: JSXIdentifier | JSXMemberExpression;
  property: JSXIdentifier;
}

interface JSXAttribute extends BaseNode {
  type: "JSXAttribute";
  name: JSXIdentifier | JSXNamespacedName;
  value: StringLiteral | JSXExpressionContainer | JSXElement | JSXFragment | null;
}

interface JSXSpreadAttribute extends BaseNode {
  type: "JSXSpreadAttribute";
  argument: Expression;
}

interface JSXExpressionContainer extends BaseNode {
  type: "JSXExpressionContainer";
  expression: Expression | JSXEmptyExpression;
}

interface JSXEmptyExpression extends BaseNode {
  type: "JSXEmptyExpression";
}

interface JSXText extends BaseNode {
  type: "JSXText";
  value: string;
  raw: string;
}

interface JSXSpreadChild extends BaseNode {
  type: "JSXSpreadChild";
  expression: Expression;
}

type JSXTagName = JSXIdentifier | JSXNamespacedName | JSXMemberExpression;

type JSXChild = JSXText | JSXElement | JSXFragment | JSXExpressionContainer | JSXSpreadChild;

// TypeScript types

interface TSTypeAnnotation extends BaseNode {
  type: "TSTypeAnnotation";
  typeAnnotation: TSType;
}

interface TSAnyKeyword extends BaseNode {
  type: "TSAnyKeyword";
}

interface TSUnknownKeyword extends BaseNode {
  type: "TSUnknownKeyword";
}

interface TSNeverKeyword extends BaseNode {
  type: "TSNeverKeyword";
}

interface TSVoidKeyword extends BaseNode {
  type: "TSVoidKeyword";
}

interface TSNullKeyword extends BaseNode {
  type: "TSNullKeyword";
}

interface TSUndefinedKeyword extends BaseNode {
  type: "TSUndefinedKeyword";
}

interface TSStringKeyword extends BaseNode {
  type: "TSStringKeyword";
}

interface TSNumberKeyword extends BaseNode {
  type: "TSNumberKeyword";
}

interface TSBigIntKeyword extends BaseNode {
  type: "TSBigIntKeyword";
}

interface TSBooleanKeyword extends BaseNode {
  type: "TSBooleanKeyword";
}

interface TSSymbolKeyword extends BaseNode {
  type: "TSSymbolKeyword";
}

interface TSObjectKeyword extends BaseNode {
  type: "TSObjectKeyword";
}

interface TSIntrinsicKeyword extends BaseNode {
  type: "TSIntrinsicKeyword";
}

interface TSThisType extends BaseNode {
  type: "TSThisType";
}

interface TSTypeReference extends BaseNode {
  type: "TSTypeReference";
  typeName: TSTypeName;
  typeArguments: TSTypeParameterInstantiation | null;
}

interface TSQualifiedName extends BaseNode {
  type: "TSQualifiedName";
  left: TSTypeName;
  right: IdentifierName;
}

type TSTypeName = IdentifierReference | TSQualifiedName | ThisExpression;

interface TSTypeQuery extends BaseNode {
  type: "TSTypeQuery";
  exprName: IdentifierReference | TSQualifiedName | TSImportType;
  typeArguments: TSTypeParameterInstantiation | null;
}

interface TSImportType extends BaseNode {
  type: "TSImportType";
  source: StringLiteral;
  options: ObjectExpression | null;
  qualifier: IdentifierName | TSQualifiedName | null;
  typeArguments: TSTypeParameterInstantiation | null;
}

interface TSTypeParameter extends BaseNode {
  type: "TSTypeParameter";
  name: BindingIdentifier;
  constraint: TSType | null;
  default: TSType | null;
  in: boolean;
  out: boolean;
  const: boolean;
}

interface TSTypeParameterDeclaration extends BaseNode {
  type: "TSTypeParameterDeclaration";
  params: TSTypeParameter[];
}

interface TSTypeParameterInstantiation extends BaseNode {
  type: "TSTypeParameterInstantiation";
  params: TSType[];
}

interface TSLiteralType extends BaseNode {
  type: "TSLiteralType";
  literal: StringLiteral | NumericLiteral | BigIntLiteral | BooleanLiteral | TemplateLiteral | UnaryExpression;
}

interface TSTemplateLiteralType extends BaseNode {
  type: "TSTemplateLiteralType";
  quasis: TemplateElement[];
  types: TSType[];
}

interface TSArrayType extends BaseNode {
  type: "TSArrayType";
  elementType: TSType;
}

interface TSIndexedAccessType extends BaseNode {
  type: "TSIndexedAccessType";
  objectType: TSType;
  indexType: TSType;
}

interface TSTupleType extends BaseNode {
  type: "TSTupleType";
  elementTypes: TSTupleElement[];
}

interface TSNamedTupleMember extends BaseNode {
  type: "TSNamedTupleMember";
  label: IdentifierName;
  elementType: TSType;
  optional: boolean;
}

interface TSOptionalType extends BaseNode {
  type: "TSOptionalType";
  typeAnnotation: TSType;
}

interface TSRestType extends BaseNode {
  type: "TSRestType";
  typeAnnotation: TSType | TSNamedTupleMember;
}

type TSTupleElement = TSType | TSNamedTupleMember | TSOptionalType | TSRestType;

interface TSJSDocNullableType extends BaseNode {
  type: "TSJSDocNullableType";
  typeAnnotation: TSType;
  postfix: boolean;
}

interface TSJSDocNonNullableType extends BaseNode {
  type: "TSJSDocNonNullableType";
  typeAnnotation: TSType;
  postfix: boolean;
}

interface TSJSDocUnknownType extends BaseNode {
  type: "TSJSDocUnknownType";
}

interface TSUnionType extends BaseNode {
  type: "TSUnionType";
  types: TSType[];
}

interface TSIntersectionType extends BaseNode {
  type: "TSIntersectionType";
  types: TSType[];
}

interface TSConditionalType extends BaseNode {
  type: "TSConditionalType";
  checkType: TSType;
  extendsType: TSType;
  trueType: TSType;
  falseType: TSType;
}

interface TSInferType extends BaseNode {
  type: "TSInferType";
  typeParameter: TSTypeParameter;
}

interface TSTypeOperator extends BaseNode {
  type: "TSTypeOperator";
  operator: "keyof" | "unique" | "readonly";
  typeAnnotation: TSType;
}

interface TSParenthesizedType extends BaseNode {
  type: "TSParenthesizedType";
  typeAnnotation: TSType;
}

interface TSFunctionType extends BaseNode {
  type: "TSFunctionType";
  typeParameters: TSTypeParameterDeclaration | null;
  params: FunctionParameter[];
  returnType: TSTypeAnnotation | null;
}

interface TSConstructorType extends BaseNode {
  type: "TSConstructorType";
  abstract: boolean;
  typeParameters: TSTypeParameterDeclaration | null;
  params: FunctionParameter[];
  returnType: TSTypeAnnotation | null;
}

interface TSTypePredicate extends BaseNode {
  type: "TSTypePredicate";
  parameterName: IdentifierName | TSThisType;
  typeAnnotation: TSTypeAnnotation | null;
  asserts: boolean;
}

interface TSTypeLiteral extends BaseNode {
  type: "TSTypeLiteral";
  members: TSSignature[];
}

interface TSMappedType extends BaseNode {
  type: "TSMappedType";
  key: BindingIdentifier;
  constraint: TSType;
  nameType: TSType | null;
  typeAnnotation: TSType | null;
  optional: false | true | "+" | "-";
  readonly: null | true | "+" | "-";
}

interface TSPropertySignature extends BaseNode {
  type: "TSPropertySignature";
  key: Expression;
  typeAnnotation: TSTypeAnnotation | null;
  computed: boolean;
  optional: boolean;
  readonly: boolean;
  accessibility: null;
  static: false;
}

interface TSMethodSignature extends BaseNode {
  type: "TSMethodSignature";
  key: Expression;
  computed: boolean;
  optional: boolean;
  kind: "method" | "get" | "set";
  typeParameters: TSTypeParameterDeclaration | null;
  params: FunctionParameter[];
  returnType: TSTypeAnnotation | null;
  accessibility: null;
  readonly: false;
  static: false;
}

interface TSCallSignatureDeclaration extends BaseNode {
  type: "TSCallSignatureDeclaration";
  typeParameters: TSTypeParameterDeclaration | null;
  params: FunctionParameter[];
  returnType: TSTypeAnnotation | null;
}

interface TSConstructSignatureDeclaration extends BaseNode {
  type: "TSConstructSignatureDeclaration";
  typeParameters: TSTypeParameterDeclaration | null;
  params: FunctionParameter[];
  returnType: TSTypeAnnotation | null;
}

interface TSIndexSignature extends BaseNode {
  type: "TSIndexSignature";
  parameters: BindingIdentifier[];
  typeAnnotation: TSTypeAnnotation;
  readonly: boolean;
  static: boolean;
  accessibility: null;
}

type TSSignature =
  | TSPropertySignature
  | TSMethodSignature
  | TSCallSignatureDeclaration
  | TSConstructSignatureDeclaration
  | TSIndexSignature;

interface TSTypeAliasDeclaration extends BaseNode {
  type: "TSTypeAliasDeclaration";
  id: BindingIdentifier;
  typeParameters: TSTypeParameterDeclaration | null;
  typeAnnotation: TSType;
  declare: boolean;
}

interface TSInterfaceDeclaration extends BaseNode {
  type: "TSInterfaceDeclaration";
  id: BindingIdentifier;
  typeParameters: TSTypeParameterDeclaration | null;
  extends: TSInterfaceHeritage[];
  body: TSInterfaceBody;
  declare: boolean;
}

interface TSInterfaceBody extends BaseNode {
  type: "TSInterfaceBody";
  body: TSSignature[];
}

interface TSInterfaceHeritage extends BaseNode {
  type: "TSInterfaceHeritage";
  expression: Expression;
  typeArguments: TSTypeParameterInstantiation | null;
}

interface TSClassImplements extends BaseNode {
  type: "TSClassImplements";
  expression: Expression;
  typeArguments: TSTypeParameterInstantiation | null;
}

interface TSEnumDeclaration extends BaseNode {
  type: "TSEnumDeclaration";
  id: BindingIdentifier;
  body: TSEnumBody;
  const: boolean;
  declare: boolean;
}

interface TSEnumBody extends BaseNode {
  type: "TSEnumBody";
  members: TSEnumMember[];
}

interface TSEnumMember extends BaseNode {
  type: "TSEnumMember";
  id: IdentifierName | StringLiteral | TemplateLiteral;
  initializer: Expression | null;
  computed: boolean;
}

interface TSModuleDeclaration extends BaseNode {
  type: "TSModuleDeclaration";
  id: BindingIdentifier | StringLiteral | TSQualifiedName | IdentifierName;
  body?: TSModuleBlock;
  kind: "namespace" | "module" | "global";
  declare: boolean;
  global: boolean;
}

interface TSModuleBlock extends BaseNode {
  type: "TSModuleBlock";
  body: Statement[];
}

interface TSParameterProperty extends BaseNode {
  type: "TSParameterProperty";
  decorators: Decorator[];
  parameter: BindingIdentifier | AssignmentPattern;
  override: boolean;
  readonly: boolean;
  accessibility: "public" | "private" | "protected" | null;
  static: false;
}

interface TSAsExpression extends BaseNode {
  type: "TSAsExpression";
  expression: Expression;
  typeAnnotation: TSType;
}

interface TSSatisfiesExpression extends BaseNode {
  type: "TSSatisfiesExpression";
  expression: Expression;
  typeAnnotation: TSType;
}

interface TSTypeAssertion extends BaseNode {
  type: "TSTypeAssertion";
  typeAnnotation: TSType;
  expression: Expression;
}

interface TSNonNullExpression extends BaseNode {
  type: "TSNonNullExpression";
  expression: Expression;
}

interface TSInstantiationExpression extends BaseNode {
  type: "TSInstantiationExpression";
  expression: Expression;
  typeArguments: TSTypeParameterInstantiation;
}

interface TSExportAssignment extends BaseNode {
  type: "TSExportAssignment";
  expression: Expression;
}

interface TSNamespaceExportDeclaration extends BaseNode {
  type: "TSNamespaceExportDeclaration";
  id: IdentifierName;
}

interface TSImportEqualsDeclaration extends BaseNode {
  type: "TSImportEqualsDeclaration";
  id: BindingIdentifier;
  moduleReference: TSExternalModuleReference | IdentifierReference | TSQualifiedName;
  importKind: "value" | "type";
}

interface TSExternalModuleReference extends BaseNode {
  type: "TSExternalModuleReference";
  expression: StringLiteral;
}

type TSType =
  | TSAnyKeyword
  | TSUnknownKeyword
  | TSNeverKeyword
  | TSVoidKeyword
  | TSNullKeyword
  | TSUndefinedKeyword
  | TSStringKeyword
  | TSNumberKeyword
  | TSBigIntKeyword
  | TSBooleanKeyword
  | TSSymbolKeyword
  | TSObjectKeyword
  | TSIntrinsicKeyword
  | TSThisType
  | TSTypeReference
  | TSTypeQuery
  | TSImportType
  | TSLiteralType
  | TSTemplateLiteralType
  | TSArrayType
  | TSIndexedAccessType
  | TSTupleType
  | TSNamedTupleMember
  | TSJSDocNullableType
  | TSJSDocNonNullableType
  | TSJSDocUnknownType
  | TSUnionType
  | TSIntersectionType
  | TSConditionalType
  | TSInferType
  | TSTypeOperator
  | TSParenthesizedType
  | TSFunctionType
  | TSConstructorType
  | TSTypePredicate
  | TSTypeLiteral
  | TSMappedType;

interface Hashbang extends BaseNode {
  type: "Hashbang";
  value: string;
}

interface Program extends BaseNode {
  type: "Program";
  sourceType: "module" | "script";
  hashbang: Hashbang | null;
  body: Array<Statement | ModuleDeclaration | Directive>;
}

type Declaration =
  | FunctionDeclaration
  | ClassDeclaration
  | VariableDeclaration
  | TSDeclareFunction
  | TSTypeAliasDeclaration
  | TSInterfaceDeclaration
  | TSEnumDeclaration
  | TSModuleDeclaration
  | TSImportEqualsDeclaration;

type Expression =
  | IdentifierReference
  | Literal
  | ThisExpression
  | Super
  | ArrayExpression
  | ObjectExpression
  | FunctionExpression
  | ArrowFunctionExpression
  | ClassExpression
  | TaggedTemplateExpression
  | TemplateLiteral
  | MemberExpression
  | CallExpression
  | NewExpression
  | ChainExpression
  | SequenceExpression
  | ParenthesizedExpression
  | BinaryExpression
  | LogicalExpression
  | ConditionalExpression
  | UnaryExpression
  | UpdateExpression
  | AssignmentExpression
  | YieldExpression
  | AwaitExpression
  | ImportExpression
  | MetaProperty
  | TSAsExpression
  | TSSatisfiesExpression
  | TSTypeAssertion
  | TSNonNullExpression
  | TSInstantiationExpression
  | JSXElement
  | JSXFragment;

type Statement =
  | ExpressionStatement
  | BlockStatement
  | EmptyStatement
  | DebuggerStatement
  | ReturnStatement
  | LabeledStatement
  | BreakStatement
  | ContinueStatement
  | IfStatement
  | SwitchStatement
  | ThrowStatement
  | TryStatement
  | WhileStatement
  | DoWhileStatement
  | ForStatement
  | ForInStatement
  | ForOfStatement
  | WithStatement
  | Declaration;

type ModuleDeclaration =
  | ImportDeclaration
  | ExportNamedDeclaration
  | ExportDefaultDeclaration
  | ExportAllDeclaration
  | TSExportAssignment
  | TSNamespaceExportDeclaration;

type Node =
  | Program
  | Hashbang
  | Statement
  | Expression
  | ModuleDeclaration
  | Directive
  | ObjectProperty
  | BindingProperty
  | PrivateIdentifier
  | TemplateElement
  | VariableDeclarator
  | CatchClause
  | SwitchCase
  | RestElement
  | ArrayPattern
  | ObjectPattern
  | AssignmentPattern
  | ClassBody
  | MethodDefinition
  | TSAbstractMethodDefinition
  | PropertyDefinition
  | TSAbstractPropertyDefinition
  | AccessorProperty
  | TSAbstractAccessorProperty
  | StaticBlock
  | Decorator
  | ImportSpecifier
  | ImportDefaultSpecifier
  | ImportNamespaceSpecifier
  | ImportAttribute
  | ExportSpecifier
  | JSXOpeningElement
  | JSXClosingElement
  | JSXOpeningFragment
  | JSXClosingFragment
  | JSXIdentifier
  | JSXNamespacedName
  | JSXMemberExpression
  | JSXAttribute
  | JSXSpreadAttribute
  | JSXExpressionContainer
  | JSXEmptyExpression
  | JSXText
  | JSXSpreadChild
  | TSTypeAnnotation
  | TSType
  | TSTypeParameter
  | TSTypeParameterDeclaration
  | TSTypeParameterInstantiation
  | TSQualifiedName
  | TSPropertySignature
  | TSMethodSignature
  | TSCallSignatureDeclaration
  | TSConstructSignatureDeclaration
  | TSIndexSignature
  | TSInterfaceBody
  | TSInterfaceHeritage
  | TSClassImplements
  | TSEnumBody
  | TSEnumMember
  | TSModuleBlock
  | TSParameterProperty
  | TSExternalModuleReference
  | TSOptionalType
  | TSRestType;

export type {
  ParseOptions,
  ParseResult,
  Comment,
  Diagnostic,
  DiagnosticLabel,
  SourceType,
  SourceLang,
  BaseNode,
  Program,
  Statement,
  Expression,
  Declaration,
  ModuleDeclaration,
  Node,
  Hashbang,
  AssignmentTarget,
  SimpleAssignmentTarget,
  AssignmentTargetPattern,
  Argument,
  ArrayExpressionElement,
  ObjectPropertyKind,
  ForStatementInit,
  ForStatementLeft,
  Identifier,
  IdentifierName,
  IdentifierReference,
  BindingIdentifier,
  LabelIdentifier,
  PrivateIdentifier,
  Literal,
  StringLiteral,
  NumericLiteral,
  BigIntLiteral,
  BooleanLiteral,
  NullLiteral,
  RegExpLiteral,
  BindingPattern,
  FunctionParameter,
  Property,
  ObjectProperty,
  BindingProperty,
  ArrayPattern,
  ObjectPattern,
  AssignmentPattern,
  RestElement,
  SequenceExpression,
  ParenthesizedExpression,
  BinaryExpression,
  LogicalExpression,
  ConditionalExpression,
  UnaryExpression,
  UpdateExpression,
  AssignmentExpression,
  YieldExpression,
  AwaitExpression,
  ArrayExpression,
  ObjectExpression,
  SpreadElement,
  MemberExpression,
  ComputedMemberExpression,
  StaticMemberExpression,
  PrivateFieldExpression,
  CallExpression,
  ChainExpression,
  TaggedTemplateExpression,
  NewExpression,
  MetaProperty,
  ImportExpression,
  TemplateLiteral,
  TemplateElement,
  Super,
  ThisExpression,
  Directive,
  ExpressionStatement,
  BlockStatement,
  IfStatement,
  SwitchStatement,
  SwitchCase,
  ForStatement,
  ForInStatement,
  ForOfStatement,
  WhileStatement,
  DoWhileStatement,
  BreakStatement,
  ContinueStatement,
  LabeledStatement,
  WithStatement,
  ReturnStatement,
  ThrowStatement,
  TryStatement,
  CatchClause,
  DebuggerStatement,
  EmptyStatement,
  VariableDeclaration,
  VariableDeclarator,
  Function,
  FunctionDeclaration,
  FunctionExpression,
  TSDeclareFunction,
  TSEmptyBodyFunctionExpression,
  ArrowFunctionExpression,
  Class,
  ClassDeclaration,
  ClassExpression,
  ClassBody,
  MethodDefinition,
  TSAbstractMethodDefinition,
  PropertyDefinition,
  TSAbstractPropertyDefinition,
  AccessorProperty,
  TSAbstractAccessorProperty,
  StaticBlock,
  Decorator,
  ClassElement,
  ImportDeclaration,
  ImportSpecifier,
  ImportDefaultSpecifier,
  ImportNamespaceSpecifier,
  ImportAttribute,
  ExportNamedDeclaration,
  ExportDefaultDeclaration,
  ExportAllDeclaration,
  ExportSpecifier,
  JSXElement,
  JSXOpeningElement,
  JSXClosingElement,
  JSXFragment,
  JSXOpeningFragment,
  JSXClosingFragment,
  JSXIdentifier,
  JSXNamespacedName,
  JSXMemberExpression,
  JSXAttribute,
  JSXSpreadAttribute,
  JSXExpressionContainer,
  JSXEmptyExpression,
  JSXText,
  JSXSpreadChild,
  JSXTagName,
  JSXChild,
  TSType,
  TSTypeName,
  TSSignature,
  TSTupleElement,
  TSTypeAnnotation,
  TSAnyKeyword,
  TSUnknownKeyword,
  TSNeverKeyword,
  TSVoidKeyword,
  TSNullKeyword,
  TSUndefinedKeyword,
  TSStringKeyword,
  TSNumberKeyword,
  TSBigIntKeyword,
  TSBooleanKeyword,
  TSSymbolKeyword,
  TSObjectKeyword,
  TSIntrinsicKeyword,
  TSThisType,
  TSTypeReference,
  TSQualifiedName,
  TSTypeQuery,
  TSImportType,
  TSTypeParameter,
  TSTypeParameterDeclaration,
  TSTypeParameterInstantiation,
  TSLiteralType,
  TSTemplateLiteralType,
  TSArrayType,
  TSIndexedAccessType,
  TSTupleType,
  TSNamedTupleMember,
  TSOptionalType,
  TSRestType,
  TSJSDocNullableType,
  TSJSDocNonNullableType,
  TSJSDocUnknownType,
  TSUnionType,
  TSIntersectionType,
  TSConditionalType,
  TSInferType,
  TSTypeOperator,
  TSParenthesizedType,
  TSFunctionType,
  TSConstructorType,
  TSTypePredicate,
  TSTypeLiteral,
  TSMappedType,
  TSPropertySignature,
  TSMethodSignature,
  TSCallSignatureDeclaration,
  TSConstructSignatureDeclaration,
  TSIndexSignature,
  TSTypeAliasDeclaration,
  TSInterfaceDeclaration,
  TSInterfaceBody,
  TSInterfaceHeritage,
  TSClassImplements,
  TSEnumDeclaration,
  TSEnumBody,
  TSEnumMember,
  TSModuleDeclaration,
  TSModuleBlock,
  TSParameterProperty,
  TSAsExpression,
  TSSatisfiesExpression,
  TSTypeAssertion,
  TSNonNullExpression,
  TSInstantiationExpression,
  TSExportAssignment,
  TSNamespaceExportDeclaration,
  TSImportEqualsDeclaration,
  TSExternalModuleReference,
  BinaryOperator,
  LogicalOperator,
  UnaryOperator,
  UpdateOperator,
  AssignmentOperator,
};
