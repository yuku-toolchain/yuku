import type { TokenKindMap } from "./tokens";

export type { TokenKindMap };

/** How the source is parsed. */
export type SourceType = "script" | "module" | "commonjs";

/** What `Program.sourceType` reports. `commonjs` input reports `script`. */
export type ModuleKind = "script" | "module";

/** The language of the source. */
export type SourceLang = "js" | "ts" | "jsx" | "tsx" | "dts";

/** Whether a comment is a line or a block comment. */
export type CommentType = "Line" | "Block";

/** Where an attached comment sits, `inside` meaning within an otherwise empty node. */
export type CommentPosition = "before" | "after" | "inside";

/** A comment, also in its node's {@link BaseNode.comments} with `attachComments`. */
export interface Comment {
  type: CommentType;
  /** Text without the delimiters. */
  value: string;
  /** UTF-16 offset, delimiters included. */
  start: number;
  /** UTF-16 offset. */
  end: number;
  position?: CommentPosition;
  /** Whether it shares a line with the node edge it sits by. */
  sameLine?: boolean;
}

/** One of the values of the `TokenKind` object. */
export type TokenKind = TokenKindMap[keyof TokenKindMap];

/** The tokens of a file in source order, each an index below `length`. Comments are not tokens. */
export interface TokenList {
  readonly length: number;
  /** `tokens.kind(i) === TokenKind.Arrow` */
  kind(index: number): TokenKind;
  /** Source text. A string literal keeps its quotes. */
  text(index: number): string;
  /** UTF-16 offset. */
  start(index: number): number;
  end(index: number): number;

  /** A reserved word or a contextual keyword the lexer knows. */
  isKeyword(index: number): boolean;
  /** Reserved always or in strict mode. */
  isReserved(index: number): boolean;
  /** A word that can never be an identifier. */
  isUnconditionallyReserved(index: number): boolean;
  /** Reserved in strict mode only, such as `let`. */
  isStrictModeReserved(index: number): boolean;
  /** An identifier or any keyword. */
  isIdentifierLike(index: number): boolean;
  isNumericLiteral(index: number): boolean;
  isBinaryOperator(index: number): boolean;
  isLogicalOperator(index: number): boolean;
  isUnaryOperator(index: number): boolean;
  isAssignmentOperator(index: number): boolean;
  /** Operator precedence, 0 when none. */
  precedence(index: number): number;

  /** A line terminator precedes the token. */
  newlineBefore(index: number): boolean;
  /** The text has an escape sequence. */
  escaped(index: number): boolean;
  /** A template chunk whose cooked value is undefined. */
  invalidEscape(index: number): boolean;
  /** A string with an unpaired surrogate. */
  loneSurrogate(index: number): boolean;

  /** The half-open range of the tokens inside a node, empty within a single token. */
  range(node: Span): [from: number, to: number];
  /** The last token ending at or before an offset, or -1. */
  before(offset: number): number;
  /** The first token starting at or after an offset, or -1. */
  after(offset: number): number;
  /** The token containing an offset, or -1. */
  at(offset: number): number;
}

/** A labeled span of a {@link Diagnostic}. */
export interface DiagnosticLabel {
  /** UTF-16 offset. */
  start: number;
  /** UTF-16 offset. */
  end: number;
  message: string;
}

/** Severity level of a {@link Diagnostic}. */
export type DiagnosticSeverity = "error" | "warning" | "hint" | "info";

/** A problem in the source. */
export interface Diagnostic {
  severity: DiagnosticSeverity;
  message: string;
  /** Null when the source has no path. */
  path: string | null;
  /** UTF-16 offset. */
  start: number;
  /** UTF-16 offset. */
  end: number;
  /** Related spans. */
  labels: DiagnosticLabel[];
  /** A fix suggestion. */
  help: string | null;
}

/** The `type` of every node. */
export type NodeType = Node["type"];

/** The node, or union of nodes, carrying a given `type`. */
export type NodeOfType<K extends NodeType> = Extract<Node, { type: K }>;

export interface BaseNode {
  start: number;
  end: number;
  /** Attached comments in source order, with `attachComments`. */
  comments?: Comment[];
}

export type Span = BaseNode;

export type BinaryOperator =
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

export type LogicalOperator = "&&" | "||" | "??";

export type UnaryOperator = "-" | "+" | "!" | "~" | "typeof" | "void" | "delete";

export type UpdateOperator = "++" | "--";

export type AssignmentOperator =
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

export type VariableDeclarationKind = "var" | "let" | "const" | "using" | "await using";

export type PropertyKind = "init" | "get" | "set";

export type MethodDefinitionKind = "constructor" | "method" | "get" | "set";

export type TSAccessibility = "public" | "private" | "protected";

export type TSMethodSignatureKind = "method" | "get" | "set";

export type TSTypeOperatorOperator = "keyof" | "unique" | "readonly";

export type TSModuleDeclarationKind = "module" | "namespace" | "global";

export type TSMappedTypeModifierOperator = true | "+" | "-";

export type ImportPhase = "source" | "defer";

export type ImportOrExportKind = "value" | "type";

export type FunctionType =
  | "FunctionDeclaration"
  | "FunctionExpression"
  | "TSDeclareFunction"
  | "TSEmptyBodyFunctionExpression";

export type ClassType = "ClassDeclaration" | "ClassExpression";

export type MethodDefinitionType = "MethodDefinition" | "TSAbstractMethodDefinition";

export type PropertyDefinitionType = "PropertyDefinition" | "TSAbstractPropertyDefinition";

export type AccessorPropertyType = "AccessorProperty" | "TSAbstractAccessorProperty";

/** What TypeScript adds to a binding position. */
interface PatternBase extends BaseNode {
  decorators?: Decorator[];
  optional?: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
}

export interface Identifier extends PatternBase {
  type: "Identifier";
  name: string;
}

export type IdentifierName = Identifier;
export type IdentifierReference = Identifier;
export type BindingIdentifier = Identifier;
export type LabelIdentifier = Identifier;

export interface PrivateIdentifier extends BaseNode {
  type: "PrivateIdentifier";
  name: string;
}

interface LiteralBase extends BaseNode {
  type: "Literal";
  raw?: string;
}

export interface StringLiteral extends LiteralBase {
  value: string;
}

export interface NumericLiteral extends LiteralBase {
  value: number;
}

export interface BigIntLiteral extends LiteralBase {
  value: bigint;
  bigint: string;
}

export interface BooleanLiteral extends LiteralBase {
  value: boolean;
}

export interface NullLiteral extends LiteralBase {
  value: null;
  raw?: "null";
}

export interface RegExpLiteral extends LiteralBase {
  value: RegExp | null;
  regex: {
    pattern: string;
    flags: string;
  };
}

export type Literal =
  | StringLiteral
  | NumericLiteral
  | BigIntLiteral
  | BooleanLiteral
  | NullLiteral
  | RegExpLiteral;

export interface ArrayPattern extends PatternBase {
  type: "ArrayPattern";
  elements: Array<BindingPattern | SimpleAssignmentTarget | RestElement | null>;
}

export interface ObjectPattern extends PatternBase {
  type: "ObjectPattern";
  properties: Array<BindingProperty | RestElement>;
}

export interface AssignmentPattern extends PatternBase {
  type: "AssignmentPattern";
  left: BindingPattern | SimpleAssignmentTarget;
  right: Expression;
}

export interface RestElement extends PatternBase {
  type: "RestElement";
  argument: BindingPattern | SimpleAssignmentTarget;
  value?: null;
}

export type BindingPattern = BindingIdentifier | ArrayPattern | ObjectPattern | AssignmentPattern;

export type FunctionParameter = BindingPattern | RestElement | TSParameterProperty;

interface PropertyBase extends BaseNode {
  type: "Property";
  key: PropertyKey;
  shorthand: boolean;
  computed: boolean;
  optional?: false;
}

export interface ObjectProperty extends PropertyBase {
  kind: PropertyKind;
  value: Expression;
  method: boolean;
}

export interface BindingProperty extends PropertyBase {
  kind: "init";
  value: BindingPattern | SimpleAssignmentTarget;
  method: false;
}

export type Property = ObjectProperty | BindingProperty;

export type PropertyKey = IdentifierName | PrivateIdentifier | Expression;

export interface SequenceExpression extends BaseNode {
  type: "SequenceExpression";
  expressions: Expression[];
}

export interface ParenthesizedExpression extends BaseNode {
  type: "ParenthesizedExpression";
  expression: Expression;
}

export interface BinaryExpression extends BaseNode {
  type: "BinaryExpression";
  left: Expression | PrivateIdentifier;
  operator: BinaryOperator;
  right: Expression;
}

export interface LogicalExpression extends BaseNode {
  type: "LogicalExpression";
  left: Expression;
  operator: LogicalOperator;
  right: Expression;
}

export interface ConditionalExpression extends BaseNode {
  type: "ConditionalExpression";
  test: Expression;
  consequent: Expression;
  alternate: Expression;
}

export interface UnaryExpression extends BaseNode {
  type: "UnaryExpression";
  operator: UnaryOperator;
  prefix: true;
  argument: Expression;
}

export interface UpdateExpression extends BaseNode {
  type: "UpdateExpression";
  operator: UpdateOperator;
  prefix: boolean;
  argument: Expression;
}

export type SimpleAssignmentTarget =
  | IdentifierReference
  | MemberExpression
  | TSAsExpression
  | TSSatisfiesExpression
  | TSNonNullExpression
  | TSTypeAssertion;

export type AssignmentTargetPattern = ArrayPattern | ObjectPattern;

export type AssignmentTarget = SimpleAssignmentTarget | AssignmentTargetPattern;

export interface AssignmentExpression extends BaseNode {
  type: "AssignmentExpression";
  operator: AssignmentOperator;
  left: AssignmentTarget;
  right: Expression;
}

export interface YieldExpression extends BaseNode {
  type: "YieldExpression";
  delegate: boolean;
  argument: Expression | null;
}

export interface AwaitExpression extends BaseNode {
  type: "AwaitExpression";
  argument: Expression;
}

export type ArrayExpressionElement = Expression | SpreadElement | null;

export type ObjectPropertyKind = ObjectProperty | SpreadElement;

export type Argument = Expression | SpreadElement;

export interface ArrayExpression extends BaseNode {
  type: "ArrayExpression";
  elements: ArrayExpressionElement[];
}

export interface ObjectExpression extends BaseNode {
  type: "ObjectExpression";
  properties: ObjectPropertyKind[];
}

export interface SpreadElement extends BaseNode {
  type: "SpreadElement";
  argument: Expression;
}

interface MemberExpressionBase extends BaseNode {
  type: "MemberExpression";
  object: Expression | Super;
  optional: boolean;
}

export interface ComputedMemberExpression extends MemberExpressionBase {
  property: Expression;
  computed: true;
}

export interface StaticMemberExpression extends MemberExpressionBase {
  property: IdentifierName;
  computed: false;
}

export interface PrivateFieldExpression extends MemberExpressionBase {
  property: PrivateIdentifier;
  computed: false;
}

export type MemberExpression =
  | ComputedMemberExpression
  | StaticMemberExpression
  | PrivateFieldExpression;

export interface CallExpression extends BaseNode {
  type: "CallExpression";
  callee: Expression | Super;
  arguments: Argument[];
  optional: boolean;
  typeArguments?: TSTypeParameterInstantiation | null;
}

export interface ChainExpression extends BaseNode {
  type: "ChainExpression";
  expression: ChainElement;
}

export type ChainElement = CallExpression | MemberExpression | TSNonNullExpression;

export interface TaggedTemplateExpression extends BaseNode {
  type: "TaggedTemplateExpression";
  tag: Expression;
  quasi: TemplateLiteral;
  typeArguments?: TSTypeParameterInstantiation | null;
}

export interface NewExpression extends BaseNode {
  type: "NewExpression";
  callee: Expression;
  arguments: Argument[];
  typeArguments?: TSTypeParameterInstantiation | null;
}

export interface MetaProperty extends BaseNode {
  type: "MetaProperty";
  meta: IdentifierName;
  property: IdentifierName;
}

export interface ImportExpression extends BaseNode {
  type: "ImportExpression";
  source: Expression;
  options: Expression | null;
  phase: ImportPhase | null;
}

export interface TemplateLiteral extends BaseNode {
  type: "TemplateLiteral";
  quasis: TemplateElement[];
  expressions: Expression[];
}

export interface TemplateElement extends BaseNode {
  type: "TemplateElement";
  value: {
    raw: string;
    cooked: string | null;
  };
  tail: boolean;
}

export interface Super extends BaseNode {
  type: "Super";
}

export interface ThisExpression extends BaseNode {
  type: "ThisExpression";
}

export interface ExpressionStatement extends BaseNode {
  type: "ExpressionStatement";
  expression: Expression;
  directive?: null;
}

export interface Directive extends BaseNode {
  type: "ExpressionStatement";
  expression: StringLiteral;
  directive: string;
}

export interface BlockStatement extends BaseNode {
  type: "BlockStatement";
  body: (Statement | Directive)[];
}

export interface IfStatement extends BaseNode {
  type: "IfStatement";
  test: Expression;
  consequent: Statement;
  alternate: Statement | null;
}

export interface SwitchStatement extends BaseNode {
  type: "SwitchStatement";
  discriminant: Expression;
  cases: SwitchCase[];
}

export interface SwitchCase extends BaseNode {
  type: "SwitchCase";
  test: Expression | null;
  consequent: Statement[];
}

export type ForStatementInit = VariableDeclaration | Expression;
export type ForStatementLeft = VariableDeclaration | AssignmentTarget;

export interface ForStatement extends BaseNode {
  type: "ForStatement";
  init: ForStatementInit | null;
  test: Expression | null;
  update: Expression | null;
  body: Statement;
}

export interface ForInStatement extends BaseNode {
  type: "ForInStatement";
  left: ForStatementLeft;
  right: Expression;
  body: Statement;
}

export interface ForOfStatement extends BaseNode {
  type: "ForOfStatement";
  left: ForStatementLeft;
  right: Expression;
  body: Statement;
  await: boolean;
}

export interface WhileStatement extends BaseNode {
  type: "WhileStatement";
  test: Expression;
  body: Statement;
}

export interface DoWhileStatement extends BaseNode {
  type: "DoWhileStatement";
  body: Statement;
  test: Expression;
}

export interface BreakStatement extends BaseNode {
  type: "BreakStatement";
  label: LabelIdentifier | null;
}

export interface ContinueStatement extends BaseNode {
  type: "ContinueStatement";
  label: LabelIdentifier | null;
}

export interface LabeledStatement extends BaseNode {
  type: "LabeledStatement";
  label: LabelIdentifier;
  body: Statement;
}

export interface WithStatement extends BaseNode {
  type: "WithStatement";
  object: Expression;
  body: Statement;
}

export interface ReturnStatement extends BaseNode {
  type: "ReturnStatement";
  argument: Expression | null;
}

export interface ThrowStatement extends BaseNode {
  type: "ThrowStatement";
  argument: Expression;
}

export interface TryStatement extends BaseNode {
  type: "TryStatement";
  block: BlockStatement;
  handler: CatchClause | null;
  finalizer: BlockStatement | null;
}

export interface CatchClause extends BaseNode {
  type: "CatchClause";
  param: BindingPattern | null;
  body: BlockStatement;
}

export interface DebuggerStatement extends BaseNode {
  type: "DebuggerStatement";
}

export interface EmptyStatement extends BaseNode {
  type: "EmptyStatement";
}

export interface VariableDeclaration extends BaseNode {
  type: "VariableDeclaration";
  kind: VariableDeclarationKind;
  declarations: VariableDeclarator[];
  declare?: boolean;
}

export interface VariableDeclarator extends BaseNode {
  type: "VariableDeclarator";
  id: BindingPattern;
  init: Expression | null;
  definite?: boolean;
}

interface FunctionBase extends BaseNode {
  id: BindingIdentifier | null;
  generator: boolean;
  async: boolean;
  params: FunctionParameter[];
  expression: false;
}

interface BodiedFunction extends FunctionBase {
  body: BlockStatement | null;
  declare?: boolean;
  typeParameters?: TSTypeParameterDeclaration | null;
  returnType?: TSTypeAnnotation | null;
}

interface BodilessFunction extends FunctionBase {
  body: null;
  declare: boolean;
  typeParameters: TSTypeParameterDeclaration | null;
  returnType: TSTypeAnnotation | null;
}

export interface FunctionDeclaration extends BodiedFunction {
  type: "FunctionDeclaration";
}

export interface FunctionExpression extends BodiedFunction {
  type: "FunctionExpression";
}

export interface TSDeclareFunction extends BodilessFunction {
  type: "TSDeclareFunction";
}

export interface TSEmptyBodyFunctionExpression extends BodilessFunction {
  type: "TSEmptyBodyFunctionExpression";
}

export type Function =
  | FunctionDeclaration
  | FunctionExpression
  | TSDeclareFunction
  | TSEmptyBodyFunctionExpression;

export interface ArrowFunctionExpression extends BaseNode {
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

interface ClassBase extends BaseNode {
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

export interface ClassDeclaration extends ClassBase {
  type: "ClassDeclaration";
}

export interface ClassExpression extends ClassBase {
  type: "ClassExpression";
}

export type Class = ClassDeclaration | ClassExpression;

export interface ClassBody extends BaseNode {
  type: "ClassBody";
  body: ClassElement[];
}

interface MethodDefinitionBase extends BaseNode {
  decorators: Decorator[];
  key: PropertyKey;
  value: FunctionExpression | TSEmptyBodyFunctionExpression;
  kind: MethodDefinitionKind;
  computed: boolean;
  static: boolean;
  override?: boolean;
  optional?: boolean;
  accessibility?: TSAccessibility | null;
}

export interface MethodDefinition extends MethodDefinitionBase {
  type: "MethodDefinition";
}

export interface TSAbstractMethodDefinition extends MethodDefinitionBase {
  type: "TSAbstractMethodDefinition";
}

interface PropertyDefinitionBase extends BaseNode {
  decorators: Decorator[];
  key: PropertyKey;
  value: Expression | null;
  computed: boolean;
  static: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
  declare?: boolean;
  override?: boolean;
  optional?: boolean;
  definite?: boolean;
  readonly?: boolean;
  accessibility?: TSAccessibility | null;
}

export interface PropertyDefinition extends PropertyDefinitionBase {
  type: "PropertyDefinition";
}

export interface TSAbstractPropertyDefinition extends PropertyDefinitionBase {
  type: "TSAbstractPropertyDefinition";
}

export interface AccessorProperty extends PropertyDefinitionBase {
  type: "AccessorProperty";
}

export interface TSAbstractAccessorProperty extends PropertyDefinitionBase {
  type: "TSAbstractAccessorProperty";
}

export interface StaticBlock extends BaseNode {
  type: "StaticBlock";
  body: Statement[];
}

export interface Decorator extends BaseNode {
  type: "Decorator";
  expression: Expression;
}

export type ClassElement =
  | MethodDefinition
  | TSAbstractMethodDefinition
  | PropertyDefinition
  | TSAbstractPropertyDefinition
  | AccessorProperty
  | TSAbstractAccessorProperty
  | StaticBlock
  | TSIndexSignature;

export interface ImportDeclaration extends BaseNode {
  type: "ImportDeclaration";
  specifiers: ImportDeclarationSpecifier[];
  source: StringLiteral;
  phase: ImportPhase | null;
  attributes: ImportAttribute[];
  importKind?: ImportOrExportKind;
}

export type ImportDeclarationSpecifier =
  | ImportSpecifier
  | ImportDefaultSpecifier
  | ImportNamespaceSpecifier;

export interface ImportSpecifier extends BaseNode {
  type: "ImportSpecifier";
  imported: IdentifierName | StringLiteral;
  local: BindingIdentifier;
  importKind?: ImportOrExportKind;
}

export interface ImportDefaultSpecifier extends BaseNode {
  type: "ImportDefaultSpecifier";
  local: BindingIdentifier;
}

export interface ImportNamespaceSpecifier extends BaseNode {
  type: "ImportNamespaceSpecifier";
  local: BindingIdentifier;
}

export interface ImportAttribute extends BaseNode {
  type: "ImportAttribute";
  key: ImportAttributeKey;
  value: StringLiteral;
}

export type ImportAttributeKey = IdentifierName | StringLiteral;

export interface ExportNamedDeclaration extends BaseNode {
  type: "ExportNamedDeclaration";
  declaration: Declaration | null;
  specifiers: ExportSpecifier[];
  source: StringLiteral | null;
  attributes: ImportAttribute[];
  exportKind?: ImportOrExportKind;
}

export interface ExportDefaultDeclaration extends BaseNode {
  type: "ExportDefaultDeclaration";
  declaration: ExportDefaultDeclarationKind;
  exportKind?: "value";
}

export type ExportDefaultDeclarationKind = Function | Class | TSInterfaceDeclaration | Expression;

export interface ExportAllDeclaration extends BaseNode {
  type: "ExportAllDeclaration";
  exported: ModuleExportName | null;
  source: StringLiteral;
  attributes: ImportAttribute[];
  exportKind?: ImportOrExportKind;
}

export interface ExportSpecifier extends BaseNode {
  type: "ExportSpecifier";
  local: ModuleExportName;
  exported: ModuleExportName;
  exportKind?: ImportOrExportKind;
}

export type ModuleExportName = IdentifierName | IdentifierReference | StringLiteral;

export interface JSXElement extends BaseNode {
  type: "JSXElement";
  openingElement: JSXOpeningElement;
  children: JSXChild[];
  closingElement: JSXClosingElement | null;
}

export interface JSXOpeningElement extends BaseNode {
  type: "JSXOpeningElement";
  name: JSXElementName;
  attributes: JSXAttributeItem[];
  selfClosing: boolean;
  typeArguments?: TSTypeParameterInstantiation | null;
}

export interface JSXClosingElement extends BaseNode {
  type: "JSXClosingElement";
  name: JSXElementName;
}

export type JSXAttributeItem = JSXAttribute | JSXSpreadAttribute;

export interface JSXFragment extends BaseNode {
  type: "JSXFragment";
  openingFragment: JSXOpeningFragment;
  children: JSXChild[];
  closingFragment: JSXClosingFragment;
}

export interface JSXOpeningFragment extends BaseNode {
  type: "JSXOpeningFragment";
}

export interface JSXClosingFragment extends BaseNode {
  type: "JSXClosingFragment";
}

export interface JSXIdentifier extends BaseNode {
  type: "JSXIdentifier";
  name: string;
}

export interface JSXNamespacedName extends BaseNode {
  type: "JSXNamespacedName";
  namespace: JSXIdentifier;
  name: JSXIdentifier;
}

export interface JSXMemberExpression extends BaseNode {
  type: "JSXMemberExpression";
  object: JSXMemberExpressionObject;
  property: JSXIdentifier;
}

export type JSXMemberExpressionObject = JSXIdentifier | JSXMemberExpression;

export interface JSXAttribute extends BaseNode {
  type: "JSXAttribute";
  name: JSXAttributeName;
  value: JSXAttributeValue | null;
}

export type JSXAttributeName = JSXIdentifier | JSXNamespacedName;

export type JSXAttributeValue = StringLiteral | JSXExpressionContainer | JSXElement | JSXFragment;

export interface JSXSpreadAttribute extends BaseNode {
  type: "JSXSpreadAttribute";
  argument: Expression;
}

export interface JSXExpressionContainer extends BaseNode {
  type: "JSXExpressionContainer";
  expression: JSXExpression;
}

export type JSXExpression = JSXEmptyExpression | Expression;

export interface JSXEmptyExpression extends BaseNode {
  type: "JSXEmptyExpression";
}

export interface JSXText extends BaseNode {
  type: "JSXText";
  value: string;
  raw?: string;
}

export interface JSXSpreadChild extends BaseNode {
  type: "JSXSpreadChild";
  expression: Expression;
}

export type JSXElementName = JSXIdentifier | JSXNamespacedName | JSXMemberExpression;

export type JSXTagName = JSXElementName;

export type JSXChild = JSXText | JSXElement | JSXFragment | JSXExpressionContainer | JSXSpreadChild;

export interface TSTypeAnnotation extends BaseNode {
  type: "TSTypeAnnotation";
  typeAnnotation: TSType;
}

export interface TSAnyKeyword extends BaseNode {
  type: "TSAnyKeyword";
}

export interface TSUnknownKeyword extends BaseNode {
  type: "TSUnknownKeyword";
}

export interface TSNeverKeyword extends BaseNode {
  type: "TSNeverKeyword";
}

export interface TSVoidKeyword extends BaseNode {
  type: "TSVoidKeyword";
}

export interface TSNullKeyword extends BaseNode {
  type: "TSNullKeyword";
}

export interface TSUndefinedKeyword extends BaseNode {
  type: "TSUndefinedKeyword";
}

export interface TSStringKeyword extends BaseNode {
  type: "TSStringKeyword";
}

export interface TSNumberKeyword extends BaseNode {
  type: "TSNumberKeyword";
}

export interface TSBigIntKeyword extends BaseNode {
  type: "TSBigIntKeyword";
}

export interface TSBooleanKeyword extends BaseNode {
  type: "TSBooleanKeyword";
}

export interface TSSymbolKeyword extends BaseNode {
  type: "TSSymbolKeyword";
}

export interface TSObjectKeyword extends BaseNode {
  type: "TSObjectKeyword";
}

export interface TSIntrinsicKeyword extends BaseNode {
  type: "TSIntrinsicKeyword";
}

export interface TSThisType extends BaseNode {
  type: "TSThisType";
}

export interface TSTypeReference extends BaseNode {
  type: "TSTypeReference";
  typeName: TSTypeName;
  typeArguments: TSTypeParameterInstantiation | null;
}

export interface TSQualifiedName extends BaseNode {
  type: "TSQualifiedName";
  left: TSTypeName;
  right: IdentifierName;
}

export type TSTypeName = IdentifierReference | TSQualifiedName | ThisExpression;

export interface TSTypeQuery extends BaseNode {
  type: "TSTypeQuery";
  exprName: TSTypeQueryExprName;
  typeArguments: TSTypeParameterInstantiation | null;
}

export type TSTypeQueryExprName =
  | IdentifierReference
  | TSQualifiedName
  | ThisExpression
  | TSImportType;

export interface TSImportType extends BaseNode {
  type: "TSImportType";
  source: StringLiteral;
  options: ObjectExpression | null;
  qualifier: TSImportTypeQualifier | null;
  typeArguments: TSTypeParameterInstantiation | null;
}

export type TSImportTypeQualifier = IdentifierName | TSQualifiedName;

export interface TSTypeParameter extends BaseNode {
  type: "TSTypeParameter";
  name: BindingIdentifier;
  constraint: TSType | null;
  default: TSType | null;
  in: boolean;
  out: boolean;
  const: boolean;
}

export interface TSTypeParameterDeclaration extends BaseNode {
  type: "TSTypeParameterDeclaration";
  params: TSTypeParameter[];
}

export interface TSTypeParameterInstantiation extends BaseNode {
  type: "TSTypeParameterInstantiation";
  params: TSType[];
}

export interface TSLiteralType extends BaseNode {
  type: "TSLiteralType";
  literal:
    | StringLiteral
    | NumericLiteral
    | BigIntLiteral
    | BooleanLiteral
    | TemplateLiteral
    | UnaryExpression;
}

export interface TSTemplateLiteralType extends BaseNode {
  type: "TSTemplateLiteralType";
  quasis: TemplateElement[];
  types: TSType[];
}

export interface TSArrayType extends BaseNode {
  type: "TSArrayType";
  elementType: TSType;
}

export interface TSIndexedAccessType extends BaseNode {
  type: "TSIndexedAccessType";
  objectType: TSType;
  indexType: TSType;
}

export interface TSTupleType extends BaseNode {
  type: "TSTupleType";
  elementTypes: TSTupleElement[];
}

export interface TSNamedTupleMember extends BaseNode {
  type: "TSNamedTupleMember";
  label: IdentifierName;
  elementType: TSType;
  optional: boolean;
}

export interface TSOptionalType extends BaseNode {
  type: "TSOptionalType";
  typeAnnotation: TSType;
}

export interface TSRestType extends BaseNode {
  type: "TSRestType";
  typeAnnotation: TSType | TSNamedTupleMember;
}

export type TSTupleElement = TSType | TSNamedTupleMember | TSOptionalType | TSRestType;

export interface TSJSDocNullableType extends BaseNode {
  type: "TSJSDocNullableType";
  typeAnnotation: TSType;
  postfix: boolean;
}

export interface TSJSDocNonNullableType extends BaseNode {
  type: "TSJSDocNonNullableType";
  typeAnnotation: TSType;
  postfix: boolean;
}

export interface TSJSDocUnknownType extends BaseNode {
  type: "TSJSDocUnknownType";
}

export interface TSUnionType extends BaseNode {
  type: "TSUnionType";
  types: TSType[];
}

export interface TSIntersectionType extends BaseNode {
  type: "TSIntersectionType";
  types: TSType[];
}

export interface TSConditionalType extends BaseNode {
  type: "TSConditionalType";
  checkType: TSType;
  extendsType: TSType;
  trueType: TSType;
  falseType: TSType;
}

export interface TSInferType extends BaseNode {
  type: "TSInferType";
  typeParameter: TSTypeParameter;
}

export interface TSTypeOperator extends BaseNode {
  type: "TSTypeOperator";
  operator: TSTypeOperatorOperator;
  typeAnnotation: TSType;
}

export interface TSParenthesizedType extends BaseNode {
  type: "TSParenthesizedType";
  typeAnnotation: TSType;
}

interface TSFunctionSignatureBase extends BaseNode {
  typeParameters: TSTypeParameterDeclaration | null;
  params: FunctionParameter[];
  returnType: TSTypeAnnotation | null;
}

export interface TSFunctionType extends TSFunctionSignatureBase {
  type: "TSFunctionType";
}

export interface TSConstructorType extends TSFunctionSignatureBase {
  type: "TSConstructorType";
  abstract: boolean;
}

export interface TSTypePredicate extends BaseNode {
  type: "TSTypePredicate";
  parameterName: TSTypePredicateName;
  typeAnnotation: TSTypeAnnotation | null;
  asserts: boolean;
}

export type TSTypePredicateName = IdentifierName | TSThisType;

export interface TSTypeLiteral extends BaseNode {
  type: "TSTypeLiteral";
  members: TSSignature[];
}

export interface TSMappedType extends BaseNode {
  type: "TSMappedType";
  key: BindingIdentifier;
  constraint: TSType;
  nameType: TSType | null;
  typeAnnotation: TSType | null;
  optional: TSMappedTypeModifierOperator | false;
  readonly: TSMappedTypeModifierOperator | null;
}

export interface TSPropertySignature extends BaseNode {
  type: "TSPropertySignature";
  key: PropertyKey;
  typeAnnotation: TSTypeAnnotation | null;
  computed: boolean;
  optional: boolean;
  readonly: boolean;
  accessibility: null;
  static: false;
}

export interface TSMethodSignature extends TSFunctionSignatureBase {
  type: "TSMethodSignature";
  key: PropertyKey;
  computed: boolean;
  optional: boolean;
  kind: TSMethodSignatureKind;
  accessibility: null;
  readonly: false;
  static: false;
}

export interface TSCallSignatureDeclaration extends TSFunctionSignatureBase {
  type: "TSCallSignatureDeclaration";
}

export interface TSConstructSignatureDeclaration extends TSFunctionSignatureBase {
  type: "TSConstructSignatureDeclaration";
}

export interface TSIndexSignature extends BaseNode {
  type: "TSIndexSignature";
  parameters: BindingIdentifier[];
  typeAnnotation: TSTypeAnnotation;
  readonly: boolean;
  static: boolean;
  accessibility: null;
}

export type TSSignature =
  | TSPropertySignature
  | TSMethodSignature
  | TSCallSignatureDeclaration
  | TSConstructSignatureDeclaration
  | TSIndexSignature;

export interface TSTypeAliasDeclaration extends BaseNode {
  type: "TSTypeAliasDeclaration";
  id: BindingIdentifier;
  typeParameters: TSTypeParameterDeclaration | null;
  typeAnnotation: TSType;
  declare: boolean;
}

export interface TSInterfaceDeclaration extends BaseNode {
  type: "TSInterfaceDeclaration";
  id: BindingIdentifier;
  typeParameters: TSTypeParameterDeclaration | null;
  extends: TSInterfaceHeritage[];
  body: TSInterfaceBody;
  declare: boolean;
}

export interface TSInterfaceBody extends BaseNode {
  type: "TSInterfaceBody";
  body: TSSignature[];
}

export interface TSInterfaceHeritage extends BaseNode {
  type: "TSInterfaceHeritage";
  expression: Expression;
  typeArguments: TSTypeParameterInstantiation | null;
}

export interface TSClassImplements extends BaseNode {
  type: "TSClassImplements";
  expression: Expression;
  typeArguments: TSTypeParameterInstantiation | null;
}

export interface TSEnumDeclaration extends BaseNode {
  type: "TSEnumDeclaration";
  id: BindingIdentifier;
  body: TSEnumBody;
  const: boolean;
  declare: boolean;
}

export interface TSEnumBody extends BaseNode {
  type: "TSEnumBody";
  members: TSEnumMember[];
}

export interface TSEnumMember extends BaseNode {
  type: "TSEnumMember";
  id: TSEnumMemberName;
  initializer: Expression | null;
  computed: boolean;
}

export type TSEnumMemberName = IdentifierName | StringLiteral | TemplateLiteral;

export interface TSModuleDeclaration extends BaseNode {
  type: "TSModuleDeclaration";
  id: BindingIdentifier | StringLiteral | TSQualifiedName | IdentifierName;
  body?: TSModuleBlock;
  kind: TSModuleDeclarationKind;
  declare: boolean;
  global: boolean;
}

export interface TSModuleBlock extends BaseNode {
  type: "TSModuleBlock";
  body: ProgramStatement[];
}

export interface TSParameterProperty extends BaseNode {
  type: "TSParameterProperty";
  decorators: Decorator[];
  parameter: BindingIdentifier | AssignmentPattern;
  override: boolean;
  readonly: boolean;
  accessibility: TSAccessibility | null;
  static: false;
}

export interface TSAsExpression extends BaseNode {
  type: "TSAsExpression";
  expression: Expression;
  typeAnnotation: TSType;
}

export interface TSSatisfiesExpression extends BaseNode {
  type: "TSSatisfiesExpression";
  expression: Expression;
  typeAnnotation: TSType;
}

export interface TSTypeAssertion extends BaseNode {
  type: "TSTypeAssertion";
  typeAnnotation: TSType;
  expression: Expression;
}

export interface TSNonNullExpression extends BaseNode {
  type: "TSNonNullExpression";
  expression: Expression;
}

export interface TSInstantiationExpression extends BaseNode {
  type: "TSInstantiationExpression";
  expression: Expression;
  typeArguments: TSTypeParameterInstantiation;
}

export interface TSExportAssignment extends BaseNode {
  type: "TSExportAssignment";
  expression: Expression;
}

export interface TSNamespaceExportDeclaration extends BaseNode {
  type: "TSNamespaceExportDeclaration";
  id: IdentifierName;
}

export interface TSImportEqualsDeclaration extends BaseNode {
  type: "TSImportEqualsDeclaration";
  id: BindingIdentifier;
  moduleReference: TSModuleReference;
  importKind: ImportOrExportKind;
}

export type TSModuleReference = TSExternalModuleReference | IdentifierReference | TSQualifiedName;

export interface TSExternalModuleReference extends BaseNode {
  type: "TSExternalModuleReference";
  expression: StringLiteral;
}

export type TSType =
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

export interface Hashbang extends BaseNode {
  type: "Hashbang";
  value: string;
}

export interface Program extends BaseNode {
  type: "Program";
  sourceType: ModuleKind;
  hashbang: Hashbang | null;
  body: ProgramStatement[];
}

/** A statement, module declaration or directive, as `Program.body` holds. */
export type ProgramStatement = Statement | ModuleDeclaration | Directive;

export type Declaration =
  | FunctionDeclaration
  | ClassDeclaration
  | VariableDeclaration
  | TSDeclareFunction
  | TSTypeAliasDeclaration
  | TSInterfaceDeclaration
  | TSEnumDeclaration
  | TSModuleDeclaration
  | TSImportEqualsDeclaration;

export type Expression =
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

export type Statement =
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

export type ModuleDeclaration =
  | ImportDeclaration
  | ExportNamedDeclaration
  | ExportDefaultDeclaration
  | ExportAllDeclaration
  | TSExportAssignment
  | TSNamespaceExportDeclaration;

export type Node =
  | Program
  | Hashbang
  | Statement
  | Expression
  | ModuleDeclaration
  | Directive
  | ObjectProperty
  | BindingProperty
  | SpreadElement
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
  | TSEmptyBodyFunctionExpression
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
