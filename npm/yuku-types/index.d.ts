/** How the source code should be parsed. */
type SourceType = "script" | "module";

type ModuleKind = SourceType;

/** Language variant of the source code. */
type SourceLang = "js" | "ts" | "jsx" | "tsx" | "dts" | "tsrx";
/** Whether a comment came from a line or block source comment. */
type CommentType = "Line" | "Block";

/**
 * A source comment in the flat {@link ParseResult.comments} list, carrying its
 * source span.
 */
interface Comment {
  type: CommentType;
  /** Comment text without the delimiters. */
  value: string;
  /** Byte offset of the comment start (delimiter included). */
  start: number;
  /** Byte offset of the comment end (delimiter included). */
  end: number;
}

/**
 * Position of an {@link AttachedComment} relative to its host node.
 *
 * - `before`: leading the host.
 * - `after`: trailing the host.
 * - `inside`: interior to an otherwise empty host.
 */
type CommentPosition = "before" | "after" | "inside";

/**
 * A comment attached to a single host AST node, via {@link BaseNode.comments}.
 *
 * `sameLine` is true when the comment shares a source line with the
 * host's adjacent edge (host's start for `before`, host's end for
 * `after`). For `inside` it is always `false`.
 */
interface AttachedComment {
  type: CommentType;
  position: CommentPosition;
  sameLine: boolean;
  /** Comment text without the delimiters. */
  value: string;
}

/** A labeled source span attached to a {@link Diagnostic}. */
interface DiagnosticLabel {
  /** Byte offset. */
  start: number;
  /** Byte offset. */
  end: number;
  message: string;
}

/** Severity level of a {@link Diagnostic}. */
type DiagnosticSeverity = "error" | "warning" | "hint" | "info";

/**
 * A diagnostic produced during parsing or semantic analysis.
 * The parser is error tolerant: an AST is always produced even when diagnostics exist.
 */
interface Diagnostic {
  severity: DiagnosticSeverity;
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

/** Discriminant `type` string of every AST node. */
type NodeType = Node["type"];

/** The node, or union of nodes, carrying a given `type`. */
type NodeOfType<K extends NodeType> = Extract<Node, { type: K }>;

/**
 * The walk context: one reused object exposing the current position and
 * the tree mutation operations. Valid only during the visit that
 * receives it; do not store it.
 */
declare class WalkContext<T extends Node = Node, S = unknown> {
  /** The node being visited. */
  readonly node: T;
  /** The node that holds {@link node}, or null at the walk root. */
  readonly parent: Node | null;
  /**
   * The field on {@link parent} holding {@link node} (or its array),
   * or null at the walk root.
   */
  readonly key: string | null;
  /** Index within an array field, or null in a plain field. */
  readonly index: number | null;
  /** State threaded through the walk (the third {@link walk} argument). */
  state: S;
  /** Ancestors from the walk root down to {@link parent}. */
  ancestors(): Node[];
  /** Do not descend into the current node's children. */
  skip(): void;
  /** Stop the walk entirely. */
  stop(): void;
  /**
   * Replace the current node. The walk continues into the replacement's
   * children, and `leave` fires for the replacement's type. A synthetic
   * node with `start === 0 && end === 0` inherits the original span,
   * for source maps. Throws at the walk root.
   */
  replace(node: Node): void;
  /**
   * Remove the current node from its parent: spliced from array fields,
   * nulled in plain fields. Children are not walked and `leave` does
   * not fire. Throws at the walk root.
   */
  remove(): void;
  /**
   * Insert a sibling before the current node. The inserted node is NOT
   * visited. Only valid inside an array field.
   */
  insertBefore(node: Node): void;
  /**
   * Insert a sibling after the current node. The inserted node IS
   * visited. Only valid inside an array field.
   */
  insertAfter(node: Node): void;
}
// AST node types

interface BaseNode {
  start: number;
  end: number;
  /**
   * Comments attached to this node in source order. Present only when
   * {@link ParseOptions.attachComments} is true.
   */
  comments?: AttachedComment[];
}

type Span = BaseNode;

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

type VariableDeclarationKind = "var" | "let" | "const" | "using" | "await using";

type PropertyKind = "init" | "get" | "set";

type MethodDefinitionKind = "constructor" | "method" | "get" | "set";

type TSAccessibility = "public" | "private" | "protected";

type TSMethodSignatureKind = "method" | "get" | "set";

type TSTypeOperatorOperator = "keyof" | "unique" | "readonly";

type TSModuleDeclarationKind = "module" | "namespace" | "global";

type TSMappedTypeModifierOperator = true | "+" | "-";

type ImportPhase = "source" | "defer";

type ImportOrExportKind = "value" | "type";

type FunctionType =
  | "FunctionDeclaration"
  | "FunctionExpression"
  | "TSDeclareFunction"
  | "TSEmptyBodyFunctionExpression";

type ClassType = "ClassDeclaration" | "ClassExpression";

type MethodDefinitionType = "MethodDefinition" | "TSAbstractMethodDefinition";

type PropertyDefinitionType = "PropertyDefinition" | "TSAbstractPropertyDefinition";

type AccessorPropertyType = "AccessorProperty" | "TSAbstractAccessorProperty";

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
  lazy?: boolean;
  decorators?: Decorator[];
  optional?: boolean;
  typeAnnotation?: TSTypeAnnotation | null;
}

interface ObjectPattern extends BaseNode {
  type: "ObjectPattern";
  properties: Array<BindingProperty | RestElement>;
  lazy?: boolean;
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
  kind: PropertyKind;
  key: PropertyKey;
  value: Expression;
  method: boolean;
  shorthand: boolean;
  computed: boolean;
  optional?: false;
}

interface BindingProperty extends BaseNode {
  type: "Property";
  kind: "init";
  key: PropertyKey;
  value: BindingPattern;
  method: false;
  shorthand: boolean;
  computed: boolean;
  optional?: false;
}

type Property = ObjectProperty | BindingProperty;

type PropertyKey = IdentifierName | PrivateIdentifier | Expression;

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
  expression: ChainElement;
}

type ChainElement = CallExpression | MemberExpression | TSNonNullExpression;

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
  meta: IdentifierName;
  property: IdentifierName;
}

interface ImportExpression extends BaseNode {
  type: "ImportExpression";
  source: Expression;
  options: Expression | null;
  phase: ImportPhase | null;
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
  body: (Statement | Directive | Expression)[];
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
  index: IdentifierReference | null;
  key: Expression | null;
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
  resetParam: BindingPattern | null;
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
  kind: VariableDeclarationKind;
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
  body: BlockStatement | JSXCodeBlock | null;
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
  body: BlockStatement | JSXCodeBlock | null;
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
  key: PropertyKey;
  value: FunctionExpression | TSEmptyBodyFunctionExpression;
  kind: MethodDefinitionKind;
  computed: boolean;
  static: boolean;
  override?: boolean;
  optional?: boolean;
  accessibility?: TSAccessibility | null;
}

interface TSAbstractMethodDefinition extends BaseNode {
  type: "TSAbstractMethodDefinition";
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

interface PropertyDefinition extends BaseNode {
  type: "PropertyDefinition";
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

interface TSAbstractPropertyDefinition extends BaseNode {
  type: "TSAbstractPropertyDefinition";
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

interface AccessorProperty extends BaseNode {
  type: "AccessorProperty";
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

interface TSAbstractAccessorProperty extends BaseNode {
  type: "TSAbstractAccessorProperty";
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
  specifiers: ImportDeclarationSpecifier[];
  source: StringLiteral | IdentifierReference;
  phase: ImportPhase | null;
  attributes: ImportAttribute[];
  importKind?: ImportOrExportKind;
}

type ImportDeclarationSpecifier =
  | ImportSpecifier
  | ImportDefaultSpecifier
  | ImportNamespaceSpecifier;

interface ImportSpecifier extends BaseNode {
  type: "ImportSpecifier";
  imported: IdentifierName | StringLiteral;
  local: BindingIdentifier;
  importKind?: ImportOrExportKind;
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
  key: ImportAttributeKey;
  value: StringLiteral;
}

type ImportAttributeKey = IdentifierName | StringLiteral;

interface ExportNamedDeclaration extends BaseNode {
  type: "ExportNamedDeclaration";
  declaration: Declaration | null;
  specifiers: ExportSpecifier[];
  source: StringLiteral | null;
  attributes: ImportAttribute[];
  exportKind?: ImportOrExportKind;
}

interface ExportDefaultDeclaration extends BaseNode {
  type: "ExportDefaultDeclaration";
  declaration: ExportDefaultDeclarationKind;
  exportKind?: "value";
}

type ExportDefaultDeclarationKind = Function | Class | TSInterfaceDeclaration | Expression;

interface ExportAllDeclaration extends BaseNode {
  type: "ExportAllDeclaration";
  exported: ModuleExportName | null;
  source: StringLiteral;
  attributes: ImportAttribute[];
  exportKind?: ImportOrExportKind;
}

interface ExportSpecifier extends BaseNode {
  type: "ExportSpecifier";
  local: ModuleExportName;
  exported: ModuleExportName;
  exportKind?: ImportOrExportKind;
}

type ModuleExportName = IdentifierName | IdentifierReference | StringLiteral;

interface JSXElement extends BaseNode {
  type: "JSXElement";
  openingElement: JSXOpeningElement;
  children: JSXChild[];
  closingElement: JSXClosingElement | null;
}

interface JSXOpeningElement extends BaseNode {
  type: "JSXOpeningElement";
  name: JSXElementName;
  attributes: JSXAttributeItem[];
  selfClosing: boolean;
  typeArguments?: TSTypeParameterInstantiation | null;
}

interface JSXClosingElement extends BaseNode {
  type: "JSXClosingElement";
  name: JSXElementName;
}

type JSXAttributeItem = JSXAttribute | JSXSpreadAttribute;

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
  object: JSXMemberExpressionObject;
  property: JSXIdentifier;
}

type JSXMemberExpressionObject = JSXIdentifier | JSXMemberExpression;

interface JSXAttribute extends BaseNode {
  type: "JSXAttribute";
  name: JSXAttributeName;
  value: JSXAttributeValue | null;
}

type JSXAttributeName = JSXIdentifier | JSXNamespacedName;

type JSXAttributeValue =
  | StringLiteral
  | JSXExpressionContainer
  | JSXElement
  | JSXFragment
  | JSXStyleElement;

interface JSXSpreadAttribute extends BaseNode {
  type: "JSXSpreadAttribute";
  argument: Expression;
}

interface JSXExpressionContainer extends BaseNode {
  type: "JSXExpressionContainer";
  expression: JSXExpression;
}

type JSXExpression = JSXEmptyExpression | Expression;

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

interface JSXCodeBlock extends BaseNode {
  type: "JSXCodeBlock";
  body: Statement[];
  render: Node | null;
}

interface StyleSheet extends BaseNode {
  type: "StyleSheet";
  source: string;
}

interface JSXStyleElement extends BaseNode {
  type: "JSXStyleElement";
  openingElement: JSXOpeningElement;
  children: StyleSheet[];
  closingElement: JSXClosingElement | null;
  css: string;
}

interface JSXIfExpression extends BaseNode {
  type: "JSXIfExpression";
  test: Expression;
  consequent: BlockStatement;
  alternate: BlockStatement | JSXIfExpression | null;
  statementType?: "IfStatement";
}

interface JSXForExpression extends BaseNode {
  type: "JSXForExpression";
  statement: ForStatement | ForInStatement | ForOfStatement;
  empty: BlockStatement | null;
  statementType?: "ForStatement" | "ForInStatement" | "ForOfStatement";
}

interface JSXSwitchExpression extends BaseNode {
  type: "JSXSwitchExpression";
  statement: SwitchStatement;
  statementType?: "SwitchStatement";
}

interface JSXTryExpression extends BaseNode {
  type: "JSXTryExpression";
  statement: TryStatement;
  pending: BlockStatement | null;
  statementType?: "TryStatement";
}

type JSXElementName =
  | JSXIdentifier
  | JSXNamespacedName
  | JSXMemberExpression
  | JSXExpressionContainer;

type JSXTagName = JSXElementName;

type JSXChild =
  | JSXText
  | JSXElement
  | JSXFragment
  | JSXStyleElement
  | JSXExpressionContainer
  | JSXSpreadChild
  | JSXCodeBlock
  | JSXIfExpression
  | JSXForExpression
  | JSXSwitchExpression
  | JSXTryExpression;

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
  exprName: TSTypeQueryExprName;
  typeArguments: TSTypeParameterInstantiation | null;
}

type TSTypeQueryExprName = IdentifierReference | TSQualifiedName | TSImportType;

interface TSImportType extends BaseNode {
  type: "TSImportType";
  source: StringLiteral;
  options: ObjectExpression | null;
  qualifier: TSImportTypeQualifier | null;
  typeArguments: TSTypeParameterInstantiation | null;
}

type TSImportTypeQualifier = IdentifierName | TSQualifiedName;

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
  literal:
    | StringLiteral
    | NumericLiteral
    | BigIntLiteral
    | BooleanLiteral
    | TemplateLiteral
    | UnaryExpression;
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
  operator: TSTypeOperatorOperator;
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
  parameterName: TSTypePredicateName;
  typeAnnotation: TSTypeAnnotation | null;
  asserts: boolean;
}

type TSTypePredicateName = IdentifierName | TSThisType;

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
  optional: TSMappedTypeModifierOperator | false;
  readonly: TSMappedTypeModifierOperator | null;
}

interface TSPropertySignature extends BaseNode {
  type: "TSPropertySignature";
  key: PropertyKey;
  typeAnnotation: TSTypeAnnotation | null;
  computed: boolean;
  optional: boolean;
  readonly: boolean;
  accessibility: null;
  static: false;
}

interface TSMethodSignature extends BaseNode {
  type: "TSMethodSignature";
  key: PropertyKey;
  computed: boolean;
  optional: boolean;
  kind: TSMethodSignatureKind;
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
  id: TSEnumMemberName;
  initializer: Expression | null;
  computed: boolean;
}

type TSEnumMemberName = IdentifierName | StringLiteral | TemplateLiteral;

interface TSModuleDeclaration extends BaseNode {
  type: "TSModuleDeclaration";
  id: BindingIdentifier | StringLiteral | TSQualifiedName | IdentifierName;
  body?: TSModuleBlock;
  kind: TSModuleDeclarationKind;
  declare: boolean;
  global: boolean;
}

interface TSModuleBlock extends BaseNode {
  type: "TSModuleBlock";
  body: ProgramStatement[];
}

interface TSParameterProperty extends BaseNode {
  type: "TSParameterProperty";
  decorators: Decorator[];
  parameter: BindingIdentifier | AssignmentPattern;
  override: boolean;
  readonly: boolean;
  accessibility: TSAccessibility | null;
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
  moduleReference: TSModuleReference;
  importKind: ImportOrExportKind;
}

type TSModuleReference = TSExternalModuleReference | IdentifierReference | TSQualifiedName;

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
  sourceType: ModuleKind;
  hashbang: Hashbang | null;
  body: ProgramStatement[];
}

/**
 * An element of `Program.body`. Unlike {@link Statement}, this also includes
 * {@link ModuleDeclaration} (`import`/`export`) and {@link Directive}, which
 * are only valid at the top level of a program, never in nested statement
 * positions such as a block, loop, or `if` body.
 */
type ProgramStatement = Statement | ModuleDeclaration | Directive;

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
  | JSXFragment
  | JSXStyleElement
  | JSXCodeBlock
  | JSXIfExpression
  | JSXForExpression
  | JSXSwitchExpression
  | JSXTryExpression;

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
  | JSXCodeBlock
  | StyleSheet
  | JSXStyleElement
  | JSXIfExpression
  | JSXForExpression
  | JSXSwitchExpression
  | JSXTryExpression
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

export { WalkContext };
export type {
  NodeOfType,
  NodeType,
  Comment,
  AttachedComment,
  CommentType,
  CommentPosition,
  Diagnostic,
  DiagnosticLabel,
  DiagnosticSeverity,
  SourceType,
  ModuleKind,
  SourceLang,
  BaseNode,
  Span,
  Program,
  ProgramStatement,
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
  ChainElement,
  ExportDefaultDeclarationKind,
  ImportDeclarationSpecifier,
  ImportAttributeKey,
  ModuleExportName,
  PropertyKey,
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
  VariableDeclarationKind,
  Function,
  FunctionDeclaration,
  FunctionExpression,
  TSDeclareFunction,
  TSEmptyBodyFunctionExpression,
  FunctionType,
  ArrowFunctionExpression,
  Class,
  ClassDeclaration,
  ClassExpression,
  ClassType,
  ClassBody,
  MethodDefinition,
  TSAbstractMethodDefinition,
  MethodDefinitionType,
  MethodDefinitionKind,
  PropertyDefinition,
  TSAbstractPropertyDefinition,
  PropertyDefinitionType,
  AccessorProperty,
  TSAbstractAccessorProperty,
  AccessorPropertyType,
  PropertyKind,
  StaticBlock,
  Decorator,
  ClassElement,
  ImportDeclaration,
  ImportSpecifier,
  ImportDefaultSpecifier,
  ImportNamespaceSpecifier,
  ImportAttribute,
  ImportPhase,
  ImportOrExportKind,
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
  JSXMemberExpressionObject,
  JSXAttribute,
  JSXAttributeItem,
  JSXAttributeName,
  JSXAttributeValue,
  JSXSpreadAttribute,
  JSXExpressionContainer,
  JSXExpression,
  JSXEmptyExpression,
  JSXText,
  JSXSpreadChild,
  JSXCodeBlock,
  StyleSheet,
  JSXStyleElement,
  JSXIfExpression,
  JSXForExpression,
  JSXSwitchExpression,
  JSXTryExpression,
  JSXTagName,
  JSXElementName,
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
  TSTypeQueryExprName,
  TSImportType,
  TSImportTypeQualifier,
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
  TSTypeOperatorOperator,
  TSParenthesizedType,
  TSFunctionType,
  TSConstructorType,
  TSTypePredicate,
  TSTypePredicateName,
  TSTypeLiteral,
  TSMappedType,
  TSMappedTypeModifierOperator,
  TSPropertySignature,
  TSMethodSignature,
  TSMethodSignatureKind,
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
  TSEnumMemberName,
  TSModuleDeclaration,
  TSModuleDeclarationKind,
  TSModuleBlock,
  TSParameterProperty,
  TSAccessibility,
  TSAsExpression,
  TSSatisfiesExpression,
  TSTypeAssertion,
  TSNonNullExpression,
  TSInstantiationExpression,
  TSExportAssignment,
  TSNamespaceExportDeclaration,
  TSImportEqualsDeclaration,
  TSExternalModuleReference,
  TSModuleReference,
  BinaryOperator,
  LogicalOperator,
  UnaryOperator,
  UpdateOperator,
  AssignmentOperator,
};
