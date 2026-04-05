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
 * Parse JS/TS source code synchronously on the current thread.
 */
export function parseSync(source: string, options?: ParseOptions): ParseResult;

/**
 * Parse JS/TS source code asynchronously on a background thread.
 */
export function parse(source: string, options?: ParseOptions): Promise<ParseResult>;

// AST node types

interface BaseNode {
	start: number;
	end: number;
}

type BinaryOperator = "==" | "!=" | "===" | "!==" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/" | "%" | "**" | "|" | "^" | "&" | "<<" | ">>" | ">>>" | "in" | "instanceof";
type LogicalOperator = "&&" | "||" | "??";
type UnaryOperator = "-" | "+" | "!" | "~" | "typeof" | "void" | "delete";
type UpdateOperator = "++" | "--";
type AssignmentOperator = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "**=" | "<<=" | ">>=" | ">>>=" | "|=" | "^=" | "&=" | "||=" | "&&=" | "??=";

interface Identifier extends BaseNode {
	type: "Identifier";
	name: string;
}
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
type Literal = StringLiteral | NumericLiteral | BigIntLiteral | BooleanLiteral | NullLiteral | RegExpLiteral;
interface ArrayPattern extends BaseNode {
	type: "ArrayPattern";
	elements: Array<BindingPattern | RestElement | null>;
}
interface ObjectPattern extends BaseNode {
	type: "ObjectPattern";
	properties: Array<Property | RestElement>;
}
interface AssignmentPattern extends BaseNode {
	type: "AssignmentPattern";
	left: BindingPattern;
	right: Expression;
}
interface RestElement extends BaseNode {
	type: "RestElement";
	argument: BindingPattern;
}
type BindingPattern = Identifier | ArrayPattern | ObjectPattern | AssignmentPattern;
type FunctionParameter = BindingPattern | RestElement;
interface Property extends BaseNode {
	type: "Property";
	kind: "init" | "get" | "set";
	key: Expression;
	value: Expression | BindingPattern;
	method: boolean;
	shorthand: boolean;
	computed: boolean;
}
interface SequenceExpression extends BaseNode {
	type: "SequenceExpression";
	expressions: Expression[];
}
interface BinaryExpression extends BaseNode {
	type: "BinaryExpression";
	left: Expression;
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
interface AssignmentExpression extends BaseNode {
	type: "AssignmentExpression";
	operator: AssignmentOperator;
	left: Expression | BindingPattern;
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
interface ArrayExpression extends BaseNode {
	type: "ArrayExpression";
	elements: Array<Expression | SpreadElement | null>;
}
interface ObjectExpression extends BaseNode {
	type: "ObjectExpression";
	properties: Array<Property | SpreadElement>;
}
interface SpreadElement extends BaseNode {
	type: "SpreadElement";
	argument: Expression;
}
interface MemberExpression extends BaseNode {
	type: "MemberExpression";
	object: Expression | Super;
	property: Expression | PrivateIdentifier;
	computed: boolean;
	optional: boolean;
}
interface CallExpression extends BaseNode {
	type: "CallExpression";
	callee: Expression | Super;
	arguments: Array<Expression | SpreadElement>;
	optional: boolean;
}
interface ChainExpression extends BaseNode {
	type: "ChainExpression";
	expression: CallExpression | MemberExpression;
}
interface TaggedTemplateExpression extends BaseNode {
	type: "TaggedTemplateExpression";
	tag: Expression;
	quasi: TemplateLiteral;
}
interface NewExpression extends BaseNode {
	type: "NewExpression";
	callee: Expression;
	arguments: Array<Expression | SpreadElement>;
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
	directive?: string;
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
interface ForStatement extends BaseNode {
	type: "ForStatement";
	init: VariableDeclaration | Expression | null;
	test: Expression | null;
	update: Expression | null;
	body: Statement;
}
interface ForInStatement extends BaseNode {
	type: "ForInStatement";
	left: VariableDeclaration | Expression;
	right: Expression;
	body: Statement;
}
interface ForOfStatement extends BaseNode {
	type: "ForOfStatement";
	left: VariableDeclaration | Expression;
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
	label: Identifier | null;
}
interface ContinueStatement extends BaseNode {
	type: "ContinueStatement";
	label: Identifier | null;
}
interface LabeledStatement extends BaseNode {
	type: "LabeledStatement";
	label: Identifier;
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
}
interface VariableDeclarator extends BaseNode {
	type: "VariableDeclarator";
	id: BindingPattern;
	init: Expression | null;
}
interface FunctionNodeBase extends BaseNode {
	id: Identifier | null;
	generator: boolean;
	async: boolean;
	declare?: boolean;
	params: FunctionParameter[];
	body: BlockStatement | null;
	expression: false;
}
interface FunctionDeclaration extends FunctionNodeBase {
	type: "FunctionDeclaration";
}
interface FunctionExpression extends FunctionNodeBase {
	type: "FunctionExpression";
}
interface TSDeclareFunction extends FunctionNodeBase {
	type: "TSDeclareFunction";
}
interface TSEmptyBodyFunctionExpression extends FunctionNodeBase {
	type: "TSEmptyBodyFunctionExpression";
}
interface ArrowFunctionExpression extends BaseNode {
	type: "ArrowFunctionExpression";
	id: null;
	generator: false;
	async: boolean;
	params: FunctionParameter[];
	body: BlockStatement | Expression;
	expression: boolean;
}
interface ClassNodeBase extends BaseNode {
	decorators: Decorator[];
	id: Identifier | null;
	superClass: Expression | null;
	body: ClassBody;
}
interface ClassDeclaration extends ClassNodeBase {
	type: "ClassDeclaration";
}
interface ClassExpression extends ClassNodeBase {
	type: "ClassExpression";
}
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
}
interface PropertyDefinition extends BaseNode {
	type: "PropertyDefinition";
	decorators: Decorator[];
	key: Expression | PrivateIdentifier;
	value: Expression | null;
	computed: boolean;
	static: boolean;
}
interface AccessorProperty extends BaseNode {
	type: "AccessorProperty";
	decorators: Decorator[];
	key: Expression | PrivateIdentifier;
	value: Expression | null;
	computed: boolean;
	static: boolean;
}
interface StaticBlock extends BaseNode {
	type: "StaticBlock";
	body: Statement[];
}
interface Decorator extends BaseNode {
	type: "Decorator";
	expression: Expression;
}
type ClassElement = MethodDefinition | PropertyDefinition | AccessorProperty | StaticBlock;
interface ImportDeclaration extends BaseNode {
	type: "ImportDeclaration";
	specifiers: Array<ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier>;
	source: StringLiteral;
	phase: "source" | "defer" | null;
	attributes: ImportAttribute[];
}
interface ImportSpecifier extends BaseNode {
	type: "ImportSpecifier";
	imported: Identifier | StringLiteral;
	local: Identifier;
}
interface ImportDefaultSpecifier extends BaseNode {
	type: "ImportDefaultSpecifier";
	local: Identifier;
}
interface ImportNamespaceSpecifier extends BaseNode {
	type: "ImportNamespaceSpecifier";
	local: Identifier;
}
interface ImportAttribute extends BaseNode {
	type: "ImportAttribute";
	key: Identifier | StringLiteral;
	value: StringLiteral;
}
interface ExportNamedDeclaration extends BaseNode {
	type: "ExportNamedDeclaration";
	declaration: Declaration | null;
	specifiers: ExportSpecifier[];
	source: StringLiteral | null;
	attributes: ImportAttribute[];
}
interface ExportDefaultDeclaration extends BaseNode {
	type: "ExportDefaultDeclaration";
	declaration: Declaration | Expression;
}
interface ExportAllDeclaration extends BaseNode {
	type: "ExportAllDeclaration";
	exported: Identifier | StringLiteral | null;
	source: StringLiteral;
	attributes: ImportAttribute[];
}
interface ExportSpecifier extends BaseNode {
	type: "ExportSpecifier";
	local: Identifier | StringLiteral;
	exported: Identifier | StringLiteral;
}
interface TSExportAssignment extends BaseNode {
	type: "TSExportAssignment";
	expression: Expression;
}
interface TSNamespaceExportDeclaration extends BaseNode {
	type: "TSNamespaceExportDeclaration";
	id: Identifier;
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
	attributes: [];
	selfClosing: false;
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
interface Program extends BaseNode {
	type: "Program";
	sourceType: "module" | "script";
	hashbang: string | null;
	body: Array<Statement | ModuleDeclaration>;
}
type Declaration = FunctionDeclaration | ClassDeclaration | VariableDeclaration | TSDeclareFunction;
type Expression = Identifier | Literal | ThisExpression | Super | ArrayExpression | ObjectExpression | FunctionExpression | ArrowFunctionExpression | ClassExpression | TaggedTemplateExpression | TemplateLiteral | MemberExpression | CallExpression | NewExpression | ChainExpression | SequenceExpression | BinaryExpression | LogicalExpression | ConditionalExpression | UnaryExpression | UpdateExpression | AssignmentExpression | YieldExpression | AwaitExpression | ImportExpression | MetaProperty | SpreadElement | TSEmptyBodyFunctionExpression | JSXElement | JSXFragment;
type Statement = ExpressionStatement | BlockStatement | EmptyStatement | DebuggerStatement | ReturnStatement | LabeledStatement | BreakStatement | ContinueStatement | IfStatement | SwitchStatement | ThrowStatement | TryStatement | WhileStatement | DoWhileStatement | ForStatement | ForInStatement | ForOfStatement | WithStatement | Declaration;
type ModuleDeclaration = ImportDeclaration | ExportNamedDeclaration | ExportDefaultDeclaration | ExportAllDeclaration | TSExportAssignment | TSNamespaceExportDeclaration;
type Node = Program | Statement | Expression | ModuleDeclaration | Property | PrivateIdentifier | TemplateElement | VariableDeclarator | CatchClause | SwitchCase | RestElement | ArrayPattern | ObjectPattern | AssignmentPattern | ClassBody | MethodDefinition | PropertyDefinition | AccessorProperty | StaticBlock | Decorator | ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportAttribute | ExportSpecifier | JSXOpeningElement | JSXClosingElement | JSXOpeningFragment | JSXClosingFragment | JSXIdentifier | JSXNamespacedName | JSXMemberExpression | JSXAttribute | JSXSpreadAttribute | JSXExpressionContainer | JSXEmptyExpression | JSXText | JSXSpreadChild;

export type { ParseOptions, ParseResult, Comment, Diagnostic, DiagnosticLabel, SourceType, SourceLang, BaseNode, Program, Statement, Expression, Declaration, ModuleDeclaration, Node, Identifier, PrivateIdentifier, Literal, StringLiteral, NumericLiteral, BigIntLiteral, BooleanLiteral, NullLiteral, RegExpLiteral, BindingPattern, FunctionParameter, Property, ArrayPattern, ObjectPattern, AssignmentPattern, RestElement, SequenceExpression, BinaryExpression, LogicalExpression, ConditionalExpression, UnaryExpression, UpdateExpression, AssignmentExpression, YieldExpression, AwaitExpression, ArrayExpression, ObjectExpression, SpreadElement, MemberExpression, CallExpression, ChainExpression, TaggedTemplateExpression, NewExpression, MetaProperty, ImportExpression, TemplateLiteral, TemplateElement, Super, ThisExpression, ExpressionStatement, BlockStatement, IfStatement, SwitchStatement, SwitchCase, ForStatement, ForInStatement, ForOfStatement, WhileStatement, DoWhileStatement, BreakStatement, ContinueStatement, LabeledStatement, WithStatement, ReturnStatement, ThrowStatement, TryStatement, CatchClause, DebuggerStatement, EmptyStatement, VariableDeclaration, VariableDeclarator, FunctionDeclaration, FunctionExpression, TSDeclareFunction, TSEmptyBodyFunctionExpression, ArrowFunctionExpression, ClassDeclaration, ClassExpression, ClassBody, MethodDefinition, PropertyDefinition, AccessorProperty, StaticBlock, Decorator, ClassElement, ImportDeclaration, ImportSpecifier, ImportDefaultSpecifier, ImportNamespaceSpecifier, ImportAttribute, ExportNamedDeclaration, ExportDefaultDeclaration, ExportAllDeclaration, ExportSpecifier, TSExportAssignment, TSNamespaceExportDeclaration, JSXElement, JSXOpeningElement, JSXClosingElement, JSXFragment, JSXOpeningFragment, JSXClosingFragment, JSXIdentifier, JSXNamespacedName, JSXMemberExpression, JSXAttribute, JSXSpreadAttribute, JSXExpressionContainer, JSXEmptyExpression, JSXText, JSXSpreadChild, JSXTagName, JSXChild, BinaryOperator, LogicalOperator, UnaryOperator, UpdateOperator, AssignmentOperator, FunctionNodeBase, ClassNodeBase };
