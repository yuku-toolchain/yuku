// ESTree/TypeScript-ESTree AST node types.

export interface BaseNode {
	start: number;
	end: number;
}

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

export type UnaryOperator =
	| "-"
	| "+"
	| "!"
	| "~"
	| "typeof"
	| "void"
	| "delete";

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

export interface Identifier extends BaseNode {
	type: "Identifier";
	name: string;
}

export interface PrivateIdentifier extends BaseNode {
	type: "PrivateIdentifier";
	name: string;
}

export interface StringLiteral extends BaseNode {
	type: "Literal";
	value: string;
	raw: string;
}

export interface NumericLiteral extends BaseNode {
	type: "Literal";
	value: number | null;
	raw: string;
}

export interface BigIntLiteral extends BaseNode {
	type: "Literal";
	value: bigint;
	raw: string;
	bigint: string;
}

export interface BooleanLiteral extends BaseNode {
	type: "Literal";
	value: boolean;
	raw: string;
}

export interface NullLiteral extends BaseNode {
	type: "Literal";
	value: null;
	raw: "null";
}

export interface RegExpLiteral extends BaseNode {
	type: "Literal";
	value: RegExp | null;
	raw: string;
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

export interface ArrayPattern extends BaseNode {
	type: "ArrayPattern";
	elements: Array<BindingPattern | RestElement | null>;
}

export interface ObjectPattern extends BaseNode {
	type: "ObjectPattern";
	properties: Array<Property | RestElement>;
}

export interface AssignmentPattern extends BaseNode {
	type: "AssignmentPattern";
	left: BindingPattern;
	right: Expression;
}

export interface RestElement extends BaseNode {
	type: "RestElement";
	argument: BindingPattern;
}

export type BindingPattern =
	| Identifier
	| ArrayPattern
	| ObjectPattern
	| AssignmentPattern;

export type FunctionParameter = BindingPattern | RestElement;

export interface Property extends BaseNode {
	type: "Property";
	kind: "init" | "get" | "set";
	key: Expression;
	value: Expression | BindingPattern;
	method: boolean;
	shorthand: boolean;
	computed: boolean;
}

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
	left: Expression;
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

export interface AssignmentExpression extends BaseNode {
	type: "AssignmentExpression";
	operator: AssignmentOperator;
	left: Expression | BindingPattern;
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

export interface ArrayExpression extends BaseNode {
	type: "ArrayExpression";
	elements: Array<Expression | SpreadElement | null>;
}

export interface ObjectExpression extends BaseNode {
	type: "ObjectExpression";
	properties: Array<Property | SpreadElement>;
}

export interface SpreadElement extends BaseNode {
	type: "SpreadElement";
	argument: Expression;
}

export interface MemberExpression extends BaseNode {
	type: "MemberExpression";
	object: Expression | Super;
	property: Expression | PrivateIdentifier;
	computed: boolean;
	optional: boolean;
}

export interface CallExpression extends BaseNode {
	type: "CallExpression";
	callee: Expression | Super;
	arguments: Array<Expression | SpreadElement>;
	optional: boolean;
}

export interface ChainExpression extends BaseNode {
	type: "ChainExpression";
	expression: CallExpression | MemberExpression;
}

export interface TaggedTemplateExpression extends BaseNode {
	type: "TaggedTemplateExpression";
	tag: Expression;
	quasi: TemplateLiteral;
}

export interface NewExpression extends BaseNode {
	type: "NewExpression";
	callee: Expression;
	arguments: Array<Expression | SpreadElement>;
}

export interface MetaProperty extends BaseNode {
	type: "MetaProperty";
	meta: Identifier;
	property: Identifier;
}

export interface ImportExpression extends BaseNode {
	type: "ImportExpression";
	source: Expression;
	options: Expression | null;
	phase: "source" | "defer" | null;
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
	directive?: string;
}

export interface BlockStatement extends BaseNode {
	type: "BlockStatement";
	body: Statement[];
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

export interface ForStatement extends BaseNode {
	type: "ForStatement";
	init: VariableDeclaration | Expression | null;
	test: Expression | null;
	update: Expression | null;
	body: Statement;
}

export interface ForInStatement extends BaseNode {
	type: "ForInStatement";
	left: VariableDeclaration | Expression;
	right: Expression;
	body: Statement;
}

export interface ForOfStatement extends BaseNode {
	type: "ForOfStatement";
	left: VariableDeclaration | Expression;
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
	label: Identifier | null;
}

export interface ContinueStatement extends BaseNode {
	type: "ContinueStatement";
	label: Identifier | null;
}

export interface LabeledStatement extends BaseNode {
	type: "LabeledStatement";
	label: Identifier;
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
	kind: "var" | "let" | "const" | "using" | "await using";
	declarations: VariableDeclarator[];
}

export interface VariableDeclarator extends BaseNode {
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

export interface FunctionDeclaration extends FunctionNodeBase {
	type: "FunctionDeclaration";
}

export interface FunctionExpression extends FunctionNodeBase {
	type: "FunctionExpression";
}

export interface TSDeclareFunction extends FunctionNodeBase {
	type: "TSDeclareFunction";
}

export interface TSEmptyBodyFunctionExpression extends FunctionNodeBase {
	type: "TSEmptyBodyFunctionExpression";
}

export interface ArrowFunctionExpression extends BaseNode {
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

export interface ClassDeclaration extends ClassNodeBase {
	type: "ClassDeclaration";
}

export interface ClassExpression extends ClassNodeBase {
	type: "ClassExpression";
}

export interface ClassBody extends BaseNode {
	type: "ClassBody";
	body: ClassElement[];
}

export interface MethodDefinition extends BaseNode {
	type: "MethodDefinition";
	decorators: Decorator[];
	key: Expression | PrivateIdentifier;
	value: FunctionExpression | TSEmptyBodyFunctionExpression;
	kind: "constructor" | "method" | "get" | "set";
	computed: boolean;
	static: boolean;
}

export interface PropertyDefinition extends BaseNode {
	type: "PropertyDefinition";
	decorators: Decorator[];
	key: Expression | PrivateIdentifier;
	value: Expression | null;
	computed: boolean;
	static: boolean;
}

export interface AccessorProperty extends BaseNode {
	type: "AccessorProperty";
	decorators: Decorator[];
	key: Expression | PrivateIdentifier;
	value: Expression | null;
	computed: boolean;
	static: boolean;
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
	| PropertyDefinition
	| AccessorProperty
	| StaticBlock;

export interface ImportDeclaration extends BaseNode {
	type: "ImportDeclaration";
	specifiers: Array<
		ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier
	>;
	source: StringLiteral;
	phase: "source" | "defer" | null;
	attributes: ImportAttribute[];
}

export interface ImportSpecifier extends BaseNode {
	type: "ImportSpecifier";
	imported: Identifier | StringLiteral;
	local: Identifier;
}

export interface ImportDefaultSpecifier extends BaseNode {
	type: "ImportDefaultSpecifier";
	local: Identifier;
}

export interface ImportNamespaceSpecifier extends BaseNode {
	type: "ImportNamespaceSpecifier";
	local: Identifier;
}

export interface ImportAttribute extends BaseNode {
	type: "ImportAttribute";
	key: Identifier | StringLiteral;
	value: StringLiteral;
}

export interface ExportNamedDeclaration extends BaseNode {
	type: "ExportNamedDeclaration";
	declaration: Declaration | null;
	specifiers: ExportSpecifier[];
	source: StringLiteral | null;
	attributes: ImportAttribute[];
}

export interface ExportDefaultDeclaration extends BaseNode {
	type: "ExportDefaultDeclaration";
	declaration: Declaration | Expression;
}

export interface ExportAllDeclaration extends BaseNode {
	type: "ExportAllDeclaration";
	exported: Identifier | StringLiteral | null;
	source: StringLiteral;
	attributes: ImportAttribute[];
}

export interface ExportSpecifier extends BaseNode {
	type: "ExportSpecifier";
	local: Identifier | StringLiteral;
	exported: Identifier | StringLiteral;
}

export interface TSExportAssignment extends BaseNode {
	type: "TSExportAssignment";
	expression: Expression;
}

export interface TSNamespaceExportDeclaration extends BaseNode {
	type: "TSNamespaceExportDeclaration";
	id: Identifier;
}

export interface JSXElement extends BaseNode {
	type: "JSXElement";
	openingElement: JSXOpeningElement;
	children: JSXChild[];
	closingElement: JSXClosingElement | null;
}

export interface JSXOpeningElement extends BaseNode {
	type: "JSXOpeningElement";
	name: JSXTagName;
	attributes: Array<JSXAttribute | JSXSpreadAttribute>;
	selfClosing: boolean;
}

export interface JSXClosingElement extends BaseNode {
	type: "JSXClosingElement";
	name: JSXTagName;
}

export interface JSXFragment extends BaseNode {
	type: "JSXFragment";
	openingFragment: JSXOpeningFragment;
	children: JSXChild[];
	closingFragment: JSXClosingFragment;
}

export interface JSXOpeningFragment extends BaseNode {
	type: "JSXOpeningFragment";
	attributes: [];
	selfClosing: false;
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
	object: JSXIdentifier | JSXMemberExpression;
	property: JSXIdentifier;
}

export interface JSXAttribute extends BaseNode {
	type: "JSXAttribute";
	name: JSXIdentifier | JSXNamespacedName;
	value:
		| StringLiteral
		| JSXExpressionContainer
		| JSXElement
		| JSXFragment
		| null;
}

export interface JSXSpreadAttribute extends BaseNode {
	type: "JSXSpreadAttribute";
	argument: Expression;
}

export interface JSXExpressionContainer extends BaseNode {
	type: "JSXExpressionContainer";
	expression: Expression | JSXEmptyExpression;
}

export interface JSXEmptyExpression extends BaseNode {
	type: "JSXEmptyExpression";
}

export interface JSXText extends BaseNode {
	type: "JSXText";
	value: string;
	raw: string;
}

export interface JSXSpreadChild extends BaseNode {
	type: "JSXSpreadChild";
	expression: Expression;
}

export type JSXTagName =
	| JSXIdentifier
	| JSXNamespacedName
	| JSXMemberExpression;

export type JSXChild =
	| JSXText
	| JSXElement
	| JSXFragment
	| JSXExpressionContainer
	| JSXSpreadChild;

export interface Program extends BaseNode {
	type: "Program";
	sourceType: "module" | "script";
	hashbang: string | null;
	body: Array<Statement | ModuleDeclaration>;
}

export type Declaration =
	| FunctionDeclaration
	| ClassDeclaration
	| VariableDeclaration
	| TSDeclareFunction;

export type Expression =
	| Identifier
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
	| SpreadElement
	| TSEmptyBodyFunctionExpression
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
	| Statement
	| Expression
	| ModuleDeclaration
	| Property
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
	| PropertyDefinition
	| AccessorProperty
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
	| JSXSpreadChild;
