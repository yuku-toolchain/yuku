# ESTree Specification

This document specifies the complete ESTree AST node types, including ES5 through ES2026 and Stage 3 proposals.

## Table of Contents

- [Node Objects](#node-objects)
- [Identifier](#identifier)
- [Literal](#literal)
- [Programs](#programs)
- [Functions](#functions)
- [Statements](#statements)
- [Declarations](#declarations)
- [Expressions](#expressions)
- [Patterns](#patterns)
- [Classes](#classes)
- [Modules](#modules)
- [Decorators](#decorators)

---

## Node Objects

ESTree AST nodes are represented as `Node` objects, which may have any prototype inheritance but which implement the following interface:

```js
interface Node {
    type: string;
    loc: SourceLocation | null;
}
```

The `type` field is a string representing the AST variant type. Each subtype of `Node` is documented below with the specific string of its `type` field.

The `loc` field represents the source location information of the node. If the node contains no information about the source location, the field is `null`; otherwise it is an object consisting of a start position and an end position:

```js
interface SourceLocation {
    source: string | null;
    start: Position;
    end: Position;
}
```

Each `Position` object consists of a `line` number (1-indexed) and a `column` number (0-indexed):

```js
interface Position {
    line: number; // >= 1
    column: number; // >= 0
}
```

---

## Identifier

```js
interface Identifier <: Expression, Pattern {
    type: "Identifier";
    name: string;
}
```

An identifier. Note that an identifier may be an expression or a destructuring pattern.

### PrivateIdentifier

```js
interface PrivateIdentifier <: Node {
    type: "PrivateIdentifier";
    name: string;
}
```

A private identifier refers to private class elements. For a private name `#a`, its `name` is `a`.

---

## Literal

```js
interface Literal <: Expression {
    type: "Literal";
    value: string | boolean | null | number | RegExp | bigint;
}
```

A literal token. Note that a literal can be an expression.

### RegExpLiteral

```js
interface RegExpLiteral <: Literal {
    regex: {
        pattern: string;
        flags: string;
    };
}
```

The `regex` property allows regexes to be represented in environments that don't support certain flags such as `y` or `u`. In environments that don't support these flags `value` will be `null` as the regex can't be represented natively.

### BigIntLiteral

```js
interface BigIntLiteral <: Literal {
    bigint: string;
}
```

- `bigint` property is the string representation of the `BigInt` value. It must contain only decimal digits and not include numeric separators (`_`) or the suffix `n`.
- In environments that don't support `BigInt` values, `value` property will be `null` as the `BigInt` value can't be represented natively.

---

## Programs

```js
interface Program <: Node {
    type: "Program";
    sourceType: "script" | "module";
    body: [ Directive | Statement | ImportOrExportDeclaration ];
}
```

A complete program source tree. Parsers must specify `sourceType` as `"module"` if the source has been parsed as an ES6 module. Otherwise, `sourceType` must be `"script"`.

---

## Functions

```js
interface Function <: Node {
    id: Identifier | null;
    params: [ Pattern ];
    body: FunctionBody;
    generator: boolean;
    async: boolean;
}
```

A function declaration or expression.

### FunctionBody

```js
interface FunctionBody <: BlockStatement {
    body: [ Directive | Statement ];
}
```

The body of a function, which is a block statement that may begin with directives.

---

## Statements

```js
interface Statement <: Node { }
```

Any statement.

### ExpressionStatement

```js
interface ExpressionStatement <: Statement {
    type: "ExpressionStatement";
    expression: Expression;
}
```

An expression statement, i.e., a statement consisting of a single expression.

### Directive

```js
interface Directive <: ExpressionStatement {
    expression: Literal;
    directive: string;
}
```

A directive from the directive prologue of a script or function. The `directive` property is the raw string source of the directive without quotes.

### BlockStatement

```js
interface BlockStatement <: Statement {
    type: "BlockStatement";
    body: [ Statement ];
}
```

A block statement, i.e., a sequence of statements surrounded by braces.

### EmptyStatement

```js
interface EmptyStatement <: Statement {
    type: "EmptyStatement";
}
```

An empty statement, i.e., a solitary semicolon.

### DebuggerStatement

```js
interface DebuggerStatement <: Statement {
    type: "DebuggerStatement";
}
```

A `debugger` statement.

### WithStatement

```js
interface WithStatement <: Statement {
    type: "WithStatement";
    object: Expression;
    body: Statement;
}
```

A `with` statement.

### Control Flow

#### ReturnStatement

```js
interface ReturnStatement <: Statement {
    type: "ReturnStatement";
    argument: Expression | null;
}
```

A `return` statement.

#### LabeledStatement

```js
interface LabeledStatement <: Statement {
    type: "LabeledStatement";
    label: Identifier;
    body: Statement;
}
```

A labeled statement, i.e., a statement prefixed by a `break`/`continue` label.

#### BreakStatement

```js
interface BreakStatement <: Statement {
    type: "BreakStatement";
    label: Identifier | null;
}
```

A `break` statement.

#### ContinueStatement

```js
interface ContinueStatement <: Statement {
    type: "ContinueStatement";
    label: Identifier | null;
}
```

A `continue` statement.

### Choice

#### IfStatement

```js
interface IfStatement <: Statement {
    type: "IfStatement";
    test: Expression;
    consequent: Statement;
    alternate: Statement | null;
}
```

An `if` statement.

#### SwitchStatement

```js
interface SwitchStatement <: Statement {
    type: "SwitchStatement";
    discriminant: Expression;
    cases: [ SwitchCase ];
}
```

A `switch` statement.

##### SwitchCase

```js
interface SwitchCase <: Node {
    type: "SwitchCase";
    test: Expression | null;
    consequent: [ Statement ];
}
```

A `case` (if `test` is an `Expression`) or `default` (if `test === null`) clause in the body of a `switch` statement.

### Exceptions

#### ThrowStatement

```js
interface ThrowStatement <: Statement {
    type: "ThrowStatement";
    argument: Expression;
}
```

A `throw` statement.

#### TryStatement

```js
interface TryStatement <: Statement {
    type: "TryStatement";
    block: BlockStatement;
    handler: CatchClause | null;
    finalizer: BlockStatement | null;
}
```

A `try` statement. If `handler` is `null` then `finalizer` must be a `BlockStatement`.

##### CatchClause

```js
interface CatchClause <: Node {
    type: "CatchClause";
    param: Pattern | null;
    body: BlockStatement;
}
```

A `catch` clause following a `try` block. The `param` is `null` if the `catch` binding is omitted (ES2019+). E.g., `try { foo() } catch { bar() }`

### Loops

#### WhileStatement

```js
interface WhileStatement <: Statement {
    type: "WhileStatement";
    test: Expression;
    body: Statement;
}
```

A `while` statement.

#### DoWhileStatement

```js
interface DoWhileStatement <: Statement {
    type: "DoWhileStatement";
    body: Statement;
    test: Expression;
}
```

A `do`/`while` statement.

#### ForStatement

```js
interface ForStatement <: Statement {
    type: "ForStatement";
    init: VariableDeclaration | Expression | null;
    test: Expression | null;
    update: Expression | null;
    body: Statement;
}
```

A `for` statement.

#### ForInStatement

```js
interface ForInStatement <: Statement {
    type: "ForInStatement";
    left: VariableDeclaration | Pattern;
    right: Expression;
    body: Statement;
}
```

A `for`/`in` statement.

#### ForOfStatement

```js
interface ForOfStatement <: ForInStatement {
    type: "ForOfStatement";
    await: boolean;
}
```

A `for`/`of` statement. The `await` property is `true` for `for-await-of` statements (ES2018+).

---

## Declarations

```js
interface Declaration <: Statement { }
```

Any declaration node. Note that declarations are considered statements; this is because declarations can appear in any statement context.

### FunctionDeclaration

```js
interface FunctionDeclaration <: Function, Declaration {
    type: "FunctionDeclaration";
    id: Identifier;
}
```

A function declaration. Note that unlike in the parent interface `Function`, the `id` cannot be `null`, except for `export default function() {}`.

```js
interface AnonymousDefaultExportedFunctionDeclaration <: Function {
    type: "FunctionDeclaration";
    id: null;
}
```

### VariableDeclaration

```js
interface VariableDeclaration <: Declaration {
    type: "VariableDeclaration";
    declarations: [ VariableDeclarator ];
    kind: "var" | "let" | "const" | "using" | "await using";
}
```

A variable declaration. The `kind` property specifies what kind of declaration is represented:
- `"var"`: ES5 var declaration
- `"let"`, `"const"`: ES2015 let/const declarations
- `"using"`, `"await using"`: ES2026 using declarations

For `"using"` or `"await using"` declarations: for every declarator `d` of `declarations`, `d.id` must be an Identifier. If the variable declaration is the `left` of a ForOfStatement, `d.init` must be `null`, otherwise `d.init` must be an Expression.

#### VariableDeclarator

```js
interface VariableDeclarator <: Node {
    type: "VariableDeclarator";
    id: Pattern;
    init: Expression | null;
}
```

A variable declarator.

---

## Expressions

```js
interface Expression <: Node { }
```

Any expression node. Since the left-hand side of an assignment may be any expression in general, an expression can also be a pattern.

### ThisExpression

```js
interface ThisExpression <: Expression {
    type: "ThisExpression";
}
```

A `this` expression.

### Super

```js
interface Super <: Node {
    type: "Super";
}
```

A `super` pseudo-expression.

### ArrayExpression

```js
interface ArrayExpression <: Expression {
    type: "ArrayExpression";
    elements: [ Expression | SpreadElement | null ];
}
```

An array expression. An element might be `null` if it represents a hole in a sparse array. E.g. `[1,,2]`.

### ObjectExpression

```js
interface ObjectExpression <: Expression {
    type: "ObjectExpression";
    properties: [ Property | SpreadElement ];
}
```

An object expression. Spread properties are supported (ES2018+).

#### Property

```js
interface Property <: Node {
    type: "Property";
    key: Expression;
    value: Expression;
    kind: "init" | "get" | "set";
    method: boolean;
    shorthand: boolean;
    computed: boolean;
}
```

A property in an object expression.

### FunctionExpression

```js
interface FunctionExpression <: Function, Expression {
    type: "FunctionExpression";
}
```

A `function` expression.

### ArrowFunctionExpression

```js
interface ArrowFunctionExpression <: Function, Expression {
    type: "ArrowFunctionExpression";
    body: FunctionBody | Expression;
    expression: boolean;
    generator: false;
}
```

A fat arrow function expression, e.g., `let foo = (bar) => { /* body */ }`.

### Unary Operations

#### UnaryExpression

```js
interface UnaryExpression <: Expression {
    type: "UnaryExpression";
    operator: UnaryOperator;
    prefix: boolean;
    argument: Expression;
}
```

A unary operator expression.

##### UnaryOperator

```js
enum UnaryOperator {
    "-" | "+" | "!" | "~" | "typeof" | "void" | "delete"
}
```

#### UpdateExpression

```js
interface UpdateExpression <: Expression {
    type: "UpdateExpression";
    operator: UpdateOperator;
    argument: Expression;
    prefix: boolean;
}
```

An update (increment or decrement) operator expression.

##### UpdateOperator

```js
enum UpdateOperator {
    "++" | "--"
}
```

### Binary Operations

#### BinaryExpression

```js
interface BinaryExpression <: Expression {
    type: "BinaryExpression";
    operator: BinaryOperator;
    left: Expression | PrivateIdentifier;
    right: Expression;
}
```

A binary operator expression. `left` can be a private identifier (e.g. `#foo`) when `operator` is `"in"` (ES2022+).

##### BinaryOperator

```js
enum BinaryOperator {
    "==" | "!=" | "===" | "!=="
         | "<" | "<=" | ">" | ">="
         | "<<" | ">>" | ">>>"
         | "+" | "-" | "*" | "/" | "%"
         | "**"
         | "|" | "^" | "&" | "in"
         | "instanceof"
}
```

The `"**"` operator was added in ES2016.

#### AssignmentExpression

```js
interface AssignmentExpression <: Expression {
    type: "AssignmentExpression";
    operator: AssignmentOperator;
    left: Pattern;
    right: Expression;
}
```

An assignment operator expression.

##### AssignmentOperator

```js
enum AssignmentOperator {
    "=" | "+=" | "-=" | "*=" | "/=" | "%="
        | "<<=" | ">>=" | ">>>="
        | "|=" | "^=" | "&="
        | "**="
        | "||=" | "&&=" | "??="
}
```

- `"**="` was added in ES2016
- `"||="`, `"&&="`, `"??="` were added in ES2021 with short-circuiting behavior

#### LogicalExpression

```js
interface LogicalExpression <: Expression {
    type: "LogicalExpression";
    operator: LogicalOperator;
    left: Expression;
    right: Expression;
}
```

A logical operator expression.

##### LogicalOperator

```js
enum LogicalOperator {
    "||" | "&&" | "??"
}
```

The `"??"` (nullish coalescing) operator was added in ES2020.

#### MemberExpression

```js
interface MemberExpression <: Expression, Pattern, ChainElement {
    type: "MemberExpression";
    object: Expression | Super;
    property: Expression | PrivateIdentifier;
    computed: boolean;
    optional: boolean;
}
```

A member expression. If `computed` is `true`, the node corresponds to a computed (`a[b]`) member expression and `property` is an `Expression`. If `computed` is `false`, the node corresponds to a static (`a.b`) member expression and `property` is an `Identifier`.

- When `property` is a `PrivateIdentifier`, `computed` must be `false` (ES2022+)
- When `object` is a `Super`, `property` cannot be a `PrivateIdentifier`

### ConditionalExpression

```js
interface ConditionalExpression <: Expression {
    type: "ConditionalExpression";
    test: Expression;
    alternate: Expression;
    consequent: Expression;
}
```

A conditional expression, i.e., a ternary `?`/`:` expression.

### CallExpression

```js
interface CallExpression <: Expression, ChainElement {
    type: "CallExpression";
    callee: Expression | Super;
    arguments: [ Expression | SpreadElement ];
    optional: boolean;
}
```

A function or method call expression.

### NewExpression

```js
interface NewExpression <: Expression {
    type: "NewExpression";
    callee: Expression;
    arguments: [ Expression | SpreadElement ];
}
```

A `new` expression.

### SequenceExpression

```js
interface SequenceExpression <: Expression {
    type: "SequenceExpression";
    expressions: [ Expression ];
}
```

A sequence expression, i.e., a comma-separated sequence of expressions.

### SpreadElement

```js
interface SpreadElement <: Node {
    type: "SpreadElement";
    argument: Expression;
}
```

Spread expression, e.g., `[head, ...iter, tail]`, `f(head, ...iter, ...tail)`.

### YieldExpression

```js
interface YieldExpression <: Expression {
    type: "YieldExpression";
    argument: Expression | null;
    delegate: boolean;
}
```

A `yield` expression (ES2015+).

### AwaitExpression

```js
interface AwaitExpression <: Expression {
    type: "AwaitExpression";
    argument: Expression;
}
```

An `await` expression (ES2017+).

### ChainExpression

```js
interface ChainExpression <: Expression {
    type: "ChainExpression";
    expression: ChainElement;
}

interface ChainElement <: Node {
    optional: boolean;
}
```

The `ChainExpression` node is the root of optional chaining (ES2020+). The node contains one or more `ChainElement` nodes that are `optional:true`.

### ImportExpression

```js
interface ImportExpression <: Expression {
    type: "ImportExpression";
    source: Expression;
    options: Expression | null;
    phase: "source" | "defer" | null;
}
```

Dynamic import expression, e.g., `import(source)` (ES2020+).
- The `options` property contains an `Expression` when import attributes are present (ES2025+)
- The `phase` property is `"source"` for `import.source("X")` (Stage 3)
- The `phase` property is `"defer"` for `import.defer("X")` (Stage 3)

### MetaProperty

```js
interface MetaProperty <: Expression {
    type: "MetaProperty";
    meta: Identifier;
    property: Identifier;
}
```

Represents meta properties like `new.target` and `import.meta` (ES2015+).

### Template Literals

#### TemplateLiteral

```js
interface TemplateLiteral <: Expression {
    type: "TemplateLiteral";
    quasis: [ TemplateElement ];
    expressions: [ Expression ];
}
```

#### TaggedTemplateExpression

```js
interface TaggedTemplateExpression <: Expression {
    type: "TaggedTemplateExpression";
    tag: Expression;
    quasi: TemplateLiteral;
}
```

#### TemplateElement

```js
interface TemplateElement <: Node {
    type: "TemplateElement";
    tail: boolean;
    value: {
        cooked: string | null;
        raw: string;
    };
}
```

If the template literal is tagged and the text has an invalid escape, `cooked` will be `null` (ES2018+).

---

## Patterns

```js
interface Pattern <: Node { }
```

Patterns are used in destructuring binding and assignment.

### ObjectPattern

```js
interface ObjectPattern <: Pattern {
    type: "ObjectPattern";
    properties: [ AssignmentProperty | RestElement ];
}
```

```js
interface AssignmentProperty <: Property {
    type: "Property";
    value: Pattern;
    kind: "init";
    method: false;
}
```

Rest properties are supported (ES2018+).

### ArrayPattern

```js
interface ArrayPattern <: Pattern {
    type: "ArrayPattern";
    elements: [ Pattern | null ];
}
```

### RestElement

```js
interface RestElement <: Pattern {
    type: "RestElement";
    argument: Pattern;
}
```

### AssignmentPattern

```js
interface AssignmentPattern <: Pattern {
    type: "AssignmentPattern";
    left: Pattern;
    right: Expression;
}
```

---

## Classes

```js
interface Class <: Node {
    id: Identifier | null;
    superClass: Expression | null;
    body: ClassBody;
    decorators: [ Decorator ];
}
```

### ClassBody

```js
interface ClassBody <: Node {
    type: "ClassBody";
    body: [ MethodDefinition | PropertyDefinition | StaticBlock | AccessorProperty ];
}
```

### MethodDefinition

```js
interface MethodDefinition <: Node {
    type: "MethodDefinition";
    key: Expression | PrivateIdentifier;
    value: FunctionExpression;
    kind: "constructor" | "method" | "get" | "set";
    computed: boolean;
    static: boolean;
    decorators: [ Decorator ];
}
```

- When `key` is a `PrivateIdentifier`, `computed` must be `false` and `kind` cannot be `"constructor"` (ES2022+)

### PropertyDefinition

```js
interface PropertyDefinition <: Node {
    type: "PropertyDefinition";
    key: Expression | PrivateIdentifier;
    value: Expression | null;
    computed: boolean;
    static: boolean;
    decorators: [ Decorator ];
}
```

Class field definition (ES2022+). When `key` is a `PrivateIdentifier`, `computed` must be `false`.

### AccessorProperty

```js
interface AccessorProperty <: Node {
    type: "AccessorProperty";
    key: Expression | PrivateIdentifier;
    value: Expression | null;
    computed: boolean;
    static: boolean;
    decorators: [ Decorator ];
}
```

Auto-accessor property (Stage 3 decorators).

### StaticBlock

```js
interface StaticBlock <: BlockStatement {
    type: "StaticBlock";
}
```

A static block `static { }` is a block statement serving as an additional static initializer (ES2022+).

### ClassDeclaration

```js
interface ClassDeclaration <: Class, Declaration {
    type: "ClassDeclaration";
    id: Identifier;
}
```

```js
interface AnonymousDefaultExportedClassDeclaration <: Class {
    type: "ClassDeclaration";
    id: null;
}
```

### ClassExpression

```js
interface ClassExpression <: Class, Expression {
    type: "ClassExpression";
}
```

---

## Modules

### ImportOrExportDeclaration

```js
interface ImportOrExportDeclaration <: Node { }
```

An `import` or `export` declaration.

### ModuleSpecifier

```js
interface ModuleSpecifier <: Node {
    local: Identifier;
}
```

A specifier in an import or export declaration.

### Imports

#### ImportDeclaration

```js
interface ImportDeclaration <: ImportOrExportDeclaration {
    type: "ImportDeclaration";
    specifiers: [ ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier ];
    source: Literal;
    attributes: [ ImportAttribute ];
    phase: "source" | "defer" | null;
}
```

An import declaration, e.g., `import foo from "mod";`.
- The `attributes` property is non-empty when import attributes are present (ES2025+)
- The `phase` property is `"source"` for `import source X from "X"` (Stage 3)
- The `phase` property is `"defer"` for `import defer * as X from "X"` (Stage 3)
- When `phase` is `"source"`, specifiers must be a length-1 array with `ImportDefaultSpecifier`
- When `phase` is `"defer"`, specifiers must be a length-1 array with `ImportNamespaceSpecifier`

#### ImportSpecifier

```js
interface ImportSpecifier <: ModuleSpecifier {
    type: "ImportSpecifier";
    imported: Identifier | Literal;
}
```

An imported variable binding, e.g., `{foo}` in `import {foo} from "mod"`. If `imported` is a `Literal`, `imported.value` must be a string without lone surrogate (ES2022+).

#### ImportDefaultSpecifier

```js
interface ImportDefaultSpecifier <: ModuleSpecifier {
    type: "ImportDefaultSpecifier";
}
```

A default import specifier, e.g., `foo` in `import foo from "mod.js"`.

#### ImportNamespaceSpecifier

```js
interface ImportNamespaceSpecifier <: ModuleSpecifier {
    type: "ImportNamespaceSpecifier";
}
```

A namespace import specifier, e.g., `* as foo` in `import * as foo from "mod.js"`.

#### ImportAttribute

```js
interface ImportAttribute <: Node {
    type: "ImportAttribute";
    key: Identifier | Literal;
    value: Literal;
}
```

An import attribute is an object-like key-value pair (ES2025+). The `value` must be a string literal. If `key` is a `Literal`, it must be a string literal.

### Exports

#### ExportNamedDeclaration

```js
interface ExportNamedDeclaration <: ImportOrExportDeclaration {
    type: "ExportNamedDeclaration";
    declaration: Declaration | null;
    specifiers: [ ExportSpecifier ];
    source: Literal | null;
    attributes: [ ImportAttribute ];
}
```

An export named declaration. The `attributes` property must be an empty array when `source` is `null` (ES2025+).

#### ExportSpecifier

```js
interface ExportSpecifier <: ModuleSpecifier {
    type: "ExportSpecifier";
    local: Identifier | Literal;
    exported: Identifier | Literal;
}
```

An exported variable binding. `local` can be `Literal` only if the `source` is not `null` (ES2022+).

#### ExportDefaultDeclaration

```js
interface ExportDefaultDeclaration <: ImportOrExportDeclaration {
    type: "ExportDefaultDeclaration";
    declaration: AnonymousDefaultExportedFunctionDeclaration | FunctionDeclaration | AnonymousDefaultExportedClassDeclaration | ClassDeclaration | Expression;
}
```

An export default declaration, e.g., `export default function () {};`.

#### ExportAllDeclaration

```js
interface ExportAllDeclaration <: ImportOrExportDeclaration {
    type: "ExportAllDeclaration";
    source: Literal;
    exported: Identifier | Literal | null;
    attributes: [ ImportAttribute ];
}
```

An export batch declaration, e.g., `export * from "mod";`. The `exported` property contains an `Identifier` or `Literal` when a different exported name is specified (ES2020+/ES2022+). The `attributes` property is for import attributes (ES2025+).

---

## Decorators

### Decorator

```js
interface Decorator <: Node {
    type: "Decorator";
    expression: Expression;
}
```

A decorator (Stage 3 proposal).

---

## Summary of Features by ES Version

### ES5 (Baseline)
Core AST structure, expressions, statements, functions

### ES2015 (ES6)
- Arrow functions
- Classes
- Template literals
- Destructuring (patterns)
- Modules (import/export)
- Generators (`yield`)
- `let`/`const`
- `for-of` loops
- Spread operator
- `super`

### ES2016
- Exponentiation operator (`**`, `**=`)

### ES2017
- Async functions (`async`/`await`)

### ES2018
- `for-await-of`
- Object spread/rest properties
- Template literal revision (invalid escapes)

### ES2019
- Optional catch binding

### ES2020
- Optional chaining (`?.`)
- Nullish coalescing (`??`)
- `import()` dynamic import
- BigInt
- `export * as ns from`

### ES2021
- Logical assignment operators (`||=`, `&&=`, `??=`)

### ES2022
- Class fields and private members
- Static blocks
- Private fields in `in` checks
- Top-level `await`
- Arbitrary module namespace names

### ES2025
- Import attributes (`with` clause)

### ES2026
- Using declarations (`using`, `await using`)

### Stage 3 Features
- Decorators
- Source phase imports
- Defer import evaluation
