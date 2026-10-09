import type * as T from "@yuku-toolchain/types";

import { LEAD_ARROW, LEAD_EXPORT_DEFAULT, LEAD_NONE, LEAD_STMT, Output } from "./output.js";
import type { Mappings } from "./sourcemap.js";
import {
  CHAR_0,
  CHAR_BACKSLASH,
  CHAR_BACKSPACE,
  CHAR_BACKTICK,
  CHAR_CR,
  CHAR_DOLLAR,
  CHAR_DOUBLE_QUOTE,
  CHAR_EQUALS,
  CHAR_FF,
  CHAR_GT,
  CHAR_LF,
  CHAR_LT,
  CHAR_NUL,
  CHAR_OPEN_BRACE,
  CHAR_OPEN_BRACKET,
  CHAR_OPEN_PAREN,
  CHAR_SINGLE_QUOTE,
  CHAR_SPACE,
  CHAR_TAB,
  CHAR_VT,
  hasLineTerminator,
  isAsciiDigit,
  isBareInteger,
  isIdentifierName,
  isJsdocBody,
  isLoneSurrogateAt,
  isMinimalInteger,
  isSignificantBlockComment,
  isWordOp,
  scriptEscape,
  shortestDecimal,
  stripUnderscores,
  surrogateEscape,
  trimStartSpaceTab,
} from "./utils.js";

type Node = T.Node;
type Comment = T.AttachedComment;

export interface PrintOptions {
  strip: boolean;
  minify: boolean;
  pretty: boolean;
  indent: number;
  quotes: "preserve" | "double" | "single" | "shortest";
  comments: "none" | "all" | "some" | "line" | "block";
}

export interface PrintResult {
  code: string;
  diagnostics: T.Diagnostic[];
}

const PREC_LOWEST = 0;
const PREC_COMMA = 1;
const PREC_ASSIGNMENT = 2;
const PREC_LOGICAL_OR = 3;
const PREC_RELATIONAL = 9;
const PREC_UNARY = 14;
const PREC_POSTFIX = 15;
const PREC_NEW = 16;
const PREC_CALL = 17;
const PREC_GROUPING = 18;

function table<V>(entries: Record<string, V>): Record<string, V | undefined> {
  return Object.setPrototypeOf(entries, null) as Record<string, V | undefined>;
}

const OPERATOR_PRECEDENCE = table({
  "||": 3,
  "??": 3,
  "&&": 4,
  "|": 5,
  "^": 6,
  "&": 7,
  "==": 8,
  "!=": 8,
  "===": 8,
  "!==": 8,
  "<": 9,
  "<=": 9,
  ">": 9,
  ">=": 9,
  in: 9,
  instanceof: 9,
  "<<": 10,
  ">>": 10,
  ">>>": 10,
  "+": 11,
  "-": 11,
  "*": 12,
  "/": 12,
  "%": 12,
  "**": 13,
});

const ASSIGNMENT_OPERATORS = "= += -= *= /= %= **= <<= >>= >>>= |= ^= &= ||= &&= ??=".split(" ");

const LEAD_SPACED_OPERATOR = table(
  Object.fromEntries(
    [...Object.keys(OPERATOR_PRECEDENCE), ...ASSIGNMENT_OPERATORS].map((op) => [op, " " + op]),
  ),
);

const CTX_PREC = 0x1f;
const CTX_NO_IN = 1 << 5;
const CTX_NO_CALL = 1 << 6;
// the next token would change how TypeScript reads a trailing `f<T>`
const CTX_NO_INSTANTIATION = 1 << 7;
const CTX_NO_JSX_TAG = 1 << 8;
const CTX_TAGGED = 1 << 9;
const CTX_DEFER_TRAILING = 1 << 10;
const CTX_NO_DECORATORS = 1 << 11;

const TPREC_TRAILING = 1;
const TPREC_UNION = 2;
const TPREC_INTERSECTION = 3;
const TPREC_OPERATOR = 4;
const TPREC_PRIMARY = 5;

const CHAIN_DEPTH_MAX = 256;

const NO_COMMENTS: Comment[] = [];

const STRING_ESCAPE_SCAN = [
  /[\0\b\t\n\v\f\r\\"\ud800-\udfff]/,
  /[\0\b\t\n\v\f\r\\'\ud800-\udfff]/,
  /[\0\b\t\n\v\f\r\\"<>\ud800-\udfff]/,
  /[\0\b\t\n\v\f\r\\'<>\ud800-\udfff]/,
];
const TEMPLATE_RAW_SCAN = [/\r/, /[\r<>]/];

const TYPE_CONTEXT = new Set([
  "TSTypeAnnotation",
  "TSTypeReference",
  "TSQualifiedName",
  "TSTypeQuery",
  "TSImportType",
  "TSTypeParameter",
  "TSTypeParameterDeclaration",
  "TSTypeParameterInstantiation",
  "TSLiteralType",
  "TSTemplateLiteralType",
  "TSArrayType",
  "TSIndexedAccessType",
  "TSTupleType",
  "TSNamedTupleMember",
  "TSOptionalType",
  "TSRestType",
  "TSJSDocNullableType",
  "TSJSDocNonNullableType",
  "TSJSDocUnknownType",
  "TSUnionType",
  "TSIntersectionType",
  "TSConditionalType",
  "TSInferType",
  "TSTypeOperator",
  "TSParenthesizedType",
  "TSFunctionType",
  "TSConstructorType",
  "TSTypePredicate",
  "TSTypeLiteral",
  "TSPropertySignature",
  "TSMethodSignature",
  "TSCallSignatureDeclaration",
  "TSConstructSignatureDeclaration",
  "TSIndexSignature",
  "TSMappedType",
  "TSClassImplements",
  "TSInterfaceHeritage",
  "TSInterfaceBody",
]);

const FIXED_STRING = table({
  Super: "super",
  TSThisType: "this",
  TSNullKeyword: "null",
  TSAnyKeyword: "any",
  TSUnknownKeyword: "unknown",
  TSNeverKeyword: "never",
  TSVoidKeyword: "void",
  TSUndefinedKeyword: "undefined",
  TSStringKeyword: "string",
  TSNumberKeyword: "number",
  TSBigIntKeyword: "bigint",
  TSBooleanKeyword: "boolean",
  TSSymbolKeyword: "symbol",
  TSObjectKeyword: "object",
  TSIntrinsicKeyword: "intrinsic",
  TSJSDocUnknownType: "?",
});

interface Link {
  node: Node;
  inner: number;
  wrap: boolean;
  comments: Comment[] | null;
  // a cast strip removes, open for its comments
  stripped: boolean;
  deferTrailing: boolean;
}

export function print(
  program: T.Program,
  options: PrintOptions,
  mappings: Mappings | null,
): PrintResult {
  const printer = new Printer(options, mappings);
  printer.emit(program);
  return { code: printer.finish(), diagnostics: printer.diagnostics };
}

class Printer extends Output {
  readonly strip: boolean;
  readonly minify: boolean;
  readonly indentWidth: number;
  readonly quotes: PrintOptions["quotes"];
  readonly comments: PrintOptions["comments"];
  diagnostics: T.Diagnostic[] = [];

  indentDepth = 0;
  pendingSemi = false;
  skipLeadingOf: Node | null = null;
  deferTrailingOf: Node | null = null;
  inAssignTarget = false;
  inPrologue = false;
  declNoIn = false;
  definitePending = false;
  functionBody = false;
  bindingProperty = false;
  restrictedLen = -1;
  restrictedOpened = false;
  // the node whose trailing comments follow its parent's next token
  owed: Node | null = null;

  chainDepth = 0;
  links: Link[] = [];

  constructor(options: PrintOptions, mappings: Mappings | null) {
    super(options.pretty, mappings);
    this.strip = options.strip;
    this.minify = options.minify;
    this.indentWidth = options.indent;
    this.quotes = options.quotes;
    this.comments = options.comments;
  }

  newline(): void {
    if (this.pretty) this.breakLine();
  }

  breakLine(): void {
    if (this.restrictedArmed()) this.openRestrictedParen();
    this.endLine(this.indentDepth * this.indentWidth);
  }

  printEq(): void {
    this.writeSpaced(" =", "=");
    this.space();
  }

  diagnose(node: Node, message: string): void {
    this.diagnostics.push({
      severity: "error",
      message,
      path: null,
      start: node.start,
      end: node.end,
      labels: [],
      help: null,
    });
  }

  emit(node: Node | null | undefined): void {
    this.emitExpr(node, 0);
  }

  emitStmt(node: Node): void {
    if (this.strip && this.stripsToNothing(node)) {
      this.emitNothing(node);
      this.writeToken(";");
    } else {
      this.emit(node);
    }
  }

  emitNothing(node: Node): void {
    // the item's own mapping would land on whatever prints next
    const mapStart = this.mapStart;
    this.emit(node);
    this.mapStart = mapStart;
  }

  stripsToNothing(node: Node | null | undefined): boolean {
    if (node == null) return true;
    if (TYPE_CONTEXT.has(node.type)) return true;
    switch (node.type) {
      case "TSTypeAliasDeclaration":
      case "TSInterfaceDeclaration":
      case "TSNamespaceExportDeclaration":
      case "TSEnumDeclaration":
      case "TSModuleDeclaration":
      case "TSImportEqualsDeclaration":
      case "TSExportAssignment":
        return true;
      // its comments alone would leave a bare `,`
      case "ImportSpecifier":
        if (node.importKind === "type") return true;
        break;
      case "ExportSpecifier":
        if (node.exportKind === "type") return true;
        break;
    }
    if (this.hasPrintedComments(node)) return false;
    switch (node.type) {
      case "VariableDeclaration":
        return isAmbient(node);
      case "FunctionDeclaration":
      case "FunctionExpression":
      case "ClassDeclaration":
      case "ClassExpression":
      case "PropertyDefinition":
      case "AccessorProperty":
        return node.declare === true;
      case "TSDeclareFunction":
      case "TSEmptyBodyFunctionExpression":
      case "TSAbstractMethodDefinition":
      case "TSAbstractPropertyDefinition":
      case "TSAbstractAccessorProperty":
        return true;
      case "MethodDefinition":
        return node.value.body == null;
      case "ImportDeclaration":
        return (
          node.importKind === "type" ||
          (node.specifiers.length > 0 && !hasValueImportSpecifier(node.specifiers))
        );
      case "ExportNamedDeclaration":
        if (node.declaration != null) {
          return node.exportKind === "type" || this.stripsToNothing(node.declaration);
        }
        return (
          node.exportKind === "type" ||
          (node.specifiers.length > 0 && !hasValueExportSpecifier(node.specifiers))
        );
      case "ExportDefaultDeclaration":
        return isDeclaration(node.declaration) && this.stripsToNothing(node.declaration);
      case "ExportAllDeclaration":
        return node.exportKind === "type";
    }
    return false;
  }

  hasPrintedComments(node: Node): boolean {
    if (this.comments === "none") return false;
    const list = node.comments;
    if (list == null) return false;
    for (const c of list) {
      const printed =
        c.position === "before" ? node !== this.skipLeadingOf : c.position === "after";
      if (printed && this.allowComment(c)) return true;
    }
    return false;
  }

  printStmtList(items: Node[], prologue: boolean): void {
    let first = true;
    let prol = prologue;
    for (const s of items) {
      if (this.strip && this.stripsToNothing(s)) {
        this.emitNothing(s);
        continue;
      }
      if (!first) this.newline();
      this.flushSemi();
      this.inPrologue = prol;
      this.emit(s);
      first = false;
      if (prol && !isDirective(s)) prol = false;
    }
    this.inPrologue = false;
  }

  printIndentedStmtList(items: Node[], prologue: boolean): boolean {
    const prints = !this.strip || this.anyPrints(items);
    this.indentDepth++;
    if (prints) this.newline();
    this.printStmtList(items, prologue);
    this.indentDepth--;
    return prints;
  }

  anyPrints(items: Node[]): boolean {
    for (const s of items) if (!this.stripsToNothing(s)) return true;
    return false;
  }

  printBlock(items: Node[], prologue: boolean, host: Node): void {
    this.writeToken("{");
    if (items.length > 0) {
      if (this.printIndentedStmtList(items, prologue)) {
        this.pendingSemi = false;
        this.newline();
      }
    } else if (this.comments !== "none") {
      this.emitInsideComments(host);
    }
    this.writeToken("}");
  }

  emitInsideComments(host: Node): void {
    let any = false;
    for (const c of host.comments ?? NO_COMMENTS) {
      if (c.position !== "inside" || !this.allowComment(c)) continue;
      if (!any) {
        any = true;
        this.indentDepth++;
      }
      this.breakLine();
      this.writeCommentBody(c);
    }
    if (any) {
      this.indentDepth--;
      this.breakLine();
    }
  }

  // a line comment breaks the node open like a block
  emitInsideCommentsInline(host: Node): void {
    if (this.comments === "none") return;
    const list = host.comments;
    if (list == null) return;
    let any = false;
    for (const c of list) {
      if (c.position !== "inside" || !this.allowComment(c)) continue;
      if (c.type === "Line") return this.emitInsideComments(host);
      any = true;
    }
    if (!any) return;
    for (const c of list) {
      if (c.position !== "inside" || !this.allowComment(c)) continue;
      if (this.pretty && needsSpaceBeforeInlineComment(this.lastByte())) this.writeToken(" ");
      this.writeCommentBody(c);
    }
  }

  // compact mode defers `;` so a closing `}` can drop it
  softSemi(): void {
    if (this.pretty) this.writeToken(";");
    else this.pendingSemi = true;
  }

  flushSemi(): void {
    if (this.pendingSemi) {
      this.pendingSemi = false;
      this.writeToken(";");
    }
  }

  emitExpr(node: Node | null | undefined, ctx: number): void {
    if (node == null) return;
    const type = node.type;
    if (this.strip && this.emitStrippedNode(node, ctx)) return;

    if (type === "Identifier" && !this.definitePending && isPlainIdentifier(node)) {
      if (this.comments === "none" || !hasComments(node)) {
        this.recordMapping(node);
        this.writeName(node.name);
        return;
      }
    }

    const wrap = this.needsParens(node, type, ctx);
    const inner = wrap ? 0 : ctx & ~CTX_DEFER_TRAILING;
    if (wrap) this.writeToken("(");
    const list = this.openComments(node);
    this.emitNode(node, type, inner);
    this.closeComments(node, list, (ctx & CTX_DEFER_TRAILING) !== 0);
    if (wrap) this.writeToken(")");
  }

  emitStrippedNode(node: Node, ctx: number): boolean {
    if (TYPE_CONTEXT.has(node.type)) return true;
    switch (node.type) {
      case "TSTypeAliasDeclaration":
      case "TSInterfaceDeclaration":
      case "TSNamespaceExportDeclaration":
        return true;
      case "ImportSpecifier":
        return node.importKind === "type";
      case "ExportSpecifier":
        return node.exportKind === "type";
      case "TSAsExpression":
      case "TSSatisfiesExpression":
      case "TSTypeAssertion":
      case "TSNonNullExpression":
      case "TSInstantiationExpression":
        this.emitStrippedOperand(node, this.stripped(node), ctx);
        return true;
      case "TSEnumDeclaration":
        if (!node.declare) {
          this.diagnose(node, "TypeScript enums cannot be stripped to JavaScript");
        }
        return true;
      case "TSModuleDeclaration":
        if (node.global) return true;
        if (!node.declare) {
          this.diagnose(node, "TypeScript namespaces cannot be stripped to JavaScript");
        }
        return true;
      case "TSImportEqualsDeclaration":
        if (node.importKind !== "type") {
          this.diagnose(node, "`import = require()` cannot be stripped to JavaScript");
        }
        return true;
      case "TSExportAssignment":
        this.diagnose(node, "`export =` cannot be stripped to JavaScript");
        return true;
      case "TSParameterProperty":
        this.diagnose(node, "parameter properties cannot be stripped to JavaScript");
        this.emitStrippedOperand(node, node.parameter, ctx);
        return true;
    }
    return false;
  }

  precedenceOf(node: Node, type: string): number {
    switch (type) {
      case "MemberExpression":
      case "CallExpression":
        return PREC_CALL;
      case "Literal": {
        const value: unknown = (node as T.Literal).value;
        // minify's `!0` ranks as unary
        if (this.minify && typeof value === "boolean") return PREC_UNARY;
        const raw: unknown = (node as T.Literal).raw;
        // a negative number without a raw lexeme prints as a negation
        if (typeof raw !== "string" && isNegativeNumber(value)) return PREC_UNARY;
        return PREC_GROUPING;
      }
      case "BinaryExpression":
      case "LogicalExpression":
        return OPERATOR_PRECEDENCE[(node as T.BinaryExpression).operator]!;
      case "UnaryExpression":
        return PREC_UNARY;
      case "AssignmentExpression":
      case "ConditionalExpression":
      case "ArrowFunctionExpression":
        return PREC_ASSIGNMENT;
      case "NewExpression":
        return PREC_CALL;
      case "UpdateExpression":
        return PREC_POSTFIX;
      case "SequenceExpression":
        return PREC_COMMA;
      case "YieldExpression":
        return PREC_ASSIGNMENT;
      case "AwaitExpression":
      case "TSTypeAssertion":
        return PREC_UNARY;
      case "TSAsExpression":
      case "TSSatisfiesExpression":
        return PREC_RELATIONAL;
      case "ChainExpression":
      case "TaggedTemplateExpression":
      case "ImportExpression":
      case "TSNonNullExpression":
      case "TSInstantiationExpression":
        return PREC_CALL;
    }
    return PREC_GROUPING;
  }

  needsParens(node: Node, type: string, ctx: number): boolean {
    const prec = ctx & CTX_PREC;
    if (this.lead !== LEAD_NONE && this.leadNeedsParens(node, type)) return true;
    const flags = CTX_NO_CALL | CTX_NO_IN | CTX_NO_INSTANTIATION;
    if ((ctx & flags) !== 0 && this.flagNeedsParens(node, type, ctx)) {
      return true;
    }
    if (prec <= PREC_ASSIGNMENT) return prec === PREC_ASSIGNMENT && type === "SequenceExpression";
    if (prec >= PREC_CALL && type === "ChainExpression") return true;
    return this.precedenceOf(node, type) < prec;
  }

  leadNeedsParens(node: Node, type: string): boolean {
    if (this.lead === LEAD_STMT || this.lead === LEAD_ARROW) {
      if (type === "ObjectExpression") return true;
      if (
        type === "AssignmentExpression" &&
        (node as T.AssignmentExpression).left.type === "ObjectPattern"
      )
        return true;
    }
    if (this.lead === LEAD_STMT || this.lead === LEAD_EXPORT_DEFAULT) {
      switch (type) {
        case "FunctionExpression":
        case "TSEmptyBodyFunctionExpression":
        case "ClassExpression":
          return true;
        case "MemberExpression": {
          const member = node as T.MemberExpression;
          return member.computed && isNamed(member.object, "let");
        }
      }
    }
    return false;
  }

  flagNeedsParens(node: Node, type: string, ctx: number): boolean {
    if ((ctx & CTX_NO_CALL) !== 0) {
      switch (type) {
        case "CallExpression":
        case "ImportExpression":
        case "ChainExpression":
          return true;
      }
    }
    if ((ctx & CTX_NO_INSTANTIATION) !== 0 && type === "TSInstantiationExpression") return true;
    return (
      (ctx & CTX_NO_IN) !== 0 &&
      type === "BinaryExpression" &&
      (node as T.BinaryExpression).operator === "in"
    );
  }

  stripped(node: Node): Node {
    if (!this.strip) return node;
    let n = node;
    for (;;) {
      switch (n.type) {
        case "TSAsExpression":
        case "TSSatisfiesExpression":
        case "TSNonNullExpression":
        case "TSInstantiationExpression":
        case "TSTypeAssertion":
          n = n.expression;
          continue;
      }
      return n;
    }
  }

  emitNode(node: Node, type: string, ctx: number): void {
    this.spillWhenFull();
    this.recordMapping(node);
    switch (type) {
      case "MemberExpression": {
        const e = node as T.MemberExpression;
        this.emitLinkHead(e.object, PREC_CALL | (ctx & CTX_NO_CALL) | CTX_NO_INSTANTIATION);
        return this.emitMemberSuffix(e);
      }
      case "Literal":
        return this.emitLiteral(node as T.Literal);
      case "CallExpression": {
        const e = node as T.CallExpression;
        this.emitLinkHead(e.callee, PREC_CALL | (e.optional ? 0 : CTX_NO_INSTANTIATION));
        return this.emitCallSuffix(e);
      }
      case "BlockStatement":
        return this.emitBlockStatement(node as T.BlockStatement);
      case "ExpressionStatement": {
        const s = node as T.ExpressionStatement | T.Directive;
        if (typeof s.directive === "string") return this.emitDirective(s as T.Directive);
        return this.emitExpressionStatement(s as T.ExpressionStatement);
      }
      case "VariableDeclarator":
        return this.emitVariableDeclarator(node as T.VariableDeclarator);
      case "VariableDeclaration":
        return this.emitVariableDeclaration(node as T.VariableDeclaration);
      case "BinaryExpression": {
        const e = node as T.BinaryExpression;
        const headCtx =
          binaryLeftPrecedence(e) |
          (ctx & CTX_NO_IN) |
          (canFollowTypeArguments(e.operator) ? 0 : CTX_NO_INSTANTIATION);
        this.emitLinkHead(e.left as Node, headCtx);
        return this.emitBinarySuffix(e, ctx);
      }
      case "ReturnStatement":
        return this.emitReturnStatement(node as T.ReturnStatement);
      case "Property":
        if (this.bindingProperty) {
          this.bindingProperty = false;
          return this.emitBindingProperty(node as T.BindingProperty);
        }
        return this.emitProperty(node as T.ObjectProperty);
      case "IfStatement":
        return this.emitIfStatement(node as T.IfStatement);
      case "UnaryExpression":
        return this.emitUnaryExpression(node as T.UnaryExpression, ctx);
      case "LogicalExpression": {
        const e = node as T.LogicalExpression;
        const headCtx =
          this.logicalOperandPrecedence(e.left, OPERATOR_PRECEDENCE[e.operator]!, e.operator) |
          (ctx & CTX_NO_IN);
        this.emitLinkHead(e.left, headCtx);
        return this.emitLogicalSuffix(e, ctx);
      }
      case "AssignmentExpression":
        return this.emitAssignmentExpression(node as T.AssignmentExpression, ctx);
      case "Identifier":
        return this.emitIdentifier(node as T.Identifier);
      case "FunctionDeclaration":
      case "FunctionExpression":
        return this.emitFunction(node as T.Function);
      case "ArrowFunctionExpression":
        return this.emitArrowFunctionExpression(node as T.ArrowFunctionExpression, ctx);
      case "SwitchCase":
        return this.emitSwitchCase(node as T.SwitchCase);
      case "ConditionalExpression":
        return this.emitConditionalExpression(node as T.ConditionalExpression, ctx);
      case "ThisExpression":
        return this.writeToken("this");
      case "ArrayExpression":
        return this.emitArrayExpression(node as T.ArrayExpression);
      case "ObjectExpression":
        return this.emitObjectExpression(node as T.ObjectExpression);
    }
    this.emitRareNode(node, ctx);
  }

  emitRareNode(node: Node, ctx: number): void {
    switch (node.type) {
      case "ChainExpression":
      case "TaggedTemplateExpression":
      case "TSAsExpression":
      case "TSSatisfiesExpression":
      case "TSNonNullExpression":
      case "TSInstantiationExpression":
        return this.emitLink(node, ctx);
      case "TSDeclareFunction":
      case "TSEmptyBodyFunctionExpression":
        return this.emitFunction(node);
      case "ParenthesizedExpression":
        this.writeToken("(");
        this.emit(node.expression);
        return this.writeToken(")");
      case "NewExpression":
        return this.emitNewExpression(node);
      case "TemplateLiteral":
        return this.emitTemplateLiteral(node, ctx);
      case "UpdateExpression":
        return this.emitUpdateExpression(node);
      case "SequenceExpression":
        return this.emitSequenceExpression(node, ctx);
      case "ThrowStatement":
        return this.emitThrowStatement(node);
      case "ForStatement":
        return this.emitForStatement(node);
      case "ForInStatement":
        return this.emitForInStatement(node);
      case "ForOfStatement":
        return this.emitForOfStatement(node);
      case "WhileStatement":
        return this.emitWhileStatement(node);
      case "DoWhileStatement":
        return this.emitDoWhileStatement(node);
      case "BreakStatement":
        return this.printJump("break", node.label, node);
      case "ContinueStatement":
        return this.printJump("continue", node.label, node);
      case "SwitchStatement":
        return this.emitSwitchStatement(node);
      case "TryStatement":
        return this.emitTryStatement(node);
      case "CatchClause":
        return this.emitCatchClause(node);
      case "LabeledStatement":
        return this.emitLabeledStatement(node);
      case "EmptyStatement":
        // not deferred, `if(x);` needs the `;` to materialize the body
        return this.writeToken(";");
      case "DebuggerStatement":
        this.writeToken("debugger");
        this.emitInsideCommentsInline(node);
        return this.softSemi();
      case "WithStatement":
        return this.emitWithStatement(node);
      case "SpreadElement":
        this.writeToken("...");
        return this.emitValue(node.argument);
      case "AwaitExpression":
        this.writeKeyword("await");
        return this.emitExpr(node.argument, PREC_UNARY | (ctx & CTX_NO_INSTANTIATION));
      case "YieldExpression":
        return this.emitYieldExpression(node, ctx);
      case "ImportExpression":
        return this.emitImportExpression(node);
      case "MetaProperty":
        this.emit(node.meta);
        this.writeToken(".");
        return this.emit(node.property);
      case "PrivateIdentifier":
        this.writeToken("#");
        return this.writeName(node.name);
      case "ClassDeclaration":
      case "ClassExpression":
        return this.emitClass(node, ctx);
      case "ClassBody":
        return this.emitClassBody(node);
      case "MethodDefinition":
      case "TSAbstractMethodDefinition":
        return this.emitMethodDefinition(node);
      case "PropertyDefinition":
      case "AccessorProperty":
      case "TSAbstractPropertyDefinition":
      case "TSAbstractAccessorProperty":
        return this.emitPropertyDefinition(node);
      case "StaticBlock":
        this.writeToken("static");
        this.space();
        return this.printBlock(node.body, false, node);
      case "Decorator":
        return this.emitDecorator(node);
      case "ArrayPattern":
        return this.emitArrayPattern(node);
      case "ObjectPattern":
        return this.emitObjectPattern(node);
      case "AssignmentPattern":
        return this.emitAssignmentPattern(node);
      case "RestElement":
        return this.emitRestElement(node);
      case "Program":
        return this.emitProgram(node);
      case "ImportDeclaration":
        return this.emitImportDeclaration(node);
      case "ImportSpecifier":
        return this.emitImportSpecifier(node);
      case "ImportDefaultSpecifier":
        return this.emit(node.local);
      case "ImportNamespaceSpecifier":
        this.writeKeyword("* as");
        return this.emit(node.local);
      case "ImportAttribute":
        this.emit(node.key);
        this.writeToken(":");
        this.space();
        return this.emit(node.value);
      case "ExportNamedDeclaration":
        return this.emitExportNamedDeclaration(node);
      case "ExportDefaultDeclaration":
        return this.emitExportDefaultDeclaration(node);
      case "ExportAllDeclaration":
        return this.emitExportAllDeclaration(node);
      case "ExportSpecifier":
        return this.emitExportSpecifier(node);
      case "JSXElement":
        this.emit(node.openingElement);
        for (const c of node.children) this.emit(c);
        return this.emit(node.closingElement);
      case "JSXOpeningElement":
        return this.emitJSXOpeningElement(node);
      case "JSXClosingElement":
        this.writeToken("</");
        this.emit(node.name);
        return this.writeToken(">");
      case "JSXFragment":
        this.emit(node.openingFragment);
        for (const c of node.children) this.emit(c);
        return this.emit(node.closingFragment);
      case "JSXIdentifier":
        return this.writeToken(node.name);
      case "JSXNamespacedName":
        this.emit(node.namespace);
        this.writeToken(":");
        return this.emit(node.name);
      case "JSXMemberExpression":
        this.emit(node.object);
        this.writeToken(".");
        return this.emit(node.property);
      case "JSXAttribute":
        return this.emitJSXAttribute(node);
      case "JSXSpreadAttribute":
        return this.printJSXSpread(node.argument);
      case "JSXExpressionContainer":
        this.writeToken("{");
        this.emitValue(node.expression);
        return this.writeToken("}");
      case "JSXEmptyExpression":
        return this.emitInsideCommentsInline(node);
      case "JSXOpeningFragment":
        this.writeToken("<");
        this.emitInsideCommentsInline(node);
        return this.writeToken(">");
      case "JSXClosingFragment":
        this.writeToken("</");
        this.emitInsideCommentsInline(node);
        return this.writeToken(">");
      case "JSXText":
        return this.writeLiteral(node.raw || node.value);
      case "JSXSpreadChild":
        return this.printJSXSpread(node.expression);
    }
    const fixed = FIXED_STRING[node.type];
    if (fixed !== undefined) return this.writeToken(fixed);
    return this.emitTypeScript(node, ctx);
  }

  emitLink(node: Node, ctx: number): void {
    this.emitLinkHead(this.linkHead(node), this.linkHeadCtx(node, ctx));
    this.emitLinkSuffix(node, ctx);
  }

  emitLinkHead(head: Node, headCtx: number): void {
    if (this.chainDepth < CHAIN_DEPTH_MAX) {
      this.chainDepth++;
      this.emitExpr(head, headCtx);
      this.chainDepth--;
    } else {
      this.emitChainIteratively(head, headCtx);
    }
  }

  emitChainIteratively(head: Node, headCtx: number): void {
    const first = this.stripped(head);
    if (!isChainLink(first)) return this.emitExpr(head, headCtx);

    const links = this.links;
    const base = links.length;
    this.openStrippedCasts(head, first, false);
    let link = this.openLink(first, headCtx);
    for (;;) {
      const nextHead = this.linkHead(link.node);
      const nextCtx = this.linkHeadCtx(link.node, link.inner);
      const next = this.stripped(nextHead);
      if (!isChainLink(next)) {
        this.emitExpr(nextHead, nextCtx);
        break;
      }
      links.push(link);
      this.openStrippedCasts(nextHead, next, false);
      link = this.openLink(next, nextCtx);
    }
    for (;;) {
      this.spillWhenFull();
      if (!link.stripped) this.emitLinkSuffix(link.node, link.inner);
      this.closeLink(link);
      if (links.length === base) break;
      link = links.pop()!;
    }
  }

  openLink(node: Node, ctx: number): Link {
    const wrap = this.needsParens(node, node.type, ctx);
    if (wrap) this.writeToken("(");
    const comments = this.openComments(node);
    this.recordMapping(node);
    return {
      node,
      inner: wrap ? 0 : ctx & ~CTX_DEFER_TRAILING,
      wrap,
      comments,
      stripped: false,
      deferTrailing: (ctx & CTX_DEFER_TRAILING) !== 0,
    };
  }

  closeLink(link: Link): void {
    this.closeComments(link.node, link.comments, link.deferTrailing);
    if (link.wrap) this.writeToken(")");
  }

  // only the outermost cast defers, so the comments print in source order
  openStrippedCasts(outer: Node, operand: Node, deferTrailing: boolean): boolean {
    if (this.comments === "none") return false;
    let opened = false;
    for (let cast = outer; cast !== operand; cast = strippedOperand(cast)) {
      const comments = this.openComments(cast);
      if (comments === null) continue;
      this.links.push({
        node: cast,
        inner: 0,
        wrap: false,
        comments,
        stripped: true,
        deferTrailing: deferTrailing && !opened,
      });
      opened = true;
    }
    return opened;
  }

  emitStrippedOperand(outer: Node, operand: Node, ctx: number): void {
    const links = this.links;
    const base = links.length;
    const opened = this.openStrippedCasts(outer, operand, (ctx & CTX_DEFER_TRAILING) !== 0);
    this.emitExpr(operand, opened ? ctx & ~CTX_DEFER_TRAILING : ctx);
    while (links.length > base) this.closeLink(links.pop()!);
  }

  linkHead(node: Node): Node {
    switch (node.type) {
      case "BinaryExpression":
      case "LogicalExpression":
        return node.left as Node;
      case "MemberExpression":
        return node.object;
      case "CallExpression":
        return node.callee;
      case "TaggedTemplateExpression":
        return node.tag;
      case "ChainExpression":
      case "TSNonNullExpression":
      case "TSInstantiationExpression":
      case "TSAsExpression":
      case "TSSatisfiesExpression":
        return node.expression;
    }
    throw new Error("not a chain link: " + node.type);
  }

  linkHeadCtx(node: Node, ctx: number): number {
    switch (node.type) {
      case "BinaryExpression":
        return (
          binaryLeftPrecedence(node) |
          (ctx & CTX_NO_IN) |
          (canFollowTypeArguments(node.operator) ? 0 : CTX_NO_INSTANTIATION)
        );
      case "LogicalExpression":
        return (
          this.logicalOperandPrecedence(
            node.left,
            OPERATOR_PRECEDENCE[node.operator]!,
            node.operator,
          ) |
          (ctx & CTX_NO_IN)
        );
      // TypeScript rejects `f<T>?.x`, and minify prints `?.["x"]` as `?.x`
      case "MemberExpression":
      case "TaggedTemplateExpression":
        return PREC_CALL | (ctx & CTX_NO_CALL) | CTX_NO_INSTANTIATION;
      // `f<T>?.()` keeps its type arguments where `f<T>()` takes them as the call's own
      case "CallExpression":
        return PREC_CALL | (node.optional ? 0 : CTX_NO_INSTANTIATION);
      case "ChainExpression":
        return ctx;
      case "TSNonNullExpression":
      case "TSInstantiationExpression":
        return PREC_POSTFIX | CTX_NO_INSTANTIATION;
      case "TSAsExpression":
      case "TSSatisfiesExpression":
        return PREC_RELATIONAL | (ctx & CTX_NO_IN) | CTX_DEFER_TRAILING;
    }
    throw new Error("not a chain link: " + node.type);
  }

  emitLinkSuffix(node: Node, ctx: number): void {
    switch (node.type) {
      case "BinaryExpression":
        return this.emitBinarySuffix(node, ctx);
      case "LogicalExpression":
        return this.emitLogicalSuffix(node, ctx);
      case "MemberExpression":
        return this.emitMemberSuffix(node);
      case "CallExpression":
        return this.emitCallSuffix(node);
      case "TaggedTemplateExpression":
        this.emit(node.typeArguments);
        return this.emitExpr(node.quasi, CTX_TAGGED);
      case "ChainExpression":
        return;
      case "TSNonNullExpression":
        return this.writeToken("!");
      case "TSInstantiationExpression":
        return this.emit(node.typeArguments);
      case "TSAsExpression":
        this.writeKeywordThenOwed(" as");
        return this.emit(node.typeAnnotation);
      case "TSSatisfiesExpression":
        this.writeKeywordThenOwed(" satisfies");
        return this.emit(node.typeAnnotation);
    }
    throw new Error("not a chain link: " + node.type);
  }

  emitBinarySuffix(e: T.BinaryExpression, ctx: number): void {
    const op = e.operator;
    const p = OPERATOR_PRECEDENCE[op]!;
    if (isWordOp(op)) {
      this.writeToken(" ");
      this.writeKeyword(op);
    } else if (this.pretty) {
      this.writeToken(LEAD_SPACED_OPERATOR[op]!);
      this.space();
    } else {
      const first = op.charCodeAt(0);
      const extendsAngle = first === CHAR_GT || first === CHAR_EQUALS;
      // `f<T>==x` would re-lex the type argument closer into `>=`
      if (extendsAngle && this.lastByte() === CHAR_GT) this.writeToken(" ");
      this.writeToken(op);
    }
    this.emitExpr(e.right, (op === "**" ? p : p + 1) | (ctx & (CTX_NO_IN | CTX_NO_INSTANTIATION)));
  }

  emitLogicalSuffix(e: T.LogicalExpression, ctx: number): void {
    const p = OPERATOR_PRECEDENCE[e.operator]!;
    this.writeSpaced(LEAD_SPACED_OPERATOR[e.operator]!, e.operator);
    this.space();
    const prec = this.logicalOperandPrecedence(e.right, p + 1, e.operator);
    this.emitExpr(e.right, prec | (ctx & CTX_NO_IN));
  }

  emitMemberSuffix(e: T.MemberExpression): void {
    const staticKey = this.minify && e.computed ? simpleStringKey(e.property) : null;
    if (e.computed && staticKey === null) {
      if (e.optional) this.writeToken("?.");
      this.writeToken("[");
      this.emit(e.property);
      this.writeToken("]");
      return;
    }
    this.writeToken(e.optional ? "?." : ".");
    if (staticKey !== null) this.writeNodeText(e.property, staticKey);
    else this.emit(e.property);
  }

  emitCallSuffix(e: T.CallExpression): void {
    if (e.optional) this.writeToken("?.");
    this.emit(e.typeArguments);
    this.printArgList(e.arguments);
  }

  logicalOperandPrecedence(child: Node, min: number, parent: string): number {
    return logicalMismatch(parent, this.stripped(child)) ? PREC_GROUPING : min;
  }

  allowComment(c: Comment): boolean {
    switch (this.comments) {
      case "none":
        return false;
      case "all":
        return true;
      case "some":
        return c.type === "Block" && isSignificantBlockComment(c.value);
      case "line":
        return c.type === "Line";
      case "block":
        return c.type === "Block";
    }
  }

  emitLeadingComments(node: Node, list: Comment[]): void {
    if (node === this.skipLeadingOf) return;
    for (const c of list) {
      if (c.position === "before" && this.allowComment(c)) this.writeLeading(c);
    }
  }

  // a comment between `get`/`async` and its key would split them
  hoistKeyComments(key: Node | null | undefined): void {
    if (this.comments === "none") return;
    if (key == null) return;
    this.emitLeadingComments(key, key.comments ?? NO_COMMENTS);
    this.skipLeadingOf = key;
  }

  emitTrailingComments(list: Comment[]): void {
    for (const c of list) {
      if (c.position === "after" && this.allowComment(c)) {
        // a deferred `;` landing after the comment would re-home it on reparse
        this.flushSemi();
        this.writeTrailing(c);
      }
    }
  }

  openComments(node: Node): Comment[] | null {
    if (this.comments === "none") return null;
    const list = node.comments;
    if (list == null || list.length === 0) return null;
    const savedLead = this.lead;
    this.emitLeadingComments(node, list);
    this.lead = savedLead;
    return list;
  }

  closeComments(node: Node, list: Comment[] | null, deferTrailing: boolean): void {
    if (list === null) return;
    if (deferTrailing || node === this.deferTrailingOf) this.owed = node;
    else this.emitTrailingComments(list);
  }

  writeNodeText(node: Node, text: string): void {
    const list = this.openComments(node);
    this.writeToken(text);
    this.closeComments(node, list, false);
  }

  emitOwedComments(): void {
    const owed = this.owed;
    if (owed === null) return;
    this.owed = null;
    this.emitTrailingComments(owed.comments!);
  }

  writeKeywordThenOwed(word: string): void {
    this.writeToken(word);
    this.emitOwedComments();
    if (!this.atLineStart()) this.writeToken(" ");
  }

  writeLeading(c: Comment): void {
    const armed = this.restrictedArmed();
    if (c.type === "Block" && c.sameLine) {
      // a comment spanning lines is a line terminator too
      if (armed && hasLineTerminator(c.value)) this.openRestrictedParen();
      const last = this.lastByte();
      if (this.pretty && last !== 0 && last !== CHAR_SPACE && last !== CHAR_LF) {
        this.writeToken(" ");
      }
      this.writeCommentBody(c);
      if (this.pretty) this.writeToken(" ");
    } else {
      this.breakLine();
      this.writeCommentBody(c);
      this.breakLine();
    }
    // a comment is not the operand's first token
    if (this.restrictedLen >= 0) this.restrictedLen = this.length();
  }

  writeTrailing(c: Comment): void {
    if (c.sameLine) {
      if (this.pretty) this.writeToken(" ");
      this.writeCommentBody(c);
      if (c.type === "Line") this.breakLine();
      return;
    }
    this.breakLine();
    this.writeCommentBody(c);
    this.breakLine();
  }

  writeCommentBody(c: Comment): void {
    if (c.type === "Line") {
      this.writeToken("//");
      this.writeComment(c.value);
      return;
    }
    this.writeToken("/*");
    this.writeBlockBody(c.value);
    this.writeComment("*/");
  }

  writeBlockBody(value: string): void {
    if (!this.pretty || !isJsdocBody(value)) {
      this.writeComment(value);
      return;
    }
    const lines = value.split("\n");
    const head = trimEndCr(lines[0]!);
    if (head.length > 0) this.writeToken(head);
    for (let i = 1; i < lines.length; i++) {
      this.breakLine();
      const line = trimStartSpaceTab(trimEndCr(lines[i]!));
      // a line break alone would collapse a blank line
      if (line.length === 0 && i + 1 < lines.length) {
        this.heldSpaces = 0;
        this.writeComment("\n");
        continue;
      }
      this.writeToken(" ");
      if (line.length > 0) this.writeToken(line);
    }
  }

  emitProgram(p: T.Program): void {
    if (p.hashbang != null) {
      this.writeToken("#!");
      this.writeLiteral(p.hashbang.value);
      this.writeToken("\n");
    }
    this.printStmtList(p.body, true);
    this.pendingSemi = false;
    if (this.comments !== "none") this.emitInsideComments(p);
  }

  emitBlockStatement(s: T.BlockStatement): void {
    const body = this.functionBody;
    this.functionBody = false;
    this.printBlock(s.body, body, s);
  }

  emitDirective(d: T.Directive): void {
    const e = d.expression;
    if (e.type === "Literal" && typeof e.value === "string") {
      // an escaped `"use strict"` is not a directive, so the raw lexeme must survive
      const raw = e.raw;
      const text = typeof raw === "string" && raw.length >= 2 ? raw : quoteVerbatim(d.directive);
      this.writeNodeText(e, text);
    } else {
      this.emit(e);
    }
    this.softSemi();
  }

  emitExpressionStatement(s: T.ExpressionStatement): void {
    const e = s.expression;
    const asDirective = this.inPrologue && e.type === "Literal" && typeof e.value === "string";
    this.inPrologue = false;
    if (asDirective) {
      this.writeToken("(");
      this.emit(e);
      this.writeToken(")");
      this.softSemi();
      return;
    }
    this.lead = LEAD_STMT;
    this.emitExpr(e, 0);
    this.softSemi();
  }

  emitIfStatement(s: T.IfStatement): void {
    this.writeSpaced("if (", "if(");
    this.emit(s.test);
    this.writeToken(")");
    this.space();
    this.emitStmt(s.consequent);
    if (s.alternate != null) {
      this.flushSemi();
      this.space();
      this.writeKeyword("else");
      this.emitStmt(s.alternate);
    }
  }

  emitReturnStatement(s: T.ReturnStatement): void {
    this.writeToken("return");
    this.emitInsideCommentsInline(s);
    this.emitRestrictedArg(s.argument, 0);
    this.softSemi();
  }

  emitThrowStatement(s: T.ThrowStatement): void {
    this.writeToken("throw");
    this.emitRestrictedArg(s.argument, 0);
    this.softSemi();
  }

  // asi would split an operand whose first token follows a line terminator, so that opens a
  // paren the operand closes
  emitRestrictedArg(node: Node | null | undefined, ctx: number): void {
    if (node == null) return;
    this.writeToken(" ");
    const outerOpened = this.restrictedOpened;
    this.restrictedOpened = false;
    this.restrictedLen = this.length();
    this.emitExpr(node, ctx);
    this.restrictedLen = -1;
    const opened = this.restrictedOpened;
    this.restrictedOpened = outerOpened;
    if (opened) this.writeToken(")");
  }

  restrictedArmed(): boolean {
    if (this.restrictedLen < 0) return false;
    if (this.length() === this.restrictedLen) return true;
    this.restrictedLen = -1;
    return false;
  }

  openRestrictedParen(): void {
    this.restrictedOpened = true;
    this.restrictedLen = -1;
    // the keyword's separator space becomes part of ` (`
    this.heldSpaces = 0;
    this.writeToken(" (");
  }

  printJump(keyword: string, label: Node | null | undefined, host: Node): void {
    if (label != null) {
      this.writeKeyword(keyword);
      this.emit(label);
    } else {
      this.writeToken(keyword);
      this.emitInsideCommentsInline(host);
    }
    this.softSemi();
  }

  emitLabeledStatement(s: T.LabeledStatement): void {
    this.emit(s.label);
    this.writeToken(":");
    this.space();
    this.emitStmt(s.body);
  }

  emitWithStatement(s: T.WithStatement): void {
    this.writeSpaced("with (", "with(");
    this.emit(s.object);
    this.writeToken(")");
    this.space();
    this.emitStmt(s.body);
  }

  emitWhileStatement(s: T.WhileStatement): void {
    this.writeSpaced("while (", "while(");
    this.emit(s.test);
    this.writeToken(")");
    this.space();
    this.emitStmt(s.body);
  }

  emitDoWhileStatement(s: T.DoWhileStatement): void {
    this.writeKeyword("do");
    this.emitStmt(s.body);
    this.flushSemi();
    this.writeSpaced(" while (", "while(");
    this.emit(s.test);
    this.writeToken(");");
  }

  emitForStatement(s: T.ForStatement): void {
    this.writeSpaced("for (", "for(");
    const init = s.init;
    if (init != null) {
      if (init.type === "VariableDeclaration") this.printForDeclaration(init, true);
      else this.emitExpr(init, CTX_NO_IN);
    }
    this.writeToken(";");
    if (s.test != null) {
      this.space();
      this.emit(s.test);
    }
    this.writeToken(";");
    if (s.update != null) {
      this.space();
      this.emit(s.update);
    }
    this.writeToken(")");
    this.space();
    this.emitStmt(s.body);
  }

  emitForInStatement(s: T.ForInStatement): void {
    this.writeSpaced("for (", "for(");
    this.printForLeft(s.left);
    this.writeKeyword(" in");
    this.emit(s.right);
    this.writeToken(")");
    this.space();
    this.emitStmt(s.body);
  }

  emitForOfStatement(s: T.ForOfStatement): void {
    this.writeToken("for");
    if (s.await) this.writeToken(" await");
    this.writeSpaced(" (", "(");
    // `for (async of …)` is forbidden, it would read as `for await`
    const wrapAsync = !s.await && isNamed(s.left, "async");
    if (wrapAsync) this.writeToken("(");
    this.printForLeft(s.left);
    if (wrapAsync) this.writeToken(")");
    this.writeKeyword(" of");
    this.emitValue(s.right);
    this.writeToken(")");
    this.space();
    this.emitStmt(s.body);
  }

  printForLeft(left: Node): void {
    if (left.type === "VariableDeclaration") this.printForDeclaration(left, false);
    else this.emitAssignTarget(left, 0);
  }

  printForDeclaration(d: T.VariableDeclaration, noIn: boolean): void {
    const list = this.openComments(d);
    this.printVariableDecl(d, false, noIn);
    this.closeComments(d, list, false);
  }

  emitSwitchStatement(s: T.SwitchStatement): void {
    this.writeSpaced("switch (", "switch(");
    this.emit(s.discriminant);
    this.writeSpaced(") {", "){");
    if (s.cases.length > 0) {
      for (const c of s.cases) {
        this.flushSemi();
        this.newline();
        this.emit(c);
      }
      this.pendingSemi = false;
      this.newline();
    }
    this.writeToken("}");
  }

  emitSwitchCase(c: T.SwitchCase): void {
    if (c.test != null) {
      this.writeKeyword("case");
      this.emit(c.test);
      this.writeToken(":");
    } else {
      this.writeToken("default");
      this.emitInsideCommentsInline(c);
      this.writeToken(":");
    }
    if (c.consequent.length === 0) return;
    this.printIndentedStmtList(c.consequent, false);
  }

  emitTryStatement(s: T.TryStatement): void {
    this.writeKeyword("try");
    this.emit(s.block);
    if (s.handler != null) {
      this.space();
      this.emit(s.handler);
    }
    if (s.finalizer != null) {
      this.space();
      this.writeKeyword("finally");
      this.emit(s.finalizer);
    }
  }

  emitCatchClause(c: T.CatchClause): void {
    this.writeToken("catch");
    if (c.param != null) {
      this.writeSpaced(" (", "(");
      this.emit(c.param);
      this.writeToken(")");
    }
    this.space();
    this.emit(c.body);
  }

  emitVariableDeclaration(d: T.VariableDeclaration): void {
    if (this.strip && isAmbient(d)) return;
    this.printVariableDecl(d, true, false);
  }

  printVariableDecl(d: T.VariableDeclaration, withSemicolon: boolean, noIn: boolean): void {
    if (!this.strip && d.declare) this.writeKeyword("declare");
    this.writeKeyword(d.kind);
    const prev = this.declNoIn;
    this.declNoIn = noIn;
    this.emitList(d.declarations);
    this.declNoIn = prev;
    if (withSemicolon) this.softSemi();
  }

  emitVariableDeclarator(d: T.VariableDeclarator): void {
    if (!this.strip) this.definitePending = d.definite === true;
    this.emit(d.id);
    if (d.init != null) {
      this.printEq();
      this.emitExpr(d.init, PREC_ASSIGNMENT | (this.declNoIn ? CTX_NO_IN : 0));
    }
  }

  emitList(items: readonly (Node | null)[]): void {
    this.emitItems(items, 0);
  }

  emitItems(items: readonly (Node | null)[], ctx: number): void {
    const depth = this.indentDepth;
    for (let i = 0; i < items.length; i++) {
      this.emitExpr(items[i], ctx | CTX_DEFER_TRAILING);
      this.closeItem(i + 1 < items.length, depth);
    }
    this.closeList(depth);
  }

  // an item's separator precedes its trailing comments, so a line comment cannot swallow it
  closeItem(separated: boolean, hangDepth: number): void {
    if (separated) this.writeToken(",");
    this.emitOwedComments();
    if (!separated) return;
    if (hangDepth >= 0 && this.indentDepth === hangDepth && this.atLineStart()) {
      this.indentDepth = hangDepth + 1;
      this.breakLine();
    }
    if (!this.atLineStart()) this.space();
    this.spillWhenFull();
  }

  closeList(depth: number): void {
    if (this.indentDepth === depth) return;
    this.indentDepth = depth;
    if (this.atLineStart()) this.breakLine();
  }

  emitSequenceExpression(e: T.SequenceExpression, ctx: number): void {
    this.emitItems(e.expressions, PREC_ASSIGNMENT | (ctx & CTX_NO_IN));
  }

  emitConditionalExpression(e: T.ConditionalExpression, ctx: number): void {
    const noIn = ctx & CTX_NO_IN;
    this.emitExpr(e.test, PREC_LOGICAL_OR | noIn);
    this.writeSpaced(" ?", "?");
    this.space();
    this.emitExpr(e.consequent, PREC_ASSIGNMENT);
    this.writeSpaced(" :", ":");
    this.space();
    this.emitExpr(e.alternate, PREC_ASSIGNMENT | noIn);
  }

  emitUnaryExpression(e: T.UnaryExpression, ctx: number): void {
    if (isWordOp(e.operator)) this.writeKeyword(e.operator);
    else this.writeToken(e.operator);
    this.emitExpr(e.argument, PREC_UNARY | (ctx & CTX_NO_INSTANTIATION));
  }

  emitUpdateExpression(e: T.UpdateExpression): void {
    if (e.prefix) {
      this.writeToken(e.operator);
      this.emitAssignTarget(e.argument, 0);
    } else {
      this.emitAssignTarget(e.argument, 0);
      this.writeToken(e.operator);
    }
  }

  emitAssignmentExpression(e: T.AssignmentExpression, ctx: number): void {
    this.emitAssignTarget(e.left, 0);
    this.writeSpaced(LEAD_SPACED_OPERATOR[e.operator]!, e.operator);
    this.space();
    this.emitExpr(e.right, PREC_ASSIGNMENT | (ctx & CTX_NO_IN));
  }

  emitAssignTarget(node: Node | null | undefined, ctx: number): void {
    if (node == null) return;
    const prev = this.inAssignTarget;
    if (needsParensAsAssignTarget(node)) {
      this.inAssignTarget = false;
      this.writeToken("(");
      this.emitExpr(node, ctx);
      this.writeToken(")");
    } else {
      this.inAssignTarget = true;
      this.emitExpr(node, ctx);
    }
    this.inAssignTarget = prev;
  }

  emitChildOfAssignTarget(node: Node | null | undefined): void {
    if (this.inAssignTarget) this.emitAssignTarget(node, 0);
    else this.emitValue(node);
  }

  emitValue(node: Node | null | undefined): void {
    this.emitExpr(node, PREC_ASSIGNMENT);
  }

  emitArrayExpression(e: T.ArrayExpression): void {
    const inTarget = this.inAssignTarget;
    this.inAssignTarget = false;
    this.writeToken("[");
    this.emitInsideCommentsInline(e);
    const list = e.elements;
    const depth = this.indentDepth;
    for (let i = 0; i < list.length; i++) {
      if (inTarget) this.emitAssignTarget(list[i], CTX_DEFER_TRAILING);
      else this.emitExpr(list[i], PREC_ASSIGNMENT | CTX_DEFER_TRAILING);
      this.closeItem(i + 1 < list.length, depth);
    }
    this.closeList(depth);
    // a trailing hole needs its own comma, else `[a,]` is one element
    if (list.length > 0 && list[list.length - 1] == null) this.writeToken(",");
    this.writeToken("]");
    this.inAssignTarget = inTarget;
  }

  emitObjectExpression(e: T.ObjectExpression): void {
    const inTarget = this.inAssignTarget;
    this.writeToken("{");
    this.emitInsideCommentsInline(e);
    const list = e.properties;
    if (list.length > 0) {
      this.space();
      const depth = this.indentDepth;
      for (let i = 0; i < list.length; i++) {
        this.inAssignTarget = inTarget;
        this.emitExpr(list[i], CTX_DEFER_TRAILING);
        this.closeItem(i + 1 < list.length, depth);
      }
      this.closeList(depth);
      if (!this.atLineStart()) this.space();
    }
    this.writeToken("}");
    this.inAssignTarget = inTarget;
  }

  emitProperty(p: T.ObjectProperty): void {
    if (p.method || p.kind === "get" || p.kind === "set") {
      const fn = p.value as T.Function;
      switch (p.kind) {
        case "get":
          this.writeKeyword("get");
          break;
        case "set":
          this.writeKeyword("set");
          break;
        case "init":
          if (fn.async) this.writeKeyword("async");
          if (fn.generator) this.writeToken("*");
          break;
      }
      this.printObjectKey(p.key, p.computed);
      this.emitMethodValue(fn);
      return;
    }

    if (p.shorthand && shorthandStillValid(p.key, p.value)) {
      this.emitChildOfAssignTarget(p.value);
      return;
    }

    this.printObjectKey(p.key, p.computed);
    this.writeToken(":");
    this.space();
    this.emitChildOfAssignTarget(p.value);
  }

  emitBindingProperty(p: T.BindingProperty): void {
    if (p.shorthand && shorthandStillValid(p.key, p.value)) {
      this.emitAssignTarget(p.value, 0);
      return;
    }
    this.printObjectKey(p.key, p.computed);
    this.writeToken(":");
    this.space();
    this.emitAssignTarget(p.value, 0);
  }

  emitNewExpression(e: T.NewExpression): void {
    this.writeKeyword("new");
    this.emitExpr(e.callee, PREC_NEW | CTX_NO_CALL | CTX_NO_INSTANTIATION);
    this.emit(e.typeArguments);
    this.printArgList(e.arguments);
  }

  emitYieldExpression(e: T.YieldExpression, ctx: number): void {
    this.writeToken("yield");
    if (e.delegate) this.writeToken("*");
    this.emitRestrictedArg(e.argument, PREC_ASSIGNMENT | (ctx & CTX_NO_IN));
  }

  printArgList(args: readonly Node[]): void {
    this.writeToken("(");
    this.emitItems(args, PREC_ASSIGNMENT);
    this.writeToken(")");
  }

  emitImportExpression(e: T.ImportExpression): void {
    this.writeToken("import");
    if (e.phase != null) {
      this.writeToken(".");
      this.writeToken(e.phase);
    }
    this.writeToken("(");
    this.emitValue(e.source);
    if (e.options != null) {
      this.writeToken(",");
      this.space();
      this.emitValue(e.options);
    }
    this.writeToken(")");
  }

  emitLiteral(lit: T.Literal): void {
    const value = lit.value;
    switch (typeof value) {
      case "string":
        return this.emitStringLiteral(lit as T.StringLiteral);
      case "number":
        return this.emitNumericLiteral(lit as T.NumericLiteral);
      case "boolean":
        if (this.minify) return this.writeToken(value ? "!0" : "!1");
        return this.writeToken(value ? "true" : "false");
    }
    if ("regex" in lit && lit.regex != null) return this.emitRegExpLiteral(lit);
    if ("bigint" in lit && typeof lit.bigint === "string") return this.emitBigIntLiteral(lit);
    const raw = lit.raw;
    // a number JSON turned into null, such as `1e999`, still has its raw lexeme
    if (typeof raw === "string" && raw !== "null") return this.writeNumericRaw(raw);
    this.writeToken("null");
  }

  emitStringLiteral(lit: T.StringLiteral): void {
    const raw = typeof lit.raw === "string" ? lit.raw : "";
    const q = this.pickQuote(lit.value, raw.charCodeAt(0) === CHAR_SINGLE_QUOTE);
    this.writeToken(q === CHAR_SINGLE_QUOTE ? "'" : '"');
    this.writeEscapedString(lit.value, q);
    this.writeLiteral(q === CHAR_SINGLE_QUOTE ? "'" : '"');
  }

  pickQuote(s: string, singleQuoted: boolean): number {
    switch (this.quotes) {
      case "preserve":
        return singleQuoted ? CHAR_SINGLE_QUOTE : CHAR_DOUBLE_QUOTE;
      case "single":
        return CHAR_SINGLE_QUOTE;
      case "double":
        return CHAR_DOUBLE_QUOTE;
      case "shortest": {
        if (s.indexOf('"') < 0) return CHAR_DOUBLE_QUOTE;
        let single = 0;
        let double = 0;
        for (let i = 0; i < s.length; i++) {
          const code = s.charCodeAt(i);
          if (code === CHAR_SINGLE_QUOTE) single++;
          else if (code === CHAR_DOUBLE_QUOTE) double++;
        }
        return single < double ? CHAR_SINGLE_QUOTE : CHAR_DOUBLE_QUOTE;
      }
    }
  }

  writeEscapedString(s: string, quote: number): void {
    const variant = (this.minify ? 2 : 0) + (quote === CHAR_SINGLE_QUOTE ? 1 : 0);
    const first = s.search(STRING_ESCAPE_SCAN[variant]!);
    if (first < 0) return this.writeLiteral(s);
    let start = 0;
    for (let i = first; i < s.length; i++) {
      const code = s.charCodeAt(i);
      let escape: string | null = null;
      if (code >= 0x80) {
        if (code < 0xd800 || code > 0xdfff || !isLoneSurrogateAt(s, i)) continue;
        escape = surrogateEscape(code);
      } else if (code >= 0x0e && code !== CHAR_BACKSLASH && code !== quote) {
        if (!this.minify || (code !== CHAR_LT && code !== CHAR_GT)) continue;
        escape = scriptEscape(s, i);
      } else {
        escape = stringEscape(s, i, code, quote);
      }
      if (escape === null) continue;
      if (i > start) this.writeLiteral(s.slice(start, i));
      this.writeLiteral(escape);
      start = i + 1;
    }
    if (start < s.length) this.writeLiteral(start === 0 ? s : s.slice(start));
  }

  emitNumericLiteral(lit: T.NumericLiteral): void {
    if (typeof lit.raw === "string") return this.writeNumericRaw(lit.raw);
    const value = lit.value;
    if (isNegativeNumber(value)) {
      this.writeToken("-");
      this.writeNumericRaw(String(-value));
    } else {
      this.writeNumericRaw(String(value));
    }
  }

  writeNumericRaw(raw: string): void {
    this.writeNumber(this.minify ? shortestNumber(raw) : raw);
  }

  writeNumber(text: string): void {
    this.writeToken(text);
    if (isBareInteger(text)) this.markBareInteger();
  }

  emitBigIntLiteral(lit: T.BigIntLiteral): void {
    const raw =
      typeof lit.raw === "string" && lit.raw.endsWith("n") ? lit.raw.slice(0, -1) : lit.bigint;
    this.writeToken(raw);
    this.writeToken("n");
  }

  emitRegExpLiteral(lit: T.RegExpLiteral): void {
    const pattern = lit.regex.pattern;
    const raw = typeof lit.raw === "string" ? lit.raw : "";
    const slash = raw.lastIndexOf("/");
    // the parser sorts `regex.flags`, the raw lexeme keeps their source order
    const flags = slash > 0 ? raw.slice(slash + 1) : lit.regex.flags;
    this.writeToken("/");
    // an empty pattern would open a line comment
    this.writeLiteral(pattern.length > 0 ? pattern : "(?:)");
    this.writeLiteral("/");
    this.writeLiteral(flags);
  }

  emitTemplateLiteral(lit: T.TemplateLiteral, ctx: number): void {
    this.printTemplate(lit.quasis, lit.expressions, (ctx & CTX_TAGGED) !== 0);
  }

  printTemplate(
    quasis: readonly T.TemplateElement[],
    subs: readonly Node[],
    tagged: boolean,
  ): void {
    this.writeToken("`");
    for (let i = 0; i < quasis.length; i++) {
      this.printTemplateElement(quasis[i]!, tagged);
      if (i < subs.length) {
        this.writeLiteral("${");
        this.emit(subs[i]);
        this.writeToken("}");
      }
    }
    this.writeLiteral("`");
  }

  printTemplateElement(el: T.TemplateElement, tagged: boolean): void {
    const scriptSafe = this.minify && !tagged;
    const raw = el.value.raw;
    if (typeof raw === "string" && raw.length !== 0) {
      return this.writeTemplateRaw(raw, scriptSafe);
    }
    const s = el.value.cooked ?? "";
    let start = 0;
    for (let i = 0; i < s.length; i++) {
      const code = s.charCodeAt(i);
      let escape: string | null = null;
      if (isLoneSurrogateAt(s, i)) {
        escape = surrogateEscape(code);
      } else if (scriptSafe && (code === CHAR_LT || code === CHAR_GT)) {
        escape = scriptEscape(s, i);
      }
      if (escape === null) escape = templateEscape(s, i, code);
      if (escape === null) continue;
      if (i > start) this.writeLiteral(s.slice(start, i));
      this.writeLiteral(escape);
      start = i + 1;
    }
    if (start < s.length) this.writeLiteral(s.slice(start));
  }

  // template text reads a raw `\r\n` or `\r` as `\n`
  writeTemplateRaw(raw: string, scriptSafe: boolean): void {
    const first = raw.search(TEMPLATE_RAW_SCAN[scriptSafe ? 1 : 0]!);
    if (first < 0) return this.writeLiteral(raw);
    let start = 0;
    for (let i = first; i < raw.length; i++) {
      const code = raw.charCodeAt(i);
      let escape: string | null = null;
      if (code === CHAR_CR) {
        escape = "\n";
      } else if (scriptSafe && (code === CHAR_LT || code === CHAR_GT)) {
        escape = scriptEscape(raw, i);
      }
      if (escape === null) continue;
      if (i > start) this.writeLiteral(raw.slice(start, i));
      this.writeLiteral(escape);
      if (code === CHAR_CR && raw.charCodeAt(i + 1) === CHAR_LF) i++;
      start = i + 1;
    }
    this.writeLiteral(start === 0 ? raw : raw.slice(start));
  }

  emitIdentifier(id: T.Identifier): void {
    const definite = this.takeDefinite();
    if (!this.strip && id.decorators != null) this.printDecorators(id.decorators);
    this.writeName(id.name);
    if (id.optional === true) this.emitInsideCommentsInline(id);
    this.printBindingSuffix(id.optional === true, definite, id.typeAnnotation);
  }

  printBindingSuffix(
    optional: boolean,
    definite: boolean,
    annotation: Node | null | undefined,
  ): void {
    if (!this.strip && optional) this.writeToken("?");
    if (definite) this.writeToken("!");
    this.emit(annotation);
  }

  takeDefinite(): boolean {
    if (this.strip) return false;
    const d = this.definitePending;
    this.definitePending = false;
    return d;
  }

  emitAssignmentPattern(p: T.AssignmentPattern): void {
    if (!this.strip && p.decorators != null) this.printDecorators(p.decorators);
    this.emit(p.left);
    if (!this.strip && p.optional === true) this.writeToken("?");
    this.emit(p.typeAnnotation);
    this.printEq();
    this.emitValue(p.right);
  }

  emitRestElement(r: T.RestElement): void {
    if (!this.strip && r.decorators != null) this.printDecorators(r.decorators);
    this.writeToken("...");
    this.emit(r.argument);
    if (!this.strip && r.optional === true) this.writeToken("?");
    this.emit(r.typeAnnotation);
  }

  emitArrayPattern(p: T.ArrayPattern): void {
    const definite = this.takeDefinite();
    if (!this.strip && p.decorators != null) this.printDecorators(p.decorators);
    this.writeToken("[");
    this.emitInsideCommentsInline(p);
    const elements = p.elements;
    const last = elements.length > 0 ? elements[elements.length - 1] : null;
    const rest = last != null && last.type === "RestElement" ? last : null;
    const count = rest === null ? elements.length : elements.length - 1;
    const depth = this.indentDepth;
    for (let i = 0; i < count; i++) {
      this.emitAssignTarget(elements[i], CTX_DEFER_TRAILING);
      this.closeItem(i + 1 < count || rest !== null, depth);
    }
    if (rest !== null) {
      this.emitExpr(rest, CTX_DEFER_TRAILING);
      this.closeItem(false, depth);
    }
    this.closeList(depth);
    if (rest === null && count > 0 && elements[count - 1] == null) {
      // a trailing hole needs its own comma, else `[a,]` is one element
      this.writeToken(",");
    }
    this.writeToken("]");
    this.printBindingSuffix(p.optional === true, definite, p.typeAnnotation);
  }

  emitObjectPattern(p: T.ObjectPattern): void {
    const definite = this.takeDefinite();
    if (!this.strip && p.decorators != null) this.printDecorators(p.decorators);
    this.writeToken("{");
    this.emitInsideCommentsInline(p);
    const props = p.properties;
    const last = props.length > 0 ? props[props.length - 1] : null;
    const rest = last != null && last.type === "RestElement" ? last : null;
    const count = rest === null ? props.length : props.length - 1;
    const hasAny = props.length > 0;
    if (hasAny) this.space();
    const depth = this.indentDepth;
    for (let i = 0; i < count; i++) {
      this.bindingProperty = true;
      this.emitExpr(props[i], CTX_DEFER_TRAILING);
      this.closeItem(i + 1 < count || rest !== null, depth);
    }
    if (rest !== null) {
      this.emitExpr(rest, CTX_DEFER_TRAILING);
      this.closeItem(false, depth);
    }
    this.closeList(depth);
    if (hasAny && !this.atLineStart()) this.space();
    this.writeToken("}");
    this.printBindingSuffix(p.optional === true, definite, p.typeAnnotation);
  }

  printPropertyKey(key: Node, computed: boolean): void {
    if (computed) {
      this.writeToken("[");
      this.emitValue(key);
      this.writeToken("]");
    } else {
      this.emit(key);
    }
  }

  // computed `["__proto__"]` defines an own property, bare `__proto__` sets the prototype
  printObjectKey(key: Node, computed: boolean): void {
    if (this.minify) {
      const s = simpleStringKey(key);
      if (s !== null) {
        const protoClash = computed && s === "__proto__";
        if (!protoClash) return this.writeNodeText(key, s);
      }
    }
    this.printPropertyKey(key, computed);
  }

  // `["constructor"]` would become the constructor or a SyntaxError, `static ["prototype"]` too
  printClassKey(key: Node, computed: boolean, isStatic: boolean, isField: boolean): void {
    if (this.minify) {
      const s = simpleStringKey(key);
      if (s !== null) {
        if (!computed) return this.writeNodeText(key, s);
        const ctorClash = s === "constructor" && (isField || !isStatic);
        const protoClash = isStatic && s === "prototype";
        if (!ctorClash && !protoClash) return this.writeNodeText(key, s);
      }
    }
    this.printPropertyKey(key, computed);
  }

  emitFunction(f: T.Function): void {
    if (this.strip) {
      const tsOnly =
        f.declare === true ||
        f.type === "TSDeclareFunction" ||
        f.type === "TSEmptyBodyFunctionExpression";
      if (tsOnly) return;
    }
    if (!this.strip && f.declare === true) this.writeKeyword("declare");
    if (f.async) this.writeKeyword("async");
    const keyword = f.generator ? "function*" : "function";
    if (f.id != null) {
      this.writeKeyword(keyword);
      this.emit(f.id);
    } else {
      this.writeToken(keyword);
    }
    this.printFunctionAsMethod(f);
  }

  emitMethodValue(fn: T.Function): void {
    const list = this.openComments(fn);
    this.printFunctionAsMethod(fn);
    this.closeComments(fn, list, false);
  }

  printFunctionAsMethod(f: T.Function): void {
    this.emit(f.typeParameters);
    this.printParams(f.params, f);
    this.emit(f.returnType);
    if (f.body != null) {
      this.space();
      this.functionBody = true;
      this.emit(f.body);
      this.functionBody = false;
    } else if (!this.strip) {
      this.softSemi();
    }
  }

  emitArrowFunctionExpression(a: T.ArrowFunctionExpression, ctx: number): void {
    if (a.async) this.writeKeyword("async");
    this.emitExpr(a.typeParameters, CTX_NO_JSX_TAG);
    this.printParams(a.params, a);
    this.emitExpr(a.returnType, CTX_DEFER_TRAILING);
    this.writeSpaced(" =>", "=>");
    this.emitOwedComments();
    if (!this.atLineStart()) this.space();
    if (a.body.type !== "BlockStatement") {
      this.lead = LEAD_ARROW;
      this.emitExpr(a.body, PREC_ASSIGNMENT | (ctx & CTX_NO_IN));
    } else {
      this.functionBody = true;
      this.emit(a.body);
      this.functionBody = false;
    }
  }

  printParams(params: readonly Node[], host: Node): void {
    this.writeToken("(");
    const thisParam = this.strip && params.length > 0 && isNamed(params[0]!, "this");
    this.emitKeptItems(thisParam ? params.slice(1) : params);
    this.emitInsideCommentsInline(host);
    this.writeToken(")");
  }

  emitKeptItems(items: readonly Node[]): void {
    let end = items.length;
    if (this.strip) {
      while (end > 0 && this.stripsToNothing(items[end - 1])) end--;
    }
    const depth = this.indentDepth;
    for (let i = 0; i < items.length; i++) {
      const node = items[i]!;
      if (this.strip && this.stripsToNothing(node)) {
        this.emitNothing(node);
        continue;
      }
      this.emitExpr(node, CTX_DEFER_TRAILING);
      this.closeItem(i + 1 < end, depth);
    }
    this.closeList(depth);
  }

  emitClass(c: T.Class, ctx: number): void {
    if (this.strip && c.declare === true) return;
    if ((ctx & CTX_NO_DECORATORS) === 0) this.printDecorators(c.decorators);
    if (!this.strip) {
      if (c.declare === true) this.writeKeyword("declare");
      if (c.abstract === true) this.writeKeyword("abstract");
    }
    if (c.id != null) {
      this.writeKeyword("class");
      this.emit(c.id);
    } else {
      this.writeToken("class");
    }
    this.emit(c.typeParameters);
    if (c.superClass != null) {
      this.writeKeyword(" extends");
      this.emitExpr(c.superClass, PREC_CALL | CTX_NO_INSTANTIATION);
      this.emit(c.superTypeArguments);
    }
    if (!this.strip && c.implements != null && c.implements.length > 0) {
      this.writeKeyword(" implements");
      this.emitList(c.implements);
    }
    this.space();
    this.emit(c.body);
  }

  emitClassBody(b: T.ClassBody): void {
    this.writeToken("{");
    this.indentDepth++;
    let any = false;
    for (const m of b.body) {
      if (this.strip && this.stripsToNothing(m)) {
        this.emitNothing(m);
        continue;
      }
      this.flushSemi();
      this.newline();
      this.emit(m);
      any = true;
    }
    this.indentDepth--;
    if (any) {
      this.pendingSemi = false;
      this.newline();
    } else if (this.comments !== "none") {
      this.emitInsideComments(b);
    }
    this.writeToken("}");
  }

  emitMethodDefinition(m: T.MethodDefinition | T.TSAbstractMethodDefinition): void {
    const fn = m.value as T.Function;
    const isAbstract = m.type === "TSAbstractMethodDefinition";
    if (this.strip && (isAbstract || fn.body == null)) return;
    this.printDecorators(m.decorators);
    this.hoistKeyComments(m.key);
    if (!this.strip && m.accessibility != null) {
      this.writeKeyword(m.accessibility);
    }
    if (m.static) this.writeKeyword("static");
    if (!this.strip) {
      if (isAbstract) this.writeKeyword("abstract");
      if (m.override === true) this.writeKeyword("override");
    }
    switch (m.kind) {
      case "get":
        this.writeKeyword("get");
        break;
      case "set":
        this.writeKeyword("set");
        break;
      default:
        if (fn.async) this.writeKeyword("async");
        if (fn.generator) this.writeToken("*");
    }
    this.printClassKey(m.key, m.computed, m.static, false);
    if (!this.strip && m.optional === true) this.writeToken("?");
    this.emitMethodValue(fn);
    this.skipLeadingOf = null;
  }

  emitPropertyDefinition(
    p:
      | T.PropertyDefinition
      | T.AccessorProperty
      | T.TSAbstractPropertyDefinition
      | T.TSAbstractAccessorProperty,
  ): void {
    const isAbstract =
      p.type === "TSAbstractPropertyDefinition" || p.type === "TSAbstractAccessorProperty";
    const isAccessor = p.type === "AccessorProperty" || p.type === "TSAbstractAccessorProperty";
    if (this.strip && (p.declare === true || isAbstract)) return;
    this.printDecorators(p.decorators);
    this.hoistKeyComments(p.key);
    if (!this.strip) {
      if (p.declare === true) this.writeKeyword("declare");
      if (p.accessibility != null) {
        this.writeKeyword(p.accessibility);
      }
    }
    if (p.static) this.writeKeyword("static");
    if (!this.strip) {
      if (isAbstract) this.writeKeyword("abstract");
      if (p.override === true) this.writeKeyword("override");
      if (p.readonly === true) this.writeKeyword("readonly");
    }
    if (isAccessor) this.writeKeyword("accessor");
    if (!this.strip && p.definite === true) this.deferTrailingOf = p.key;
    this.printClassKey(p.key, p.computed, p.static, true);
    this.deferTrailingOf = null;
    if (!this.strip) {
      if (p.optional === true) this.writeToken("?");
      if (p.definite === true) this.writeToken("!");
    }
    this.emitOwedComments();
    this.emit(p.typeAnnotation);
    if (p.value != null) {
      this.printEq();
      this.emitValue(p.value);
    }
    this.softSemi();
    this.skipLeadingOf = null;
  }

  emitDecorator(d: T.Decorator): void {
    this.writeToken("@");
    const simple = decoratorIsSimple(d.expression);
    this.emitExpr(d.expression, simple ? PREC_LOWEST : PREC_GROUPING);
  }

  printDecorators(decs: readonly Node[] | undefined): void {
    if (decs == null || decs.length === 0) return;
    const carry = this.mapStart;
    for (let i = 0; i < decs.length; i++) {
      this.emit(decs[i]);
      // `@a.b class` would fuse to `@a.bclass` without a separator
      if (this.pretty) this.newline();
      else if (i + 1 === decs.length) this.writeToken(" ");
    }
    this.mapStart = carry;
  }

  emitImportDeclaration(d: T.ImportDeclaration): void {
    const list = d.specifiers;
    if (this.strip) {
      if (d.importKind === "type") return;
      if (list.length > 0 && !hasValueImportSpecifier(list)) return;
    }

    this.writeToken("import");
    if (d.importKind === "type") this.writeToken(" type");
    if (d.phase != null) {
      this.writeToken(" ");
      this.writeToken(d.phase);
    }

    if (list.length > 0) {
      this.writeToken(" ");
      const depth = this.indentDepth;
      let i = 0;
      if (list[0]!.type === "ImportDefaultSpecifier") {
        this.emitExpr(list[0], CTX_DEFER_TRAILING);
        this.closeItem(list.length > 1, depth);
        i = 1;
      }
      if (i < list.length) {
        if (list[i]!.type === "ImportNamespaceSpecifier") {
          this.emit(list[i]);
        } else {
          this.writeToken("{");
          this.space();
          this.emitKeptItems(i === 0 ? list : list.slice(i));
          if (!this.atLineStart()) this.space();
          this.writeToken("}");
        }
      }
      this.closeList(depth);
      this.writeKeyword(" from");
    } else {
      this.writeToken(" ");
    }

    this.emit(d.source);
    this.printAttributes(d.attributes);
    this.softSemi();
  }

  emitImportSpecifier(s: T.ImportSpecifier): void {
    if (s.importKind === "type") this.writeKeyword("type");
    this.emit(s.imported);
    if (!sameIdentifier(s.imported, s.local) || this.hasPrintedComments(s.local)) {
      this.writeKeyword(" as");
      this.emit(s.local);
    }
  }

  emitExportNamedDeclaration(d: T.ExportNamedDeclaration): void {
    const list = d.specifiers;
    if (this.strip) {
      if (d.exportKind === "type") return;
      const noValueSpecifiers =
        d.declaration == null && list.length > 0 && !hasValueExportSpecifier(list);
      if (noValueSpecifiers) return;
    }

    if (this.strip && d.declaration != null && this.stripsToNothing(d.declaration)) {
      return this.emitNothing(d.declaration);
    }
    const hoisted = decoratorsBeforeExport(d.declaration);
    if (hoisted !== null) this.printDecorators(hoisted);
    this.writeToken("export");
    if (d.exportKind === "type" && d.declaration == null) this.writeToken(" type");
    if (d.declaration != null) {
      this.writeToken(" ");
      return this.emitExpr(d.declaration, hoisted !== null ? CTX_NO_DECORATORS : 0);
    }
    this.writeSpaced(" {", "{");
    this.emitInsideCommentsInline(d);
    if (list.length > 0) {
      this.space();
      this.emitKeptItems(list);
      if (!this.atLineStart()) this.space();
    }
    this.writeToken("}");
    if (d.source != null) {
      this.writeKeyword(" from");
      this.emit(d.source);
    }
    this.printAttributes(d.attributes);
    this.softSemi();
  }

  emitExportDefaultDeclaration(d: T.ExportDefaultDeclaration): void {
    if (isDeclaration(d.declaration)) {
      if (this.strip && this.stripsToNothing(d.declaration)) {
        return this.emitNothing(d.declaration);
      }
      const hoisted = decoratorsBeforeExport(d.declaration);
      if (hoisted !== null) this.printDecorators(hoisted);
      this.writeKeyword("export default");
      return this.emitExpr(d.declaration, hoisted !== null ? CTX_NO_DECORATORS : 0);
    }
    this.writeKeyword("export default");
    this.lead = LEAD_EXPORT_DEFAULT;
    this.emitExpr(d.declaration, PREC_ASSIGNMENT);
    this.softSemi();
  }

  emitExportAllDeclaration(d: T.ExportAllDeclaration): void {
    if (this.strip && d.exportKind === "type") return;
    this.writeToken("export");
    if (d.exportKind === "type") this.writeToken(" type");
    this.writeToken(" *");
    if (d.exported != null) {
      this.writeKeyword(" as");
      this.emit(d.exported);
    }
    this.writeKeyword(" from");
    this.emit(d.source);
    this.printAttributes(d.attributes);
    this.softSemi();
  }

  emitExportSpecifier(s: T.ExportSpecifier): void {
    if (s.exportKind === "type") this.writeKeyword("type");
    this.emit(s.local);
    if (!sameIdentifier(s.local, s.exported) || this.hasPrintedComments(s.exported)) {
      this.writeKeyword(" as");
      this.emit(s.exported);
    }
  }

  printAttributes(attrs: readonly Node[] | undefined): void {
    if (attrs == null || attrs.length === 0) return;
    this.writeKeyword(" with");
    this.writeToken("{");
    this.space();
    this.emitList(attrs);
    this.writeSpaced(" }", "}");
  }

  emitJSXOpeningElement(o: T.JSXOpeningElement): void {
    this.writeToken("<");
    this.emit(o.name);
    this.emit(o.typeArguments);
    for (const a of o.attributes) {
      this.writeToken(" ");
      this.emit(a);
    }
    if (o.selfClosing) {
      this.writeSpaced(" />", "/>");
    } else {
      this.writeToken(">");
    }
  }

  printJSXSpread(node: Node): void {
    this.writeToken("{...");
    this.emitValue(node);
    this.writeToken("}");
  }

  emitJSXAttribute(a: T.JSXAttribute): void {
    this.emit(a.name);
    const value = a.value;
    if (value == null) return;
    this.writeToken("=");
    if (value.type === "Literal" && typeof value.value === "string") {
      // jsx attribute strings have no escapes, the raw lexeme is the value
      if (typeof value.raw === "string" && value.raw.length >= 2) {
        this.writeNodeText(value, value.raw);
      } else if (value.value.includes('"') && value.value.includes("'")) {
        this.writeToken("{");
        this.emit(value);
        this.writeToken("}");
      } else {
        this.writeNodeText(value, quoteVerbatim(value.value));
      }
    } else {
      this.emit(value);
    }
  }

  emitTypeScript(node: Node, ctx: number): void {
    switch (node.type) {
      case "TSTypeAnnotation":
        this.writeToken(":");
        this.space();
        return this.emit(node.typeAnnotation);
      case "TSTypeReference":
        this.emit(node.typeName);
        return this.emit(node.typeArguments);
      case "TSQualifiedName":
        this.emit(node.left);
        this.writeToken(".");
        return this.emit(node.right);
      case "TSTypeQuery":
        this.writeKeyword("typeof");
        this.emit(node.exprName);
        return this.emit(node.typeArguments);
      case "TSImportType":
        return this.emitTSImportType(node);
      case "TSTypeParameter":
        return this.emitTSTypeParameter(node);
      case "TSTypeParameterDeclaration":
        return this.emitTSTypeParameterDeclaration(node, ctx);
      case "TSTypeParameterInstantiation":
        this.writeToken("<");
        this.emitList(node.params);
        return this.writeToken(">");
      case "TSLiteralType": {
        const lit = node.literal;
        // `!0` is no type
        if (lit.type === "Literal" && typeof lit.value === "boolean") {
          return this.writeToken(lit.value ? "true" : "false");
        }
        return this.emit(lit);
      }
      case "TSTemplateLiteralType":
        return this.printTemplate(node.quasis, node.types, false);
      case "TSArrayType":
        this.emitType(node.elementType, TPREC_PRIMARY, CTX_DEFER_TRAILING);
        this.writeToken("[");
        this.emitOwedComments();
        return this.writeToken("]");
      case "TSIndexedAccessType":
        this.emitType(node.objectType, TPREC_PRIMARY, CTX_DEFER_TRAILING);
        this.writeToken("[");
        this.emitOwedComments();
        this.emit(node.indexType);
        return this.writeToken("]");
      case "TSTupleType":
        this.writeToken("[");
        this.emitInsideCommentsInline(node);
        this.emitList(node.elementTypes);
        return this.writeToken("]");
      case "TSNamedTupleMember":
        this.emit(node.label);
        if (node.optional) this.writeToken("?");
        this.writeToken(":");
        this.space();
        return this.emit(node.elementType);
      case "TSOptionalType":
        this.emitType(node.typeAnnotation, TPREC_PRIMARY, 0);
        return this.writeToken("?");
      case "TSRestType":
        this.writeToken("...");
        return this.emit(node.typeAnnotation);
      case "TSJSDocNullableType":
        return this.printJSDocNullability("?", node.typeAnnotation, node.postfix);
      case "TSJSDocNonNullableType":
        return this.printJSDocNullability("!", node.typeAnnotation, node.postfix);
      case "TSUnionType":
        return this.emitTypeList(node.types, "|");
      case "TSIntersectionType":
        return this.emitTypeList(node.types, "&");
      case "TSConditionalType":
        this.emitType(node.checkType, TPREC_UNION, CTX_DEFER_TRAILING);
        this.writeKeywordThenOwed(" extends");
        this.emitType(node.extendsType, TPREC_UNION, 0);
        this.writeSpaced(" ?", "?");
        this.space();
        this.emit(node.trueType);
        this.writeSpaced(" :", ":");
        this.space();
        return this.emit(node.falseType);
      case "TSInferType":
        this.writeKeyword("infer");
        return this.emit(node.typeParameter);
      case "TSTypeOperator":
        this.writeKeyword(node.operator);
        return this.emitType(node.typeAnnotation, TPREC_OPERATOR, 0);
      case "TSParenthesizedType":
        this.writeToken("(");
        this.emit(node.typeAnnotation);
        return this.writeToken(")");
      case "TSFunctionType":
        return this.printArrowType(node);
      case "TSConstructorType":
        if (node.abstract) this.writeKeyword("abstract");
        this.writeKeyword("new");
        return this.printArrowType(node);
      case "TSTypePredicate":
        if (node.asserts) this.writeKeyword("asserts");
        if (node.typeAnnotation == null) return this.emit(node.parameterName);
        this.emitExpr(node.parameterName, CTX_DEFER_TRAILING);
        this.writeKeywordThenOwed(" is");
        return this.emitUnwrappedType(node.typeAnnotation);
      case "TSTypeLiteral":
        return this.printSignatureBody(node.members, node);
      case "TSMappedType":
        return this.emitTSMappedType(node);
      case "TSPropertySignature":
        if (node.readonly) this.writeKeyword("readonly");
        this.printPropertyKey(node.key, node.computed);
        if (node.optional) this.writeToken("?");
        this.emit(node.typeAnnotation);
        return this.softSemi();
      case "TSMethodSignature":
        if (node.kind === "get") this.writeKeyword("get");
        else if (node.kind === "set") this.writeKeyword("set");
        this.printPropertyKey(node.key, node.computed);
        if (node.optional) this.writeToken("?");
        return this.printSignatureTail(node);
      case "TSCallSignatureDeclaration":
        return this.printSignatureTail(node);
      case "TSConstructSignatureDeclaration":
        this.writeKeyword("new");
        return this.printSignatureTail(node);
      case "TSIndexSignature":
        if (node.static === true) this.writeKeyword("static");
        if (node.readonly) this.writeKeyword("readonly");
        this.writeToken("[");
        this.emitList(node.parameters);
        this.writeToken("]");
        this.emit(node.typeAnnotation);
        return this.softSemi();
      case "TSTypeAliasDeclaration":
        if (node.declare) this.writeKeyword("declare");
        this.writeKeyword("type");
        this.emit(node.id);
        this.emit(node.typeParameters);
        this.printEq();
        // a leftmost bare `intrinsic` reference would reparse as the keyword
        this.wrapIf(isLeftmostIntrinsicReference(node.typeAnnotation), node.typeAnnotation, 0);
        return this.softSemi();
      case "TSInterfaceDeclaration":
        if (node.declare) this.writeKeyword("declare");
        this.writeKeyword("interface");
        this.emit(node.id);
        this.emit(node.typeParameters);
        if (node.extends.length > 0) {
          this.writeKeyword(" extends");
          this.emitList(node.extends);
        }
        this.space();
        return this.emit(node.body);
      case "TSInterfaceBody":
        return this.printSignatureBody(node.body, node);
      case "TSInterfaceHeritage":
      case "TSClassImplements":
        this.emit(node.expression);
        return this.emit(node.typeArguments);
      case "TSEnumDeclaration":
        if (node.declare) this.writeKeyword("declare");
        if (node.const) this.writeKeyword("const");
        this.writeKeyword("enum");
        this.emit(node.id);
        this.space();
        return this.emit(node.body);
      case "TSEnumBody":
        return this.emitTSEnumBody(node);
      case "TSEnumMember":
        if (node.computed) {
          this.writeToken("[");
          this.emit(node.id);
          this.writeToken("]");
        } else {
          this.emit(node.id);
        }
        if (node.initializer != null) {
          this.printEq();
          this.emit(node.initializer);
        }
        return;
      case "TSModuleDeclaration":
        if (node.declare) this.writeKeyword("declare");
        if (node.global) {
          this.emit(node.id);
          this.space();
          return this.emit(node.body);
        }
        this.writeKeyword(node.kind);
        this.emit(node.id);
        if (node.body != null) {
          this.space();
          return this.emit(node.body);
        }
        return this.softSemi();
      case "TSModuleBlock":
        return this.printBlock(node.body, false, node);
      case "TSTypeAssertion":
        this.writeToken("<");
        // `<<T>` would re-lex as `<<`
        if (typeStartsWithLeftAngle(node.typeAnnotation)) this.writeToken(" ");
        this.emit(node.typeAnnotation);
        this.writeToken(">");
        return this.emitExpr(node.expression, PREC_UNARY | (ctx & CTX_NO_INSTANTIATION));
      case "TSExportAssignment":
        this.writeToken("export");
        this.printEq();
        this.emit(node.expression);
        return this.softSemi();
      case "TSNamespaceExportDeclaration":
        this.writeKeyword("export as namespace");
        this.emit(node.id);
        return this.softSemi();
      case "TSImportEqualsDeclaration":
        this.writeKeyword("import");
        if (node.importKind === "type") this.writeKeyword("type");
        this.emit(node.id);
        this.printEq();
        this.emit(node.moduleReference);
        return this.softSemi();
      case "TSExternalModuleReference":
        this.writeToken("require(");
        this.emit(node.expression);
        return this.writeToken(")");
      case "TSParameterProperty":
        this.printDecorators(node.decorators);
        if (node.accessibility != null) {
          this.writeKeyword(node.accessibility);
        }
        if (node.override) this.writeKeyword("override");
        if (node.readonly) this.writeKeyword("readonly");
        return this.emit(node.parameter);
    }
    throw new Error("yuku-codegen: unsupported node type: " + node.type);
  }

  emitTSImportType(t: T.TSImportType): void {
    this.writeToken("import(");
    this.emit(t.source);
    if (t.options != null) {
      this.writeToken(",");
      this.space();
      this.emit(t.options);
    }
    this.writeToken(")");
    if (t.qualifier != null) {
      this.writeToken(".");
      this.emit(t.qualifier);
    }
    this.emit(t.typeArguments);
  }

  emitTSTypeParameter(p: T.TSTypeParameter): void {
    if (p.const) this.writeKeyword("const");
    if (p.in) this.writeKeyword("in");
    if (p.out) this.writeKeyword("out");
    this.emit(p.name);
    if (p.constraint != null) {
      this.writeKeyword(" extends");
      this.emit(p.constraint);
    }
    if (p.default != null) {
      this.printEq();
      this.emit(p.default);
    }
  }

  emitTSTypeParameterDeclaration(d: T.TSTypeParameterDeclaration, ctx: number): void {
    this.writeToken("<");
    this.emitList(d.params);
    const params = d.params;
    // in TSX a lone `<T>` opens a JSX tag, and `<T,>` is valid TS too
    if ((ctx & CTX_NO_JSX_TAG) !== 0 && params.length === 1) {
      if (params[0]!.constraint == null) this.writeToken(",");
    }
    this.writeToken(">");
  }

  emitType(node: Node, floor: number, ctx: number): void {
    this.wrapIf(typePrec(node) < floor, node, ctx);
  }

  // a lone member keeps its leading operator (`type X = | A`) so reparse keeps the wrapper
  emitTypeList(types: readonly Node[], op: "|" | "&"): void {
    if (types.length === 1) {
      this.writeToken(op);
      this.space();
    }
    const floor = op === "|" ? TPREC_INTERSECTION : TPREC_OPERATOR;
    for (let i = 0; i < types.length; i++) {
      if (i > 0) {
        this.space();
        this.writeToken(op);
        this.space();
      }
      this.emitType(types[i]!, floor, 0);
    }
  }

  printJSDocNullability(marker: string, node: Node, postfix: boolean): void {
    if (!postfix) this.writeToken(marker);
    this.emit(node);
    if (postfix) this.writeToken(marker);
  }

  printArrowType(t: T.TSFunctionType | T.TSConstructorType): void {
    this.emit(t.typeParameters);
    this.printParams(t.params, t);
    this.writeSpaced(" =>", "=>");
    this.space();
    this.emitUnwrappedType(t.returnType);
  }

  emitUnwrappedType(node: Node | null | undefined): void {
    if (node == null) return;
    if (node.type !== "TSTypeAnnotation") return this.emit(node);
    const list = this.openComments(node);
    this.emit(node.typeAnnotation);
    this.closeComments(node, list, false);
  }

  emitTSMappedType(t: T.TSMappedType): void {
    this.writeToken("{");
    this.space();
    switch (t.readonly) {
      case true:
        this.writeKeyword("readonly");
        break;
      case "+":
        this.writeToken("+");
        this.writeKeyword("readonly");
        break;
      case "-":
        this.writeToken("-");
        this.writeKeyword("readonly");
        break;
    }
    this.writeToken("[");
    this.emit(t.key);
    this.writeKeyword(" in");
    this.emit(t.constraint);
    if (t.nameType != null) {
      this.writeKeyword(" as");
      this.emit(t.nameType);
    }
    this.writeToken("]");
    switch (t.optional) {
      case true:
        this.writeToken("?");
        break;
      case "+":
        this.writeToken("+?");
        break;
      case "-":
        this.writeToken("-?");
        break;
    }
    if (t.typeAnnotation != null) {
      this.writeToken(":");
      this.space();
      this.emit(t.typeAnnotation);
    }
    this.softSemi();
    this.pendingSemi = false;
    this.writeSpaced(" }", "}");
  }

  printSignatureTail(
    s: T.TSMethodSignature | T.TSCallSignatureDeclaration | T.TSConstructSignatureDeclaration,
  ): void {
    this.emit(s.typeParameters);
    this.printParams(s.params, s);
    this.emit(s.returnType);
    this.softSemi();
  }

  printSignatureBody(items: readonly Node[], host: Node): void {
    this.writeToken("{");
    if (items.length > 0) {
      this.indentDepth++;
      for (const s of items) {
        this.flushSemi();
        this.newline();
        this.emit(s);
      }
      this.indentDepth--;
      this.pendingSemi = false;
      this.newline();
    } else if (this.comments !== "none") {
      this.emitInsideComments(host);
    }
    this.writeToken("}");
  }

  wrapIf(cond: boolean, node: Node, ctx: number): void {
    if (cond) this.writeToken("(");
    this.emitExpr(node, ctx);
    if (cond) this.writeToken(")");
  }

  emitTSEnumBody(b: T.TSEnumBody): void {
    this.writeToken("{");
    const list = b.members;
    if (list.length > 0) {
      this.indentDepth++;
      for (let i = 0; i < list.length; i++) {
        this.newline();
        this.emitExpr(list[i], CTX_DEFER_TRAILING);
        this.closeItem(i + 1 < list.length, -1);
      }
      this.indentDepth--;
      this.newline();
    } else if (this.comments !== "none") {
      this.emitInsideComments(b);
    }
    this.writeToken("}");
  }
}

function trimEndCr(line: string): string {
  let end = line.length;
  while (end > 0 && line.charCodeAt(end - 1) === CHAR_CR) end--;
  return end === line.length ? line : line.slice(0, end);
}

function needsSpaceBeforeInlineComment(last: number): boolean {
  switch (last) {
    case 0:
    case CHAR_SPACE:
    case CHAR_LF:
    case CHAR_OPEN_PAREN:
    case CHAR_OPEN_BRACKET:
    case CHAR_OPEN_BRACE:
    case CHAR_LT:
      return false;
  }
  return true;
}

// an uninitialized `const` is ambient, as in a `.d.ts`
function isAmbient(d: T.VariableDeclaration): boolean {
  if (d.declare === true) return true;
  return d.kind === "const" && d.declarations.some((x) => x.init == null);
}

function strippedOperand(node: Node): Node {
  switch (node.type) {
    case "TSAsExpression":
    case "TSSatisfiesExpression":
    case "TSNonNullExpression":
    case "TSInstantiationExpression":
    case "TSTypeAssertion":
      return node.expression;
    case "TSParameterProperty":
      return node.parameter;
  }
  throw new Error("not a stripped wrapper: " + node.type);
}

function hasComments(node: Node): boolean {
  const list = node.comments;
  return list != null && list.length > 0;
}

function isPlainIdentifier(id: T.Identifier): boolean {
  if (id.typeAnnotation != null) return false;
  if (id.optional === true) return false;
  return id.decorators == null || id.decorators.length === 0;
}

function isNegativeNumber(value: unknown): boolean {
  return typeof value === "number" && (value < 0 || Object.is(value, -0));
}

function quoteVerbatim(text: string): string {
  return text.includes('"') ? "'" + text + "'" : '"' + text + '"';
}

function isDirective(node: Node): boolean {
  return node.type === "ExpressionStatement" && typeof node.directive === "string";
}

function isChainLink(node: Node): boolean {
  switch (node.type) {
    case "BinaryExpression":
    case "LogicalExpression":
    case "MemberExpression":
    case "CallExpression":
    case "TaggedTemplateExpression":
    case "ChainExpression":
    case "TSNonNullExpression":
    case "TSInstantiationExpression":
    case "TSAsExpression":
    case "TSSatisfiesExpression":
      return true;
  }
  return false;
}

function isDeclaration(node: Node): boolean {
  switch (node.type) {
    case "VariableDeclaration":
    case "ImportDeclaration":
    case "ExportNamedDeclaration":
    case "ExportDefaultDeclaration":
    case "ExportAllDeclaration":
    case "TSTypeAliasDeclaration":
    case "TSInterfaceDeclaration":
    case "TSEnumDeclaration":
    case "TSModuleDeclaration":
    case "TSImportEqualsDeclaration":
    case "FunctionDeclaration":
    case "TSDeclareFunction":
    case "ClassDeclaration":
      return true;
  }
  return false;
}

function decoratorsBeforeExport(declaration: Node | null | undefined): readonly Node[] | null {
  if (declaration == null) return null;
  if (declaration.type !== "ClassDeclaration" && declaration.type !== "ClassExpression") {
    return null;
  }
  const decorators = declaration.decorators;
  if (decorators == null || decorators.length === 0) return null;
  return decorators[0]!.start < declaration.start ? decorators : null;
}

function isNamed(node: Node, name: string): boolean {
  return node.type === "Identifier" && node.name === name;
}

function identifierName(node: Node | null | undefined): string | null {
  return node != null && node.type === "Identifier" ? node.name : null;
}

function sameIdentifier(a: Node | null | undefined, b: Node | null | undefined): boolean {
  const an = identifierName(a);
  if (an === null) return false;
  return an === identifierName(b);
}

function shorthandStillValid(key: Node, value: Node): boolean {
  const v = value.type === "AssignmentPattern" ? value.left : value;
  return sameIdentifier(key, v);
}

function hasValueImportSpecifier(list: readonly Node[]): boolean {
  for (const s of list) {
    switch (s.type) {
      case "ImportDefaultSpecifier":
      case "ImportNamespaceSpecifier":
        return true;
      case "ImportSpecifier":
        if (s.importKind !== "type") return true;
        break;
    }
  }
  return false;
}

function hasValueExportSpecifier(list: readonly Node[]): boolean {
  for (const s of list) {
    if (s.type === "ExportSpecifier" && s.exportKind !== "type") return true;
  }
  return false;
}

// the parser drops the parens a ts cast needs inside a destructuring target
function needsParensAsAssignTarget(node: Node): boolean {
  switch (node.type) {
    case "TSAsExpression":
    case "TSSatisfiesExpression":
    case "TSTypeAssertion":
      return true;
  }
  return false;
}

function typeStartsWithLeftAngle(node: Node | null | undefined): boolean {
  if (node == null) return false;
  if (node.type === "TSFunctionType") return node.typeParameters != null;
  if (node.type === "TSConstructorType") return !node.abstract && node.typeParameters != null;
  return false;
}

// `??` mixed with `&&`/`||` must be parenthesized
function logicalMismatch(parent: string, child: Node): boolean {
  if (child.type !== "LogicalExpression") return false;
  return (parent === "??") !== (child.operator === "??");
}

function simpleStringKey(node: Node): string | null {
  if (node.type !== "Literal" || typeof node.value !== "string") return null;
  return isIdentifierName(node.value) ? node.value : null;
}

function endsWithTsCast(node: Node): boolean {
  let n = node;
  for (;;) {
    switch (n.type) {
      case "TSAsExpression":
      case "TSSatisfiesExpression":
        return true;
      case "BinaryExpression":
      case "LogicalExpression":
      case "AssignmentExpression":
        n = n.right;
        continue;
      case "ConditionalExpression":
        n = n.alternate;
        continue;
    }
    return false;
  }
}

// TypeScript reads `f<T>` as comparisons before `<`, `>`, `+`, or `-`, and its scanner starts
// `>=` and `>>` with `>`
function canFollowTypeArguments(operator: string): boolean {
  switch (operator) {
    case "<":
    case ">":
    case ">=":
    case ">>":
    case ">>>":
    case "+":
    case "-":
      return false;
  }
  return true;
}

// `x as T < y` would re-lex as the type arguments `T<y>`
function binaryLeftPrecedence(e: T.BinaryExpression): number {
  if (e.operator === "**") return PREC_POSTFIX;
  if (e.operator.charCodeAt(0) === CHAR_LT && endsWithTsCast(e.left as Node)) {
    return PREC_GROUPING;
  }
  return OPERATOR_PRECEDENCE[e.operator]!;
}

function decoratorIsSimple(node: Node): boolean {
  let n = node;
  for (;;) {
    switch (n.type) {
      case "Identifier":
        return true;
      case "MemberExpression":
        if (n.computed) return false;
        n = n.object;
        continue;
      case "CallExpression":
        n = n.callee;
        continue;
    }
    return false;
  }
}

function isLeftmostIntrinsicReference(node: Node): boolean {
  let n = node;
  for (;;) {
    switch (n.type) {
      case "TSTypeReference":
        return n.typeArguments == null && isNamed(n.typeName, "intrinsic");
      case "TSArrayType":
        n = n.elementType;
        continue;
      case "TSIndexedAccessType":
        n = n.objectType;
        continue;
      case "TSUnionType":
      case "TSIntersectionType":
        if (n.types.length === 0) return false;
        n = n.types[0]!;
        continue;
      case "TSConditionalType":
        n = n.checkType;
        continue;
    }
    return false;
  }
}

function typePrec(node: Node): number {
  switch (node.type) {
    case "TSFunctionType":
    case "TSConstructorType":
    case "TSConditionalType":
    case "TSInferType":
      return TPREC_TRAILING;
    case "TSUnionType":
      return TPREC_UNION;
    case "TSIntersectionType":
      return TPREC_INTERSECTION;
    case "TSTypeOperator":
      return TPREC_OPERATOR;
  }
  return TPREC_PRIMARY;
}

function stringEscape(s: string, i: number, code: number, quote: number): string | null {
  switch (code) {
    case CHAR_BACKSLASH:
      return "\\\\";
    case CHAR_LF:
      return "\\n";
    case CHAR_CR:
      return "\\r";
    case CHAR_TAB:
      return "\\t";
    case CHAR_BACKSPACE:
      return "\\b";
    case CHAR_FF:
      return "\\f";
    case CHAR_VT:
      return "\\v";
    case CHAR_NUL:
      return isAsciiDigit(s.charCodeAt(i + 1)) ? "\\x00" : "\\0";
  }
  if (code === quote) return quote === CHAR_DOUBLE_QUOTE ? '\\"' : "\\'";
  return null;
}

function templateEscape(s: string, i: number, code: number): string | null {
  switch (code) {
    case CHAR_BACKSLASH:
      return "\\\\";
    case CHAR_BACKTICK:
      return "\\`";
    case CHAR_DOLLAR:
      return s.charCodeAt(i + 1) === CHAR_OPEN_BRACE ? "\\$" : null;
    case CHAR_CR:
      return "\\r";
    case CHAR_NUL:
      return isAsciiDigit(s.charCodeAt(i + 1)) ? "\\x00" : "\\0";
  }
  return null;
}

function isDecimal(raw: string): boolean {
  return !/^0(?:[xob]|[0-7]+$)/i.test(raw);
}

function shortestNumber(raw: string): string {
  const decimal = isDecimal(raw);
  if (decimal && isMinimalInteger(raw)) return raw;
  const cleaned = stripUnderscores(raw);
  if (cleaned === null) return raw;
  if (!decimal) return cleaned;
  // `010` is legacy octal and `08` sloppy decimal
  if (
    cleaned.length > 1 &&
    cleaned.charCodeAt(0) === CHAR_0 &&
    isAsciiDigit(cleaned.charCodeAt(1))
  ) {
    return cleaned;
  }
  return shortestDecimal(cleaned);
}
