import { slotOf, walk } from "../ast/walk";
import type { Core } from "../core/core";
import { decode, type Decoded, type EncodedNodes, type Semantic } from "../core/decode";
import { encode } from "../core/encode";
import { isTs, read, shippedCore, sourceText, treeFlags } from "../core/read";
import type {
  Comment,
  Diagnostic,
  DiagnosticLabel,
  DiagnosticSeverity,
  Node,
  Program,
  SourceLang,
  SourceType,
  TokenList,
} from "../types";
import { Project } from "./project";
import {
  Model,
  type Binding,
  type Capture,
  type Definition,
  type Export,
  type Import,
  type ModuleFlags,
  type Reference,
  type Scope,
} from "./semantic";

export interface FileOptions {
  /** @default from `path`, else "js" */
  lang?: SourceLang;
  /**
   * `"commonjs"` is a script whose top level allows `return`.
   * @default "commonjs" for `.cjs` and `.cts` paths, else "module"
   */
  sourceType?: SourceType;
  /** Keep `ParenthesizedExpression` nodes. @default true */
  preserveParens?: boolean;
  /** Also report the early errors that need scopes, such as redeclarations. @default false */
  semanticErrors?: boolean;
  /** Attach each comment to its node's `comments`. @default false */
  attachComments?: boolean;
  /** Collect `tokens`. @default false */
  tokens?: boolean;
}

export interface ParseOptions extends FileOptions {
  /** Build the semantic model. @default false */
  analyze?: boolean;
  /** The file path, which `lang` and `sourceType` default from. */
  path?: string;
  core?: Core;
}

/** `line` is 1-based, `column` 0-based in UTF-16 code units. */
export interface Location {
  line: number;
  column: number;
}

/** Parses JavaScript or TypeScript. */
export function parse(source: string | Uint8Array, options: ParseOptions = {}): SourceFile {
  return new SourceFile(source, options, null);
}

export interface DiagnosticDetails {
  severity?: DiagnosticSeverity;
  labels?: DiagnosticLabel[];
  help?: string;
}

export function diagnosticAt(
  file: SourceFile,
  span: { start: number; end: number },
  message: string,
  details: DiagnosticDetails = {},
): Diagnostic {
  return {
    severity: details.severity ?? "error",
    message,
    path: file.path,
    start: span.start,
    end: span.end,
    labels: details.labels ?? [],
    help: details.help ?? null,
  };
}

export class SourceFile {
  readonly path: string | null;
  readonly source: string;
  readonly lang: SourceLang;
  readonly sourceType: SourceType;
  readonly project: Project | null;
  /** @internal */
  readonly _core: Core;
  private syntax: Decoded;
  private analyzed: Decoded;
  private model: Model | null = null;
  private lines: number[] | null = null;
  private solo: Project | null = null;

  /** @internal */
  constructor(source: string | Uint8Array, options: ParseOptions, project: Project | null) {
    const { text, malformed } = sourceText(source);
    this.path = options.path ?? null;
    this.source = text;
    this.project = project;
    this._core = options.core ?? shippedCore;
    const entry = project !== null || options.analyze === true ? "analyze" : "parse";
    const { buffer, lang, sourceType } = read(text, options, entry);
    this.lang = lang;
    this.sourceType = sourceType;
    this.syntax = decode(buffer, text, this.path);
    this.analyzed = this.syntax;
    if (malformed >= 0) {
      const span = { start: malformed, end: malformed + 1 };
      const help = "Save the file as UTF-8";
      const diagnostic = diagnosticAt(this, span, "Invalid UTF-8 sequence", { help });
      this.syntax.diagnostics().unshift(diagnostic);
    }
  }

  get program(): Program {
    return this.syntax.program();
  }
  /** Every comment, in source order. */
  get comments(): readonly Comment[] {
    return this.syntax.comments();
  }
  get tokens(): TokenList {
    const tokens = this.syntax.tokens();
    if (tokens === null) throw new Error("parse with `tokens: true` to read the tokens");
    return tokens;
  }
  get diagnostics(): readonly Diagnostic[] {
    return this.syntax.diagnostics();
  }

  /** Every scope, indexed by id. The first is the global scope. */
  get scopes(): readonly Scope[] {
    return this._model.scopes;
  }
  /** Where top-level code runs, the module scope or the global scope of a script. */
  get rootScope(): Scope {
    return this._model.rootScope;
  }
  /** Every binding, indexed by id. */
  get bindings(): readonly Binding[] {
    return this._model.bindings;
  }
  /** Every name in use, in source order. */
  get references(): readonly Reference[] {
    return this._model.references;
  }
  get imports(): readonly Import[] {
    return this._model.imports;
  }
  get exports(): readonly Export[] {
    return this._model.exports;
  }
  get moduleFlags(): ModuleFlags {
    return this._model.semantic.moduleFlags;
  }

  /** False once its path is set again or deleted. */
  get isCurrent(): boolean {
    return this.project === null || this.project.files.get(this.path ?? "") === this;
  }
  /** The files it imports from. */
  get dependencies(): readonly SourceFile[] {
    return this._project._dependencies(this);
  }
  /** The files that import from it. */
  get dependents(): readonly SourceFile[] {
    return this._project._dependents(this);
  }
  /** Every name it exports, through `export *` and module augmentations. */
  exportedNames(): string[] {
    return this._project._exportedNames(this);
  }
  /** The binding behind one of its exports. */
  resolveExport(name: string): Definition | null {
    return this._project._resolveExport(this, name);
  }

  /** The binding a node declares or refers to. */
  bindingOf(node: Node): Binding | null {
    const index = this.analyzed.indexOf(node);
    if (index === undefined) return null;
    return this._model.declarationAt(index) ?? this._model.referenceAt(index)?.binding ?? null;
  }
  referenceOf(node: Node): Reference | null {
    const index = this.analyzed.indexOf(node);
    return index === undefined ? null : this._model.referenceAt(index);
  }
  /** The scope a node creates, or the innermost one around it. */
  scopeOf(node: Node): Scope {
    const model = this._model;
    for (let current: Node | null = node; current !== null; current = this.parentOf(current)) {
      const index = this.analyzed.indexOf(current);
      if (index !== undefined) return model.scopes[model.semantic.nodeScope(index)]!;
    }
    return model.rootScope;
  }
  /** The outer bindings a function uses. Throws for a node that is not a function. */
  capturesOf(fn: Node): Capture[] {
    const index = this.analyzed.indexOf(fn);
    if (index === undefined) throw new TypeError("capturesOf: the function is not from this file");
    const model = this._model;
    const { scope, symbol, reference } = model.semantic;
    const functionScope = model.semantic.nodeScope(index);
    if (scope.kind(functionScope) !== "function" || scope.nodeIndex(functionScope) !== index) {
      throw new TypeError("capturesOf: the node does not create a function scope");
    }
    const captures = new Map<
      number,
      { binding: Binding; references: Reference[]; isWritten: boolean }
    >();
    for (let i = 0; i < reference.count; i++) {
      const target = reference.symbolId(i);
      if (target === null || reference.inTypePosition(i)) continue;
      if (!within(scope, reference.scopeId(i), functionScope)) continue;
      if (within(scope, symbol.scopeId(target), functionScope)) continue;
      let capture = captures.get(target);
      if (capture === undefined) {
        capture = { binding: model.bindings[target]!, references: [], isWritten: false };
        captures.set(target, capture);
      }
      capture.references.push(model.references[i]!);
      if (reference.isWrite(i)) capture.isWritten = true;
    }
    return [...captures.values()];
  }

  parentOf(node: Node): Node | null {
    const index = this.analyzed.indexOf(node);
    if (index !== undefined) {
      const parent = this.analyzed.parentIndex(index);
      if (parent < 0) return null;
      const candidate = this.analyzed.nodeOf(parent);
      if (slotOf(candidate, node) !== null) return candidate;
    }
    let found: Node | null = null;
    walk(this.program, {
      enter(current, ctx) {
        if (current !== node) return;
        found = ctx.parent;
        ctx.stop();
      },
    });
    return found;
  }
  /** The node, then each parent up to the root. */
  *ancestors(node: Node): IterableIterator<Node> {
    for (let current: Node | null = node; current !== null; current = this.parentOf(current)) {
      yield current;
    }
  }
  /** The innermost node containing a UTF-16 offset. */
  nodeAt(offset: number): Node | null {
    let found: Node | null = null;
    walk(this.program, {
      enter(node, ctx) {
        if (node.start <= offset && offset < node.end) found = node;
        else ctx.skip();
      },
    });
    return found;
  }
  locate(offset: number): Location {
    if (!Number.isInteger(offset) || offset < 0 || offset > this.source.length) {
      throw new RangeError(`locate: ${offset} is not an offset in the source`);
    }
    const lines = (this.lines ??= lineStarts(this.source));
    let low = 0;
    let high = lines.length - 1;
    while (low < high) {
      const mid = (low + high + 1) >>> 1;
      if (lines[mid]! <= offset) low = mid;
      else high = mid - 1;
    }
    return { line: low + 1, column: offset - lines[low]! };
  }

  /** Builds the semantic model again for the program as it is now, on its own nodes. */
  reanalyze(): void {
    const tree = encode(this.program, isTs(this.lang), true);
    const flags = treeFlags(this.lang, this.sourceType);
    const buffer = this._core.backend().runTree("analyzeTree", tree, "", flags, 0);
    this.analyzed = decode(buffer, "", this.path, tree as EncodedNodes);
    this.model = null;
    (this.project ?? this.solo)?._invalidate();
  }

  /** @internal */
  get _model(): Model {
    if (this.model !== null) return this.model;
    const semantic = this.analyzed.semantic();
    if (semantic === null) {
      throw new Error(
        "parse with `analyze: true`, or call `reanalyze()`, to read the semantic model",
      );
    }
    return (this.model = new Model(this, semantic));
  }

  /** @internal */
  get _project(): Project {
    return this.project ?? (this.solo ??= Project._solo(this));
  }
}

function within(scope: Semantic["scope"], id: number | null, ancestor: number): boolean {
  while (id !== null && id !== ancestor) id = scope.parentId(id);
  return id !== null;
}

function lineStarts(text: string): number[] {
  const starts = [0];
  if (text.indexOf("\r") === -1 && text.indexOf("\u2028") === -1 && text.indexOf("\u2029") === -1) {
    for (let at = text.indexOf("\n"); at !== -1; at = text.indexOf("\n", at + 1)) {
      starts.push(at + 1);
    }
    return starts;
  }
  for (let i = 0; i < text.length; i++) {
    const c = text.charCodeAt(i);
    if (c === 0x0d) {
      if (text.charCodeAt(i + 1) === 0x0a) i++;
      starts.push(i + 1);
    } else if (c === 0x0a || c === 0x2028 || c === 0x2029) {
      starts.push(i + 1);
    }
  }
  return starts;
}
