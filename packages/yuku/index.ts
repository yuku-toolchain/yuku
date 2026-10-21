import { BindingFlags as bindingFlags, TokenKind as tokenKind } from "./core/decode";
import type { BindingFlagMap } from "./file/semantic";
import type { TokenKindMap } from "./types";

export type * from "./types";

export {
  parse,
  type FileOptions,
  type Location,
  type ParseOptions,
  type SourceFile,
} from "./file/source-file";
export { langFromPath, sourceTypeFromPath } from "./core/wasm";
export { Project, type ProjectOptions } from "./file/project";
export type {
  Binding,
  BindingFlagMap,
  Capture,
  Definition,
  Export,
  ExportKind,
  Import,
  ImportKind,
  ModuleFlags,
  Reference,
  Scope,
  ScopeKind,
  Space,
} from "./file/semantic";

export {
  walk,
  type Visitors,
  type WalkContext,
  type WalkHandler,
  type WalkHooks,
} from "./traverse/walk";
export { pattern, type Captures, type Pattern } from "./traverse/match";

export {
  print,
  type Comments,
  type Format,
  type Printer,
  type Printers,
  type PrintOptions,
  type PrintResult,
  type Quotes,
  type SourceMap,
  type SourceMapOptions,
} from "./print";

export type { AliasMap, AliasName } from "./ast/aliases";
export { b } from "./ast/builders";
export { is, type MaybeNode, type Wrapper } from "./ast/is";
export * as lexical from "./ast/lexical";
export { js, ts, type TemplateValue } from "./ast/template";
export { bindingIdentifiers, clone, nameOf, unwrap } from "./ast/utils";

/** Every token kind by name. */
export const TokenKind: TokenKindMap = tokenKind;
export type TokenKind = TokenKindMap[keyof TokenKindMap];

export const BindingFlags: BindingFlagMap = bindingFlags;
