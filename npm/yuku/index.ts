import { BindingFlags as bindingFlags, TokenKind as tokenKind } from "./core/decode";
import type { BindingFlagMap } from "./file/semantic";
import type { TokenKindMap } from "./types";

export {
  parse,
  type FileOptions,
  type Location,
  type ParseOptions,
  type SourceFile,
} from "./file/source-file";
export { Core, type CoreOptions, type Platform } from "./core/core";

/** Every token kind by name. */
export const TokenKind: TokenKindMap = tokenKind;
export type TokenKind = TokenKindMap[keyof TokenKindMap];

export type * from "./types";
export type { AliasMap, AliasName } from "./ast/aliases";

export {
  CHILD_KEYS,
  walk,
  type Visitors,
  type WalkContext,
  type WalkHandler,
  type WalkHooks,
} from "./ast/walk";

export { b } from "./ast/builders";

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
export const BindingFlags: BindingFlagMap = bindingFlags;

export { Project, type ProjectOptions } from "./file/project";

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
