export {
  parse,
  type FileOptions,
  type Location,
  type ParseOptions,
  type SourceFile,
} from "./file/source-file";
export { Core, type CoreOptions, type Platform } from "./core/core";

export { BindingFlags, TokenKind } from "./core/decode";

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

export { Project, type ProjectOptions } from "./file/project";

export {
  print,
  type Comments,
  type Format,
  type PrintOptions,
  type PrintResult,
  type Quotes,
  type SourceMap,
  type SourceMapOptions,
} from "./print";
