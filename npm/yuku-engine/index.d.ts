import type { FileOptions, SourceLang, SourceType } from "@yuku-toolchain/types";

interface EngineOptions extends FileOptions {
  preserveParens?: boolean;
  semanticErrors?: boolean;
  attachComments?: boolean;
  tokens?: boolean;
}

/** Parses UTF-8 source into the AST buffer that `yuku-parser` decodes. */
export function parse(bytes: Uint8Array, options: EngineOptions): ArrayBuffer;

/** Parses and analyzes UTF-8 source into the buffer that `yuku-analyzer` decodes. */
export function analyze(bytes: Uint8Array, options: EngineOptions): ArrayBuffer;

/** The `lang` a path implies: `"dts"`, `"tsx"`, `"ts"`, `"jsx"`, or `"js"`. */
export function langFromPath(path: string): SourceLang;

/** The `sourceType` a path implies: `"commonjs"` for `.cjs` and `.cts`, else `"module"`. */
export function sourceTypeFromPath(path: string): SourceType;

export type { EngineOptions };
