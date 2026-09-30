import type { SourceLang, SourceType } from "@yuku-toolchain/types";

/** The options the engine reads, as the Yuku packages pass them. */
interface EngineOptions {
  sourceType?: SourceType;
  lang?: SourceLang;
  preserveParens?: boolean;
  semanticErrors?: boolean;
  attachComments?: boolean;
  tokens?: boolean;
}

/** Parses UTF-8 source into the AST buffer that `yuku-parser` decodes. */
export function parse(source: Uint8Array, options: EngineOptions): ArrayBuffer;

/** Parses and analyzes UTF-8 source into the buffer that `yuku-analyzer` decodes. */
export function analyze(source: Uint8Array, options: EngineOptions): ArrayBuffer;

/**
 * Resolves a {@link SourceLang} from a file path's extension.
 *
 * - `.d.ts`, `.d.mts`, `.d.cts` → `"dts"`
 * - `.tsx` → `"tsx"`
 * - `.ts`, `.mts`, `.cts` → `"ts"`
 * - `.jsx` → `"jsx"`
 * - everything else → `"js"`
 */
export function langFromPath(path: string): SourceLang;

/**
 * Resolves a {@link SourceType} from a file path's extension.
 *
 * - `.cjs`, `.cts` → `"commonjs"`
 * - everything else → `"module"`
 */
export function sourceTypeFromPath(path: string): SourceType;

export type { EngineOptions };
