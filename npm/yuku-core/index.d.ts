import type { Core, FileOptions, SourceLang, SourceType } from "@yuku-toolchain/types";

/** Loads the native core for this platform, the core every Yuku package uses by default. */
export function load(): Core;

/** The `lang` a path implies: `"dts"`, `"tsx"`, `"ts"`, `"jsx"`, or `"js"`. */
export function langFromPath(path: string): SourceLang;

/** The `sourceType` a path implies: `"commonjs"` for `.cjs` and `.cts`, else `"module"`. */
export function sourceTypeFromPath(path: string): SourceType;

/** Fills in `lang` and `sourceType` from `path` and checks them, before a call to a core. */
export function fileOptions<T extends FileOptions>(
  options: T,
): T & Required<Pick<FileOptions, "lang" | "sourceType">>;

export type { Core };
