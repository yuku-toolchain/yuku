export * from "./ast";

import type { Program } from "./ast";

/** How the source code should be parsed. */
export type SourceType = "script" | "module";

/** Language variant of the source code. */
export type SourceLang = "js" | "ts" | "jsx" | "tsx" | "dts";

/** Options for configuring the parser. */
export interface ParseOptions {
	/**
	 * Parse as a classic script or an ES module.
	 * Module mode enables `import`/`export`, `import.meta`, top-level `await`,
	 * and strict mode.
	 * @default "module"
	 */
	sourceType?: SourceType;

	/**
	 * Language variant — controls which syntax extensions are enabled.
	 * @default "js"
	 */
	lang?: SourceLang;

	/**
	 * Run semantic analysis after parsing and include semantic errors
	 * (e.g. duplicate declarations, invalid `break`/`continue` targets)
	 * alongside syntax errors. This requires a separate AST pass and may
	 * affect performance slightly.
	 * @default false
	 */
	semanticErrors?: boolean;
}

/** A source code comment. */
export interface Comment {
	type: "Line" | "Block";
	/** Comment text without the delimiters. */
	value: string;
	/** UTF-8 byte offset. */
	start: number;
	/** UTF-8 byte offset. */
	end: number;
}

/** A labeled source span attached to a {@link Diagnostic}. */
export interface DiagnosticLabel {
	/** UTF-8 byte offset. */
	start: number;
	/** UTF-8 byte offset. */
	end: number;
	message: string;
}

/**
 * A diagnostic produced during parsing or semantic analysis.
 * The parser is error-tolerant — an AST is always produced even when errors exist.
 */
export interface Diagnostic {
	severity: "error" | "warning" | "hint" | "info";
	message: string;
	/** Fix suggestion, or `null` if unavailable. */
	help: string | null;
	/** UTF-8 byte offset. */
	start: number;
	/** UTF-8 byte offset. */
	end: number;
	/** Additional source spans providing context. */
	labels: DiagnosticLabel[];
}

/** The result returned by the parser. */
export interface ParseResult {
	/** Root ESTree/TypeScript-ESTree AST node. */
	program: Program;
	/** All comments in source order. */
	comments: Comment[];
	/** Syntax errors, and semantic errors when {@link ParseOptions.semanticErrors} is enabled. */
	errors: Diagnostic[];
}
