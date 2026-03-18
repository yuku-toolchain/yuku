import type {
	ParseOptions,
	ParseResult,
	SourceLang,
	SourceType,
} from "yuku-parser-types";
import { deserializeAstJson } from "yuku-shared";

export type * from "yuku-parser-types";

interface WasmExports {
	alloc: (size: number) => number;
	free: (ptr: number, size: number) => void;
	parse: (
		sourcePtr: number,
		sourceLen: number,
		sourceType: number,
		lang: number,
		semanticErrors: number,
	) => bigint; // returns packed u64: high 32 bits = length, low 32 bits = pointer
	memory: WebAssembly.Memory;
}

let wasmInstance: WasmExports | null = null;

async function getWasmInstance(): Promise<WasmExports> {
	if (wasmInstance) return wasmInstance;

	const wasmModule = await WebAssembly.instantiateStreaming(
		fetch(new URL("./yuku.wasm", import.meta.url)),
	);

	wasmInstance = wasmModule.instance.exports as unknown as WasmExports;

	return wasmInstance;
}

function parseInternal(
	wasm: WasmExports,
	source: string,
	options: ParseOptions,
): ParseResult {
	const encoder = new TextEncoder();
	const sourceBytes = encoder.encode(source);
	const sourceLen = sourceBytes.length;

	// allocate source code buffer, allow empty source by not requiring allocation when length is 0
	const sourcePtr = sourceLen > 0 ? wasm.alloc(sourceLen) : 0;

	if (sourceLen > 0 && !sourcePtr) {
		throw new Error("Failed to allocate memory for source code");
	}

	try {
		if (sourceLen > 0 && sourcePtr) {
			const wasmMemory = new Uint8Array(
				wasm.memory.buffer,
				sourcePtr,
				sourceLen,
			);
			wasmMemory.set(sourceBytes);
		}

		// options are numbers, so normalize them to pass to wasm
		const sourceType = normalizeSourceType(options.sourceType);
		const lang = normalizeLang(options.lang);
		const semanticErrors = options.semanticErrors ? 1 : 0;

		const result = wasm.parse(
			sourcePtr,
			sourceLen,
			sourceType,
			lang,
			semanticErrors,
		);

		if (result === 0n) {
			throw new Error("Failed to parse source code");
		}

		// unpack u64: high 32 bits = length, low 32 bits = pointer
		const resultPtr = Number(result & 0xffffffffn);
		const jsonLen = Number(result >> 32n);

		if (resultPtr + jsonLen > wasm.memory.buffer.byteLength) {
			throw new Error("Invalid result pointer from WASM parser");
		}

		try {
			const decoder = new TextDecoder();

			const jsonBytes = new Uint8Array(wasm.memory.buffer, resultPtr, jsonLen);

			const jsonStr = decoder.decode(jsonBytes);

			return deserializeAstJson<ParseResult>(jsonStr);
		} finally {
			wasm.free(resultPtr, jsonLen);
		}
	} finally {
		if (sourceLen > 0 && sourcePtr) {
			wasm.free(sourcePtr, sourceLen);
		}
	}
}

/**
 * Parse JavaScript/TypeScript source code into an ESTree/TypeScript-ESTree compatible AST.
 *
 * @param source - Source code to parse
 * @param options - Parse options
 * @returns The parse result containing the AST, comments, and diagnostics
 * @throws Error if parsing fails
 *
 * @example
 * ```ts
 * const result = await parse('const x = 5;', {
 *   sourceType: 'module',
 *   lang: 'js'
 * });
 * ```
 */
export async function parse(
	source: string,
	options: ParseOptions = {},
): Promise<ParseResult> {
	const wasm = await getWasmInstance();
	return parseInternal(wasm, source, options);
}

/**
 * Synchronous parse function (requires WASM to be preloaded).
 * Use `parse()` for most cases.
 *
 * @param source - Source code to parse
 * @param options - Parse options
 * @returns The parse result containing the AST, comments, and diagnostics
 * @throws Error if WASM not loaded or parsing fails
 *
 * @example
 * ```ts
 * await preload(); // Load WASM first
 * const result = parseSync('const x = 5;', {
 *   sourceType: 'module',
 *   lang: 'js'
 * });
 * ```
 */
export function parseSync(
	source: string,
	options: ParseOptions = {},
): ParseResult {
	if (!wasmInstance) {
		throw new Error(
			"WASM not loaded. Call parse() first or manually call preload()",
		);
	}
	return parseInternal(wasmInstance, source, options);
}

/**
 * Preload WASM module (optional).
 * Useful if you want to load WASM before first parse.
 *
 * @example
 * ```ts
 * await preload();
 * // Now you can use parseSync()
 * const ast = parseSync('const x = 5;');
 * ```
 */
export async function preload(): Promise<void> {
	await getWasmInstance();
}

function normalizeSourceType(sourceType?: SourceType): number {
	return sourceType === "script" ? 0 : 1;
}

function normalizeLang(lang?: SourceLang): number {
	switch (lang) {
		case "ts":
			return 1;
		case "jsx":
			return 2;
		case "tsx":
			return 3;
		case "dts":
			return 4;
		default:
			return 0;
	}
}
