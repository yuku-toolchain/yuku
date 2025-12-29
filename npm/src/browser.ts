export enum SourceType {
  Script = 0,
  Module = 1,
}

export enum Lang {
  JS = 0,
  TS = 1,
  JSX = 2,
  TSX = 3,
  DTS = 4,
}

export interface ParseOptions {
  sourceType?: 'script' | 'module';
  lang?: 'js' | 'ts' | 'jsx' | 'tsx' | 'dts';
}

export interface YukuNode {
  type: string;
  [key: string]: unknown;
}

// we will expand it later
export type YukuAST = {
  program: YukuNode[];
  errors: Record<string, unknown>[];
  comments: Record<string, unknown>[];
};

interface WasmExports {
  // allocate in wasm memory
  alloc: (size: number) => number;
  free: (ptr: number, size: number) => void;
  parse: (
    sourcePtr: number,
    sourceLen: number,
    sourceType: number,
    lang: number
  ) => number; // returns pointer to JSON string (0 on failure)
  get_result_len: () => number; // returns length of last parse result
  memory: WebAssembly.Memory;
}

let wasmInstance: WasmExports | null = null;

async function getWasmInstance(): Promise<WasmExports> {
  if (wasmInstance) return wasmInstance;

  const wasmModule = await WebAssembly.instantiateStreaming(fetch(new URL('./yuku.wasm', import.meta.url)));

  wasmInstance = wasmModule.instance.exports as unknown as WasmExports;

  return wasmInstance;
}

function parseInternal(
  wasm: WasmExports,
  source: string,
  options: ParseOptions
): YukuAST {
  const encoder = new TextEncoder();
  const sourceBytes = encoder.encode(source);
  const sourceLen = sourceBytes.length;

  // allocate source code buffer, allow empty source by not requiring allocation when length is 0
  const sourcePtr = sourceLen > 0 ? wasm.alloc(sourceLen) : 0;

  if (sourceLen > 0 && !sourcePtr) {
    throw new Error('Failed to allocate memory for source code');
  }

  try {
    if (sourceLen > 0 && sourcePtr) {
      const wasmMemory = new Uint8Array(wasm.memory.buffer, sourcePtr, sourceLen);
      wasmMemory.set(sourceBytes);
    }

    // options are numbers, so normalize them to pass to wasm
    const sourceType = normalizeSourceType(options.sourceType);
    const lang = normalizeLang(options.lang);

    const resultPtr = wasm.parse(sourcePtr, sourceLen, sourceType, lang);

    if (resultPtr === 0) {
      throw new Error('Failed to parse source code');
    }

    const jsonLen = wasm.get_result_len();

    if (!Number.isInteger(resultPtr) || resultPtr < 0 || resultPtr + jsonLen > wasm.memory.buffer.byteLength) {
      throw new Error('Invalid result pointer from WASM parser');
    }

    try {
      const decoder = new TextDecoder();

      const jsonBytes = new Uint8Array(wasm.memory.buffer, resultPtr, jsonLen);

      const jsonStr = decoder.decode(jsonBytes);

      return JSON.parse(jsonStr) as YukuAST;
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
 * Parse JavaScript/TypeScript source code into a Yuku AST.
 *
 * @param source - Source code to parse
 * @param options - Parse options
 * @returns Yuku AST
 * @throws Error if parsing fails
 *
 * @example
 * ```ts
 * const ast = await parse('const x = 5;', {
 *   sourceType: 'module',
 *   lang: 'js'
 * });
 * ```
 */
export async function parse(
  source: string,
  options: ParseOptions = {}
): Promise<YukuAST> {
  const wasm = await getWasmInstance();
  return parseInternal(wasm, source, options);
}

/**
 * Synchronous parse function (requires WASM to be preloaded).
 * Use `parse()` for most cases.
 *
 * @param source - Source code to parse
 * @param options - Parse options
 * @returns Yuku AST
 * @throws Error if WASM not loaded or parsing fails
 *
 * @example
 * ```ts
 * await preload(); // Load WASM first
 * const ast = parseSync('const x = 5;', {
 *   sourceType: 'module',
 *   lang: 'js'
 * });
 * ```
 */
export function parseSync(
  source: string,
  options: ParseOptions = {}
): YukuAST {
  if (!wasmInstance) {
    throw new Error(
      'WASM not loaded. Call parse() first or manually call preload()'
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

function normalizeSourceType(sourceType?: 'script' | 'module'): SourceType {
  return sourceType === 'script' ? SourceType.Script : SourceType.Module;
}

function normalizeLang(lang?: 'js' | 'ts' | 'jsx' | 'tsx' | 'dts'): Lang {
  switch (lang) {
    case 'ts':
      return Lang.TS;
    case 'jsx':
      return Lang.JSX;
    case 'tsx':
      return Lang.TSX;
    case 'dts':
      return Lang.DTS;
    default:
      return Lang.JS;
  }
}
