import type { Core } from "@yuku-toolchain/types";

type Bytes = ArrayBuffer | ArrayBufferView | WebAssembly.Module;

type Source = string | URL | Response | Bytes;

/**
 * Loads the WebAssembly core, to pass to a Yuku package as `core`. It loads
 * `@yuku-core/wasm/yuku-core.wasm` by default, or takes a URL, a `Response`, the module's bytes, or
 * a compiled `WebAssembly.Module`.
 */
export function load(source?: Source | PromiseLike<Source>): Promise<Core>;

/** Loads the WebAssembly core synchronously, from bytes or a compiled module already in hand. */
export function loadSync(source: Bytes): Core;

export type { Core };
