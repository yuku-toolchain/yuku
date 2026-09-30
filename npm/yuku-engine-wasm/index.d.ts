/** Where {@link init} loads the WebAssembly build from. */
type InitInput =
  | string
  | URL
  | Response
  | Promise<Response>
  | ArrayBuffer
  | ArrayBufferView
  | WebAssembly.Module;

/**
 * Loads the WebAssembly build once, before the first call to any Yuku package. Runtimes without
 * `node:fs`, such as browsers and edge runtimes, need it. Elsewhere it loads on first use.
 *
 * @param wasm The module to load. Defaults to `@yuku-engine/wasm/yuku-engine.wasm`.
 */
export function init(wasm?: InitInput): Promise<void>;

/** The calls `yuku-engine` makes, returning the same buffers as the native binary. */
export function parse(source: Uint8Array, options: object): ArrayBuffer;
export function analyze(source: Uint8Array, options: object): ArrayBuffer;

export type { InitInput };
