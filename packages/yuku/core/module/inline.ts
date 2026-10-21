import base64 from "../yuku.wasm";
import { decodeBase64 } from "../base64";
import { NodeBuffer } from "../node";

let module: WebAssembly.Module | null = null;

export function bundled(): WebAssembly.Module {
  if (module === null) module = new WebAssembly.Module(bytesOf(base64));
  return module;
}

function bytesOf(text: string): Uint8Array<ArrayBuffer> {
  const native = Uint8Array as { fromBase64?(text: string): Uint8Array<ArrayBuffer> };
  if (native.fromBase64 !== undefined) return native.fromBase64(text);
  if (NodeBuffer !== undefined) return NodeBuffer.from(text, "base64");
  return decodeBase64(text);
}
