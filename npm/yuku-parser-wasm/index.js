import { walk as astWalk } from "yuku-ast";
import { decode } from "./decode.js";

const wasmUrl = new URL("./yuku-parser.wasm", import.meta.url);

async function instantiate() {
  if (wasmUrl.protocol !== "file:") {
    try {
      return (await WebAssembly.instantiateStreaming(fetch(wasmUrl))).instance;
    } catch {
      const bytes = await (await fetch(wasmUrl)).arrayBuffer();
      return (await WebAssembly.instantiate(bytes)).instance;
    }
  }
  const { readFile } = await import("node:fs/promises");
  return (await WebAssembly.instantiate(await readFile(wasmUrl))).instance;
}

const { memory, alloc, free, parse: wasmParse } = (await instantiate()).exports;
const _enc = new TextEncoder();

const SOURCE_TYPES = { script: 0, module: 1, commonjs: 2 };
const LANGS = { js: 0, ts: 1, jsx: 2, tsx: 3, dts: 4 };

function packFlags(o = {}) {
  let f = (SOURCE_TYPES[o.sourceType] ?? 1) | ((LANGS[o.lang] ?? 0) << 2);
  if (o.preserveParens !== false) f |= 1 << 5;
  if (o.semanticErrors) f |= 1 << 6;
  if (o.attachComments) f |= 1 << 7;
  if (o.tokens) f |= 1 << 8;
  return f;
}

export function parse(source, options) {
  const bytes = typeof source === "string" ? _enc.encode(source) : source;
  const srcLen = bytes.length;
  const srcPtr = alloc(srcLen || 1);
  // Growing wasm memory detaches memory.buffer, so re-view after every call.
  new Uint8Array(memory.buffer, srcPtr, srcLen).set(bytes);

  const ptr = wasmParse(srcPtr, srcLen, packFlags(options));
  free(srcPtr, srcLen || 1);
  if (ptr === 0) throw new Error("yuku-parser-wasm: failed to parse source");

  const len = new DataView(memory.buffer).getUint32(ptr, true);
  const buffer = memory.buffer.slice(ptr + 4, ptr + 4 + len);
  free(ptr, 4 + len);

  return decode(buffer, source);
}

export function langFromPath(path) {
  if (path.endsWith(".d.ts") || path.endsWith(".d.mts") || path.endsWith(".d.cts")) return "dts";
  if (path.endsWith(".tsx")) return "tsx";
  if (path.endsWith(".ts") || path.endsWith(".mts") || path.endsWith(".cts")) return "ts";
  if (path.endsWith(".jsx")) return "jsx";
  return "js";
}

export function sourceTypeFromPath(path) {
  return path.endsWith(".cjs") || path.endsWith(".cts") ? "commonjs" : "module";
}

export { WalkContext } from "yuku-ast";

let deprecationWarned = false;
function warnWalkMoved(name) {
  if (deprecationWarned) return;
  deprecationWarned = true;
  console.warn(
    `[@yuku-parser/wasm] ${name}() has moved to the yuku-ast package. ` +
      `Install yuku-ast and update the import to: import { ${name} } from "yuku-ast". ` +
      `This re-export will be removed in an upcoming minor version.`,
  );
}

export function walk(root, visitors, state) {
  warnWalkMoved("walk");
  return astWalk(root, visitors, state);
}
