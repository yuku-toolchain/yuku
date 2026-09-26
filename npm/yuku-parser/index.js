import binding from "./binding.js";
import { decode } from "./decode.js";

export { TokenKind } from "./decode.js";

const _enc = new TextEncoder();
const _dec = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

export function parse(source, options) {
  const text = typeof source === "string" ? source : _dec.decode(source);
  const bytes = typeof source === "string" ? _enc.encode(source) : source;
  return decode(binding.parse(bytes, options ?? {}), text);
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
