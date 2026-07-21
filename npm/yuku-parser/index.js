import { walk as astWalk } from "yuku-ast";
import binding from "./binding.js";
import { decode } from "./decode.js";

const _enc = new TextEncoder();

export function parse(source, options) {
  const bytes = typeof source === "string" ? _enc.encode(source) : source;
  return decode(binding.parse(bytes, options ?? {}), source);
}

export function langFromPath(path) {
  if (path.endsWith(".d.ts") || path.endsWith(".d.mts") || path.endsWith(".d.cts")) return "dts";
  if (path.endsWith(".tsx")) return "tsx";
  if (path.endsWith(".ts") || path.endsWith(".mts") || path.endsWith(".cts")) return "ts";
  if (path.endsWith(".jsx")) return "jsx";
  return "js";
}

export function sourceTypeFromPath(path) {
  return path.endsWith(".cjs") || path.endsWith(".cts") ? "script" : "module";
}

export { WalkContext, walk } from "yuku-ast";
