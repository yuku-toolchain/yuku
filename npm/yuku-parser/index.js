import binding from "./binding.js";
import { decode } from "./decode.js";

export function parse(source, options) {
  return decode(binding.parse(source, options ?? {}), source);
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
