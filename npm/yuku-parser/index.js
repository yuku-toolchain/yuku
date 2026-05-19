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

export function locOf(lineStarts, offset) {
  let lo = 0, hi = lineStarts.length;
  while (lo < hi) {
    const mid = (lo + hi) >>> 1;
    if (lineStarts[mid] <= offset) lo = mid + 1;
    else hi = mid;
  }
  return { line: lo, column: offset - lineStarts[lo - 1] };
}
