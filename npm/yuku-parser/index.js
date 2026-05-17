import binding from "./binding.js";
import { decode } from "./decode.js";
import { wrap } from "./proxy.js";

const BUFREF = Symbol.for("yuku.bufRef");
const SOURCE = Symbol.for("yuku.source");

export function parse(source, options) {
  const buffer = binding.parse(source, options ?? {});
  const result = decode(buffer, source);
  const decodeProgram = Object.getOwnPropertyDescriptor(result, "program").get;
  let wrapped;
  Object.defineProperty(result, "program", {
    get() {
      if (wrapped) return wrapped;
      const raw = decodeProgram.call(result);
      Object.defineProperty(raw, BUFREF, { value: buffer, configurable: true });
      Object.defineProperty(raw, SOURCE, { value: source, configurable: true });
      return (wrapped = wrap(raw));
    },
    configurable: true,
  });
  return result;
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
