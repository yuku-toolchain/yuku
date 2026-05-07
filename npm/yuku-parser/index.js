import binding from "./binding.js";
import { decode } from "./decode.js";

export function parse(source, options) {
  const buffer = binding.parse(source, options ?? {});
  let decoded = null;

  const getDecoded = () => {
    if (decoded === null) {
      decoded = decode(buffer, source);
    }
    return decoded;
  };

  return {
    get program() {
      return getDecoded().program;
    },
    get comments() {
      return getDecoded().comments;
    },
    get diagnostics() {
      return getDecoded().diagnostics;
    },
  };
}

export function langFromPath(path) {
  if (path.endsWith(".d.ts") || path.endsWith(".d.mts") || path.endsWith(".d.cts")) return "dts";
  if (path.endsWith(".tsx")) return "tsx";
  if (path.endsWith(".ts") || path.endsWith(".mts") || path.endsWith(".cts")) return "ts";
  if (path.endsWith(".jsx")) return "jsx";
  return "js";
}

export function sourceTypeFromPath(path) {
  if (path.endsWith(".cjs") || path.endsWith(".cts")) return "script";
  return "module";
}
