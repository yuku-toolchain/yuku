import { Analyzer } from "./analyzer.js";

export { Analyzer };
export { SymbolFlags, langFromPath, sourceTypeFromPath } from "./module.js";

export function analyze(source, options = {}) {
  const { path = "input.js", ...rest } = options;
  return new Analyzer().addFile(path, source, rest);
}
