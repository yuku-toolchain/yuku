import { analyze as analyzeSource, parse as parseSource } from "#engine";

const LANGS = ["js", "jsx", "ts", "tsx", "dts"];
const SOURCE_TYPES = ["module", "script", "commonjs"];

export function parse(source, options) {
  return parseSource(source, fileOptions(options));
}

export function analyze(source, options) {
  return analyzeSource(source, fileOptions(options));
}

export function langFromPath(path) {
  if (/\.d\.[cm]?ts$/.test(path)) return "dts";
  if (path.endsWith(".tsx")) return "tsx";
  if (/\.[cm]?ts$/.test(path)) return "ts";
  if (path.endsWith(".jsx")) return "jsx";
  return "js";
}

export function sourceTypeFromPath(path) {
  return /\.c[jt]s$/.test(path) ? "commonjs" : "module";
}

function fileOptions(options) {
  const path = options.path ?? "";
  const lang = options.lang ?? langFromPath(path);
  if (!LANGS.includes(lang)) {
    throw new TypeError('`lang` must be "js", "jsx", "ts", "tsx", or "dts"');
  }
  const sourceType = options.sourceType ?? sourceTypeFromPath(path);
  if (!SOURCE_TYPES.includes(sourceType)) {
    throw new TypeError('`sourceType` must be "module", "script", or "commonjs"');
  }
  return { ...options, lang, sourceType };
}
