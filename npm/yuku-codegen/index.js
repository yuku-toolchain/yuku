import binding from "./binding.js";
import { encode } from "./encode.js";

function normalizeOptions(options) {
  if (options == null) return {};
  const next = { ...options };
  const c = next.comments;
  if (c === true) next.comments = "all";
  else if (c === false) next.comments = "none";
  const s = next.sourceMaps;
  if (s === true) next.sourceMaps = {};
  else if (s === false) next.sourceMaps = undefined;
  return next;
}

function prepare(result, options) {
  if (!result || !result.program || !result.program.type) {
    throw new TypeError(
      "Expected a `ParseResult` from yuku-parser, got " +
        (result === null ? "null" : typeof result),
    );
  }
  if (options?.sourceMaps && !result.lineStarts) {
    throw new Error(
      "Source maps require a `ParseResult` with `lineStarts`. " +
        "If you built or transformed this result outside of yuku-parser, " +
        "either drop `sourceMaps` or attach a `lineStarts` array.",
    );
  }
  return encode(result);
}

export function print(result, options) {
  return binding.print(prepare(result, options), normalizeOptions(options));
}

export function strip(result, options) {
  return binding.strip(prepare(result, options), normalizeOptions(options));
}

export function minify(result, options) {
  return binding.minify(prepare(result, options), normalizeOptions(options));
}
