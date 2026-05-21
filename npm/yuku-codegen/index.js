import binding from "./binding.js";
import { encode } from "./encode.js";

const _kLineStarts = Symbol.for("yuku-parser.lineStarts");

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

function prepare(input, options) {
  const root = input && input.type ? input : input?.program;
  if (!root || !root.type) {
    throw new TypeError(
      "Expected an AST node or `ParseResult`, got " +
        (input === null ? "null" : typeof input),
    );
  }
  if (options?.sourceMaps && !root[_kLineStarts]) {
    throw new Error(
      "Source maps require an AST produced by yuku-parser. " +
        "The input has no line-start info attached. If this AST was built " +
        "or transformed outside of yuku-parser, drop `sourceMaps` or " +
        "re-parse with yuku-parser first.",
    );
  }
  return encode(root);
}

export function print(input, options) {
  return binding.print(prepare(input, options), normalizeOptions(options));
}

export function strip(input, options) {
  return binding.strip(prepare(input, options), normalizeOptions(options));
}

export function minify(input, options) {
  return binding.minify(prepare(input, options), normalizeOptions(options));
}
