import binding from "./binding.js";
import { encode } from "./encode.js";

function normalizeOptions(options) {
  const { minify, sourceMaps, ...next } = options ?? {};
  if (typeof next.comments === "boolean") next.comments = next.comments ? "all" : "none";
  const m = minify === true ? { whitespace: true, syntax: true, quotes: true } : minify || {};
  next.minify = !!m.syntax;
  if (m.whitespace) next.format = "compact";
  if (m.quotes) next.quotes = "shortest";
  next.sourceMap ??= sourceMaps;
  return next;
}

export function generate(program, options) {
  if (!program || !program.type) {
    throw new TypeError(
      "Expected a `Program` node from yuku-parser, got " +
        (program === null ? "null" : typeof program),
    );
  }
  return binding.generate(encode(program), normalizeOptions(options));
}

/** @deprecated Use `generate(program, options)`. */
export function print(program, options) {
  return generate(program, options);
}

/** @deprecated Use `generate(program, { strip: true })`. */
export function strip(program, options) {
  return generate(program, { ...options, strip: true });
}

/** @deprecated Use `generate(program, { minify: true })`. */
export function minify(program, options) {
  return generate(program, { ...options, minify: { syntax: true, quotes: true } });
}
