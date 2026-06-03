import binding from "./binding.js";
import { encode } from "./encode.js";

function normalizeOptions(options) {
  if (options == null) return {};
  const next = { ...options };
  const c = next.comments;
  if (c === true) next.comments = "all";
  else if (c === false) next.comments = "none";
  const s = next.sourceMaps;
  if (s) {
    const { lineStarts: _, ...meta } = s;
    next.sourceMaps = meta;
  } else {
    next.sourceMaps = undefined;
  }
  return next;
}

function run(method, program, options) {
  if (!program || !program.type) {
    throw new TypeError(
      "Expected a `Program` node from yuku-parser, got " +
        (program === null ? "null" : typeof program),
    );
  }
  const sourceMaps = options?.sourceMaps;
  const lineStarts = sourceMaps ? sourceMaps.lineStarts : null;
  if (sourceMaps && !Array.isArray(lineStarts)) {
    throw new Error(
      "`sourceMaps` requires a `lineStarts` array. Pass the parser's " +
        "`ParseResult.lineStarts`, e.g. `{ sourceMaps: { lineStarts } }`.",
    );
  }
  return binding[method](encode(program, lineStarts), normalizeOptions(options));
}

export function print(program, options) {
  return run("print", program, options);
}

export function strip(program, options) {
  return run("strip", program, options);
}

export function minify(program, options) {
  return run("minify", program, options);
}
