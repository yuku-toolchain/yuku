import binding from "./binding.js";
import { encode } from "./encode.js";

function normalizeOptions(options) {
  if (options == null) return {};
  const next = { ...options };
  const c = next.comments;
  if (c === true) next.comments = "all";
  else if (c === false) next.comments = "none";
  if (next.sourceMaps == null) next.sourceMaps = undefined;
  return next;
}

function run(method, program, options) {
  if (!program || !program.type) {
    throw new TypeError(
      "Expected a `Program` node from yuku-parser, got " +
        (program === null ? "null" : typeof program),
    );
  }
  return binding[method](encode(program), normalizeOptions(options));
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
