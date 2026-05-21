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

function encodeAst(ast) {
  return encode(ast.program, ast.lineStarts);
}

export function print(ast, options) {
  return binding.print(encodeAst(ast), normalizeOptions(options));
}

export function strip(ast, options) {
  return binding.strip(encodeAst(ast), normalizeOptions(options));
}

export function minify(ast, options) {
  return binding.minify(encodeAst(ast), normalizeOptions(options));
}
