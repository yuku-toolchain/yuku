import binding from "./binding.js";
import { encode } from "./encode.js";

function normalizeOptions(options) {
  if (options == null) return {};
  const c = options.comments;
  if (c === true) return { ...options, comments: "all" };
  if (c === false) return { ...options, comments: "none" };
  return options;
}

function encodeAst(ast) {
  return encode(ast.program, ast.comments, ast.lineStarts);
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
