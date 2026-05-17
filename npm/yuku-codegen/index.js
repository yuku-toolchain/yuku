import binding from "./binding.js";
import { encode } from "./encode.js";

const DIRTY = Symbol.for("yuku.dirty");
const BUFREF = Symbol.for("yuku.bufRef");
const SOURCE = Symbol.for("yuku.source");
const RAW = Symbol.for("yuku.raw");

function run(op, estree, options) {
  const opts = options ?? {};
  if (estree == null || typeof estree !== "object") {
    return op(encode(estree), "", opts);
  }
  const cached = estree[BUFREF];
  const source = estree[SOURCE] ?? "";
  if (cached) {
    const d = estree[DIRTY];
    if (d && !d.self && !d.subtree) return op(cached, source, opts);
    return op(encode(estree[RAW] ?? estree, cached), source, opts);
  }
  return op(encode(estree[RAW] ?? estree), "", opts);
}

export function print(estree, options) {
  return run(binding.print, estree, options);
}

export function strip(estree, options) {
  return run(binding.strip, estree, options);
}

export function minify(estree, options) {
  return run(binding.minify, estree, options);
}
