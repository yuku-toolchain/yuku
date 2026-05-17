import binding from "./binding.js";
import { encode } from "./encode.js";

const DIRTY = Symbol.for("yuku.dirty");
const BUFREF = Symbol.for("yuku.bufRef");
const SOURCE = Symbol.for("yuku.source");
const RAW = Symbol.for("yuku.raw");

export function generate(estree, options) {
  const opts = options ?? {};
  if (estree == null || typeof estree !== "object") {
    return binding.generate(encode(estree), "", opts);
  }
  const cached = estree[BUFREF];
  const source = estree[SOURCE] ?? "";
  if (cached) {
    const d = estree[DIRTY];
    if (d && !d.self && !d.subtree) {
      return binding.generate(cached, source, opts);
    }
    return binding.generate(encode(estree[RAW] ?? estree, cached), source, opts);
  }
  return binding.generate(encode(estree[RAW] ?? estree), "", opts);
}
