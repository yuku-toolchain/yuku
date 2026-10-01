import { parse as parseBytes } from "yuku-engine";
import { decode } from "./decode.js";

export { langFromPath, sourceTypeFromPath } from "yuku-engine";
export { TokenKind } from "./decode.js";

const _enc = new TextEncoder();
const _dec = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

export function parse(source, options = {}) {
  const text = typeof source === "string" ? source : _dec.decode(source);
  const bytes = typeof source === "string" ? _enc.encode(source) : source;
  return decode(parseBytes(bytes, options), text, options.path ?? null);
}
