import { parse as parseSource } from "yuku-engine";
import { decode } from "./decode.js";

export { langFromPath, sourceTypeFromPath } from "yuku-engine";
export { TokenKind } from "./decode.js";

const _dec = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

export function parse(source, options = {}) {
  const text = typeof source === "string" ? source : _dec.decode(source);
  return decode(parseSource(source, options), text, options.path ?? null);
}
