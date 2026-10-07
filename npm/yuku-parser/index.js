import { fileOptions, load } from "yuku-core";
import { decode } from "./decode.js";

export { langFromPath, sourceTypeFromPath } from "yuku-core";
export { TokenKind } from "./decode.js";

const _dec = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

export function parse(source, options = {}) {
  const text = typeof source === "string" ? source : _dec.decode(source);
  const core = options.core ?? load();
  return decode(core.parse(text, fileOptions(options)), text, options.path ?? null);
}
