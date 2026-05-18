import binding from "./binding.js";
import { encode } from "./encode.js";

export function print(estree, options) {
  return binding.print(encode(estree), options ?? {});
}

export function strip(estree, options) {
  return binding.strip(encode(estree), options ?? {});
}

export function minify(estree, options) {
  return binding.minify(encode(estree), options ?? {});
}
