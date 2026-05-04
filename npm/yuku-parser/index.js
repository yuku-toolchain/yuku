import binding from "./binding.js";
import { decode } from "./decode.js";

export function parse(source, options) {
  const buffer = binding.parse(source, options ?? {});
  let decoded = null;

  const getDecoded = () => {
    if (decoded === null) {
      decoded = decode(buffer, source);
    }
    return decoded;
  };

  return {
    get program() {
      return getDecoded().program;
    },
    get comments() {
      return getDecoded().comments;
    },
    get diagnostics() {
      return getDecoded().diagnostics;
    },
  };
}
