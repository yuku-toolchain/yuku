import binding from './binding.js';
import { decode } from './decode.js';

export function parse(source, options) {
  const buffer = binding.parse(source, options ?? {});
  return decode(buffer, source);
}
