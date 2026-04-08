import binding from './binding.js';
import { decode } from './decode.js';

export function parse(source, options) {
  const buffer = binding.parseSync(source, options ?? {});
  return decode(buffer, source);
}
