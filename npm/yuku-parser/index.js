import binding from './binding.js';
import { decode } from './decode.js';

export function parseSync(source, options) {
  const buffer = binding.parseSync(source, options ?? {});
  return decode(buffer, source);
}

export async function parse(source, options) {
  const buffer = await binding.parse(source, options ?? {});
  return decode(buffer, source);
}
