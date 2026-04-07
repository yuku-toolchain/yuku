import { createRequire } from 'node:module';
import { readFileSync } from 'node:fs';
import { execSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const require = createRequire(import.meta.url);
const __dirname = dirname(fileURLToPath(import.meta.url));
const { platform, arch } = process;

const isFileMusl = (f) => f.includes('libc.musl-') || f.includes('ld-musl-');

function isMusl() {
  if (platform !== 'linux') return false;

  try {
    if (readFileSync('/usr/bin/ldd', 'utf-8').includes('musl')) return true;
  } catch {}

  try {
    const report = typeof process.report?.getReport === 'function'
      ? process.report.getReport()
      : null;
    if (report) {
      const header = typeof report === 'string' ? JSON.parse(report).header : report.header;
      if (header?.glibcVersionRuntime) return false;
      if (Array.isArray(report.sharedObjects) && report.sharedObjects.some(isFileMusl)) return true;
    }
  } catch {}

  try {
    return execSync('ldd --version', { encoding: 'utf8' }).includes('musl');
  } catch {}

  return false;
}

function loadBinding() {
  const errors = [];
  const libc = platform === 'linux' ? (isMusl() ? '-musl' : '-gnu') : '';
  const suffix = `${platform}-${arch}${libc}`;

  try {
    return require(join(__dirname, '@yuku-parser', 'binding-' + suffix, 'yuku-parser.node'));
  } catch (e) {
    errors.push(e);
  }

  try {
    return require('@yuku-parser/binding-' + suffix + '/yuku-parser.node');
  } catch (e) {
    errors.push(e);
  }

  throw new Error(
    `Failed to load native binding for ${platform}-${arch}.\n` +
    `If this persists, try removing node_modules and reinstalling.\n` +
    errors.map(e => `  - ${e.message}`).join('\n'),
    { cause: errors[errors.length - 1] }
  );
}

export default loadBinding();
