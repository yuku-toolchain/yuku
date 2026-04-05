const { platform, arch } = process;
const path = require('path');

function isMusl() {
  try {
    const report = process.report.getReport();
    const header = typeof report === 'string' ? JSON.parse(report).header : report.header;
    return !header.glibcVersionRuntime;
  } catch {
    return false;
  }
}

function loadBinding() {
  const libc = platform === 'linux' ? (isMusl() ? '-musl' : '-gnu') : '';
  const suffix = `${platform}-${arch}${libc}`;
  const localPath = path.join(__dirname, '@yuku-parser', 'binding-' + suffix, 'yuku-parser.node');

  // try local path first (development), then npm-installed package
  try {
    return require(localPath);
  } catch {}

  try {
    return require('@yuku-parser/binding-' + suffix + '/yuku-parser.node');
  } catch (e) {
    throw new Error(
      `Failed to load native binding for ${platform}-${arch}.\n` +
      `Error: ${e.message}`
    );
  }
}

module.exports = loadBinding();
