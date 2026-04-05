const { platform, arch } = process;

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

  try {
    return require('@yuku-parser/binding-' + suffix + '/yuku-parser.node');
  } catch (e) {
    throw new Error(
      `Failed to load native binding for ${platform}-${arch}. ` +
      `Tried: @yuku-parser/binding-${suffix}/yuku-parser.node\n` +
      `Error: ${e.message}`
    );
  }
}

module.exports = loadBinding();
