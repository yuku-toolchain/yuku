const binding = require('./binding.js');
const { decode } = require('./decode.js');

function parse(source, options) {
  const buffer = binding.parse(source, options);
  return decode(buffer, source);
}

module.exports = { parse };
