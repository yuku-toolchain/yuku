const binding = require("./zig-out/lib/yuku-parser.node");
const { decode } = require("./decode.js");

const source = await Bun.file('test/index.js').text();

// warmup
// binding.parse(source, { lang: "js" });
// decode(binding.parse(source, { lang: "js" }), source);

console.time("total (parse + serialize + decode)");

console.time("  native (parse + serialize)");
const buffer = binding.parse(source, { lang: "js" });
console.timeEnd("  native (parse + serialize)");

console.time("  js decode");
const result = decode(buffer, source);
console.timeEnd("  js decode");

console.timeEnd("total (parse + serialize + decode)");

console.log(`buffer size: ${(buffer.byteLength / 1024 / 1024).toFixed(2)} MB`);
console.log(`nodes in program.body: ${result.program.body.length}`);
