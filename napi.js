const binding = require("./zig-out/lib/yuku-parser.node");
const { decode } = require("./decode.js");

const source = await Bun.file('test/index.js').text();

// warmup
binding.parse(source, { lang: "js" });
decode(binding.parse(source, { lang: "js" }), source);

console.log(`file: ${(source.length / 1024).toFixed(1)} KB`);

console.time("  native (parse + serialize + decode)");

console.time("  native (parse + serialize)");
const buffer = binding.parse(source, { lang: "js" });
console.timeEnd("  native (parse + serialize)");

console.time("  js decode");
const result = decode(buffer, source);
console.log(JSON.stringify(result.program.body, null, 2))
console.timeEnd("  js decode");

console.timeEnd("  native (parse + serialize + decode)");

console.log(`buffer: ${(buffer.byteLength / 1024).toFixed(1)} KB, nodes in body: ${result.program.body.length}`);
