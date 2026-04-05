const binding = require("./zig-out/lib/yuku-parser.node");
const { decode } = require("./decode.js");

const source = await Bun.file('test/index.js').text();

// warmup
binding.parse(source, { lang: "js" });
decode(binding.parse(source, { lang: "js" }), source);

console.log(`file: ${(source.length / 1024 / 1024).toFixed(2)} MB`);

console.time("total");

console.time("  native (parse + serialize)");
const buffer = binding.parse(source, { lang: "js" });
console.timeEnd("  native (parse + serialize)");

console.time("  js decode");
const result = decode(buffer, source);
// console.log(JSON.stringify(result.program.body, null, 2));
console.timeEnd("  js decode");

console.timeEnd("total");

console.log(`buffer: ${(buffer.byteLength / 1024 / 1024).toFixed(2)} MB`);
console.log(`body: ${result.program.body.length}, comments: ${result.comments.length}, diags: ${result.diagnostics.length}`);
