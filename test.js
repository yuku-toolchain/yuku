const mod = require("./zig-out/lib/yuku-parser.node")

const source = await Bun.file("./cool.js").text()

console.time("parse")
await mod.parse(source)
console.timeEnd("parse")
