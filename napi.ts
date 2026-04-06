import { parse } from "yuku-parser"

const source = await Bun.file("test/index.js").text()

console.time("parse")
await parse(source)
console.timeEnd("parse")
