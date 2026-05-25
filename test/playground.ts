import { parse } from "yuku-parser"

const source = await Bun.file("test/fixture.ts").text()

console.time('stringify')
parse(source);
console.timeEnd('stringify')
