import { parse } from "yuku-parser";

const source = await Bun.file("test/index.ts").text();

console.time('parse')
const result = parse(source, {
  lang: "tsx",
  semanticErrors: true
});
console.timeEnd('parse')

console.log(JSON.stringify(result.diagnostics, null, 2))
// console.log(JSON.stringify(result.program, null, 2))
