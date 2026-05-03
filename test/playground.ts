import { parse } from "yuku-parser";

const source = await Bun.file("test/index.ts").text();

console.time('parse')
const {program: _, diagnostics} = parse(source, {
  lang: "ts",
  semanticErrors: true
});
console.timeEnd('parse')

console.log(JSON.stringify(diagnostics, null, 2))
// console.log(JSON.stringify(_, null, 2))
