import { parse } from "yuku-parser";

const source = await Bun.file("test/index.ts").text();

console.time('parse')
const result = parse(source, {
  lang: "ts"
});
console.timeEnd('parse')

console.log(JSON.stringify(result, null, 2))
