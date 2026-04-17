import { parse } from "yuku-parser";

const source = await Bun.file("test/index.ts").text();

parse(source, {
  lang: "js"
});parse(source, {
  lang: "js"
});parse(source, {
  lang: "js"
});parse(source, {
  lang: "js"
});parse(source, {
  lang: "js"
});parse(source, {
  lang: "js"
});parse(source, {
  lang: "js"
});parse(source, {
  lang: "js"
});parse(source, {
  lang: "js"
});parse(source, {
  lang: "js"
});

console.time('parse')
const result = parse(source, {
  lang: "js"
});
console.timeEnd('parse')

// console.log(JSON.stringify(result, null, 2))
