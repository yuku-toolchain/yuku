import { parse } from "yuku-parser";

const source = await Bun.file("test/index.ts").text();

// warmups

parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});
parse(source, {
  lang: "ts",
});

console.time('parse')
const {program: _} = parse(source, {
  lang: "ts",
});
console.timeEnd('parse')

// console.log(JSON.stringify(result.diagnostics, null, 2))
// console.log(JSON.stringify(result.program, null, 2))
