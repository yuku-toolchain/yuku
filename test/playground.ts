import { minify } from "yuku-minify";

const source = await Bun.file("test/fixture.ts").text();

const { code, errors } = minify(source, {
  lang: "ts",
});

console.log(code.length);
console.log(errors);
