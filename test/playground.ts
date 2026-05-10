import { minify } from "yuku-minify";

const source = await Bun.file("test/fixture.ts").text();

const { code, errors } = minify(source, {
  lang: "ts",
});

console.log(code);
console.log(errors);
