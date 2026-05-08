import { minify } from "yuku-minify";

const source = await Bun.file("test/fixture.ts").text();

const { code, errors } = minify(' var x = 1; var x = 2; return x;', {
  lang: "ts",
});

console.log(code);
console.log(errors);
