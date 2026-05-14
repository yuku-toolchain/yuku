import { minify } from "yuku-minify";
import { minifySync } from "oxc-minify";

const source = await Bun.file("test/fixture.ts").text();

console.time('yuku')
const {code} = minify(source, {
  lang: "ts",
}
);
console.timeEnd('yuku')

console.time('oxc')
const { code: codeOxc } = minifySync("test/fixture.ts", source, {
  compress: false,
  mangle: true,
  sourcemap: false,
});
console.timeEnd('oxc')

console.log(code.length);
console.log(codeOxc.length);
