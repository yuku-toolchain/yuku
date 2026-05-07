import { strip } from "yuku-strip";

const source = await Bun.file("input.ts").text();

console.time("strip");
const { code } = strip(source, {
  lang: "ts",
});
await Bun.write("generated.js", code);
console.timeEnd("strip");
