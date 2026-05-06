import mod from "yuku-strip";

const source = await Bun.file("test/index.ts").text();

console.time("strip");
const { code } = mod.strip(source, {
  lang: "ts",
});
console.log(code);
console.timeEnd("strip");
