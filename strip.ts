import mod from "yuku-strip";

const source = await Bun.file("input.js").text();

console.time("strip");
const { code, map } = mod.strip(source, {
  lang: "js",
  sourcemap: "v3",
  format: "compact"
});
await Bun.write("generated.js", code);
await Bun.write("sourcemap.json", JSON.stringify(map, null, 2));
console.timeEnd("strip");
