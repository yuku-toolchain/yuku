import { parse } from "yuku-parser";

const source = await Bun.file("test/index.ts").text();

const result = parse(source, {
  lang: "ts"
});

console.log(JSON.stringify(result, null, 2))
