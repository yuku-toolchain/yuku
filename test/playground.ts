import { parse } from "yuku-parser";
import { generate } from "astring";

const source = await Bun.file("test/fixture.ts").text();

const ast = parse(source, { lang: "ts" });

console.time("gen");
generate(ast.program);
console.timeEnd("gen");
