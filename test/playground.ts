import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const source = await Bun.file("test/fixture.ts").text();

const ast = parse(source, { lang: "ts" });

const result = print(ast.program, {
  sourceMaps: {
    source,
    file: "fixture.js",
    sourceFileName: "fixture.ts",
    sourcesContent: true,
  },
});

console.log(result.code);
