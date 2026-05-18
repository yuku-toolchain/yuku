import { parse } from "yuku-parser";
import { minify } from "yuku-codegen";

const source = await Bun.file("test/fixture.ts").text();

const ast = parse(source, { lang: "ts" });

const result = minify(ast.program, {
  format: "compact",
  sourceMaps: {
    source,
  },
});

console.log(result.map);
