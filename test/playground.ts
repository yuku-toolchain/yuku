import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

// non-ascii (é) on line 1 exercises the utf-16 column mapping.
const source = `const gréeting = "hi";
function add(a, b) {
  return a + b;
}
`;

const { program, diagnostics, ...rest } = parse(source);
console.log("parse result keys:", Object.keys({ program, diagnostics, ...rest }));

// source maps take the original `source` to resolve generated positions.
const withMap = print(program, {
  sourceMaps: {
    source,
    file: "out.js",
    sourceFileName: "in.js",
    sourcesContent: source,
  },
});
console.log("\n--- code ---\n" + withMap.code);
console.log("--- map ---");
console.log(JSON.stringify(withMap.map, null, 2));

// no source provided -> no source map.
const noMap = print(program, { sourceMaps: { file: "out.js" } as never });
console.log("\nno-source map:", noMap.map);
