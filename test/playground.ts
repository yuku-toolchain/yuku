import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const src = `
  function f() {
    /* hi */
    const x = 42;
    return // Wow x;
  }
`;

const r = parse(src, { attachComments: true });
console.log(print(r.program, {
  comments: true,
  sourceMaps: {}
}).map)
