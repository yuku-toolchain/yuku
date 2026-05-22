import { parse } from "yuku-parser";

const src = `
  function f() {
    const x = 42;
    return x;
  }
`;

const r = parse(src, { attachComments: true });

console.log(r.program)
