import { parse } from "yuku-parser";

const src = `
  function f() { /* hi */ }
`;

const r = parse(src, { attachComments: true });
console.log(JSON.stringify(r, null, 2));
