import { parse } from "yuku-parser"
import { print } from "yuku-codegen"

const result = parse("const nice = -1.0\nnice-=10 // cool", {
  attachComments: true
});

console.log(print(result, {
  format: "compact",
}).code);
