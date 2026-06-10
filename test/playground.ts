import { parse } from "yuku-parser";

console.log(
  JSON.stringify(
    parse("const [d!] = xs;", {
      lang: "ts",
    }),
    null,
    2,
  ),
);
