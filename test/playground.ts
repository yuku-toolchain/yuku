import { parse } from "yuku-parser";
import { print } from "yuku-codegen";

const source = await Bun.file("test/fixture.ts").text();

const ast = parse(source, { lang: "ts" });

const result = print(ast);

console.log(result.code);
