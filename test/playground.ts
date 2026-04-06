import { printDiagnostics } from "./ast-helpers-for-test";
import { parse } from "yuku-parser";

console.clear();

console.log();

const source = await Bun.file("test/index.js").text();

const result = await parse(source, {
  sourceType: "module",
	semanticErrors: true,
});

console.log(result.program);

console.log();

printDiagnostics(source, result.diagnostics, "test.js");
