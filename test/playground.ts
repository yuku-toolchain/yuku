import { printDiagnostics } from "./ast-helpers-for-test";
import { parse } from "yuku-parser";

console.clear();

console.log();

const source = await Bun.file("test/index.js").text();

parse(source, {
  sourceType: "module",
	// semanticErrors: true,
});

console.time("parse");
const result = parse(source, {
  sourceType: "module",
	// semanticErrors: true,
});
console.timeEnd("parse");

// console.log(result.program);

console.log();

printDiagnostics(source, result.diagnostics, "test.js");
