import { printDiagnostics } from "yuku-parser-types/print";
import { parse } from "yuku-parser-wasm";

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
