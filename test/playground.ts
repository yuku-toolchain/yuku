import { printDiagnostics } from "yuku-parser-types/print";
import { parse } from "yuku-parser-wasm";

console.clear();

console.log();

const source = `
  import "./setup_FIXTURE.js";
  assert.sameValue(globalThis.evaluations.length, 0, "import defer does not trigger evaluation");
  Object.getOwnPropertySymbols(ns);
  assert(globalThis.evaluations.length > 0, "It triggers evaluation");
`;

const result = await parse(source, {
	lang: "js",
	sourceType: "script",
	semanticErrors: true,
});

console.log(result.program);

console.log();

printDiagnostics(source, result.diagnostics, "test.js");
