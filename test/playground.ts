import { printDiagnostics } from "../npm/parser-types/dist/print";
import { parse } from "../npm/parser-wasm/dist";

console.clear();

console.log();

const source = `
  function foo(a, b, c) {
    arguments[0] = 1;
    arguments[1] = 'str';
    arguments[2] = 2.1;
    return 10 === a && 'sss' === b && 1 === c;
  }
`;

const result = await parse(source, {
	semanticErrors: true,
});

console.log(result.program);

console.log();

printDiagnostics(source, result.diagnostics, "test.js");
