import { printDiagnostics } from "../npm/parser-types/dist/print";
import { parse } from "../npm/parser-wasm/dist";

console.clear();

console.log();

const source = `
  function* (blue){
    const blue = "nice"
  };
`;

const result = await parse(source, {
	semanticErrors: true,
});

console.log(result.program);

console.log();

printDiagnostics(source, result.errors, "test.js");
