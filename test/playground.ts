import { parse } from "../npm/parser-wasm/dist";

console.clear();

console.log();

const result = await parse(`function* a(){ (b = yield* c) => 1; }`, {
	semanticErrors: true,
});

console.log(JSON.stringify(result.program, null, 2));

console.log();
