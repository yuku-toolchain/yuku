import { parse } from "../npm/parser-wasm/dist";

console.clear();

console.log();

const result = await parse(
	`function* a(){ (b = yield* c) => 1; } const a = cool`,
	{
		semanticErrors: true,
	},
);

console.log(JSON.stringify(result, null, 2));

console.log();
