// CI runs this, compiled to JavaScript, on the minimum supported runtime

import { strict as assert } from "node:assert";
import { existsSync, readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { parse, print, walk } from "yuku";

const require = createRequire(import.meta.url);
const dist = require.resolve("yuku/package.json").replace(/package\.json$/, "dist");
const addon = `${dist}/native/${process.platform}-${process.arch}.node`;
const wasm = new WebAssembly.Module(readFileSync(require.resolve("yuku/yuku.wasm")));

const dlopen = process.dlopen;
let loaded = false;
process.dlopen = (module: { exports: unknown }, path: string) => {
  dlopen.call(process, module, path);
  loaded = true;
};

const source = "const answer: number = 42;\nlet count = 0;\ncount++;\n";

const file = parse(source, { lang: "ts", semantic: true });
assert.equal(loaded, existsSync(addon));
const fallback = parse(source, { lang: "ts", semantic: true, wasm });
assert.equal(JSON.stringify(fallback.program), JSON.stringify(file.program));
assert.equal(fallback.rootScope.find("count")?.references.length, 1);
assert.deepEqual(file.diagnostics, []);
assert.equal(print(file, { strip: true }).code, "const answer = 42;\nlet count = 0;\ncount++;");

let identifiers = 0;
walk(file.program, {
  Identifier() {
    identifiers += 1;
  },
});
assert.equal(identifiers, 3);

assert.equal(file.rootScope.find("count")?.references.length, 1);

console.log(`yuku runs on ${process.version}, ${loaded ? "natively and " : ""}as WebAssembly`);
