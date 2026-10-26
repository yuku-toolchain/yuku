// CI runs this, compiled to JavaScript, on the minimum supported runtime

import { strict as assert } from "node:assert";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { Core, parse, print, walk } from "yuku";

const yuku = createRequire(createRequire(import.meta.url).resolve("yuku/package.json"));
const addon = installed(`@yuku-toolchain/${process.platform}-${process.arch}`);
const wasm = new WebAssembly.Module(readFileSync(yuku.resolve("@yuku-toolchain/wasm/yuku.wasm")));
const core = new Core({ wasm });

const dlopen = process.dlopen;
let loaded = false;
process.dlopen = (module: { exports: unknown }, path: string) => {
  dlopen.call(process, module, path);
  loaded = true;
};

const source = "const answer: number = 42;\nlet count = 0;\ncount++;\n";

const file = parse(source, { lang: "ts", semantic: true });
assert.equal(loaded, addon);
const fallback = parse(source, { lang: "ts", semantic: true, core });
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

function installed(name: string): boolean {
  try {
    yuku.resolve(name);
    return true;
  } catch {
    return false;
  }
}
