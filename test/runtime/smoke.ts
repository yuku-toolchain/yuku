// CI runs this, compiled to JavaScript, on the minimum supported runtime

import { strict as assert } from "node:assert";
import { createRequire } from "node:module";
import { analyze } from "yuku-analyzer";
import { walk } from "yuku-ast";
import { generate } from "yuku-codegen";
import { parse } from "yuku-parser";

const source = "const answer: number = 42;\nlet count = 0;\ncount++;\n";

const { program, diagnostics } = parse(source, { lang: "ts" });
assert.deepEqual(diagnostics, []);
assert.equal(
  generate(program, { strip: true }).code,
  "const answer = 42;\nlet count = 0;\ncount++;",
);

let identifiers = 0;
walk(program, {
  Identifier() {
    identifiers += 1;
  },
});
assert.equal(identifiers, 3);

assert.equal(analyze(source, { lang: "ts" }).rootScope.find("count")?.references.length, 1);

const loaded = Object.keys(createRequire(import.meta.url).cache);
assert.ok(
  loaded.some((path) => path.endsWith("yuku-core.node")),
  "the native binary did not load",
);

console.log(`every package runs on ${process.version}`);
