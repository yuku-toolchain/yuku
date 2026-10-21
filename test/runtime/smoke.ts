// CI runs this, compiled to JavaScript, on the minimum supported runtime

import { strict as assert } from "node:assert";
import { parse, print, walk } from "yuku";

const source = "const answer: number = 42;\nlet count = 0;\ncount++;\n";

const file = parse(source, { lang: "ts", semantic: true });
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

console.log(`yuku runs on ${process.version}`);
