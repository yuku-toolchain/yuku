// Test harness for the minimal N-API addon (issue #84).
// Runs ONE variant per process (argv[2]) so that when Bun segfaults we know
// exactly which step triggered it. Usage: `bun experiment/minimal.ts <which>`
//   load   - require the .node, don't call anything (tests module load/init)
//   hello  - call hello()      (no args, no alloc, no ArrayBuffer)
//   strlen - call strLen("..") (string arg -> arena allocation)
//   buffer - call makeBuffer() (returns a napi ArrayBuffer)
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const which = process.argv[2] ?? "load";
const runtime = typeof (globalThis as any).Bun !== "undefined" ? "bun" : "node";
console.log(`runtime=${runtime} test=${which}`);

// Non-npm addon: napi-zig installs it to zig-out/lib/<name>.node.
const binding = require("../zig-out/lib/yuku-minimal.node");
console.log("LOADED: minimal addon required OK");

switch (which) {
  case "load":
    console.log("OK load: required without calling into native code");
    break;
  case "hello":
    console.log("OK hello:", binding.hello());
    break;
  case "strlen":
    console.log("OK strlen:", binding.strLen("hello world"));
    break;
  case "buffer": {
    const ab = binding.makeBuffer();
    console.log("OK buffer: byteLength =", ab.byteLength, "first =", new Uint8Array(ab)[0]);
    break;
  }
  default:
    throw new Error(`unknown variant: ${which}`);
}
