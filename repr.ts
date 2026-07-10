// Reproduction for issue #84 — "Parser crash with Bun on Windows".
// https://github.com/yuku-toolchain/yuku/issues/84
//
// Uses the workspace build of yuku-parser (npm/yuku-parser -> binding.js ->
// @yuku-parser/binding-win32-x64/yuku-parser.node). On Bun + Windows the very
// first call into the native binding segfaults; on Node it parses fine.
import { parse } from "yuku-parser";

const runtime = typeof (globalThis as any).Bun !== "undefined" ? "bun" : "node";
console.log(`runtime: ${runtime}`);

const code = "const a = 16; const b = 16;";
console.log("calling parse()...");

const result = parse(code);

// If we reach here, the native binding did not crash.
console.log("REPRO_OK: parsed without crashing");
console.log(`  program.type = ${result.program.type}`);
console.log(`  statements   = ${result.program.body.length}`);
