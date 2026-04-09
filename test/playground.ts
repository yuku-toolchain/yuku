import { printDiagnostics } from "./ast-helpers-for-test";
import { parse } from "yuku-parser";

const source = await Bun.file("test/index.js").text();

const result = parse(source, {
  sourceType: "module",
});

console.log();

printDiagnostics(source, result.diagnostics, "test.js");
