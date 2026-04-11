import { printDiagnostics } from "./ast-helpers-for-test";
import { parse } from "yuku-parser";

const source = await Bun.file("test/index.js").text();

console.log("--- without allowReturnOutsideFunction ---");
const r1 = parse(source);
printDiagnostics(source, r1.diagnostics, "test.js");

console.log("--- with allowReturnOutsideFunction ---");
const r2 = parse(source, { allowReturnOutsideFunction: true });
printDiagnostics(source, r2.diagnostics, "test.js");
