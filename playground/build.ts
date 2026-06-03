import { cpSync, mkdirSync, rmSync } from "node:fs";

const dir = import.meta.dir;
const npm = `${dir}/../npm`;
const dist = `${dir}/dist`;

rmSync(dist, { recursive: true, force: true });
mkdirSync(`${dist}/pkg/yuku-parser-wasm`, { recursive: true });
mkdirSync(`${dist}/pkg/yuku-codegen-wasm`, { recursive: true });

for (const f of ["index.html", "style.css", "main.js"]) {
  cpSync(`${dir}/${f}`, `${dist}/${f}`);
}
cpSync(`${dir}/fonts`, `${dist}/fonts`, { recursive: true });

for (const f of ["index.js", "decode.js", "yuku-parser.wasm"]) {
  cpSync(`${npm}/yuku-parser-wasm/${f}`, `${dist}/pkg/yuku-parser-wasm/${f}`);
}
for (const f of ["index.js", "encode.js", "yuku-codegen.wasm"]) {
  cpSync(`${npm}/yuku-codegen-wasm/${f}`, `${dist}/pkg/yuku-codegen-wasm/${f}`);
}

console.log(`built ${dist}`);
