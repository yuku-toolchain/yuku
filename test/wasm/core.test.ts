import { describe, expect, test } from "bun:test";
import { readFileSync } from "node:fs";
import * as wasmCore from "@yuku-core/wasm";
import { analyze } from "yuku-analyzer";
import { fileOptions, load } from "yuku-core";
import { parse } from "yuku-parser";
import { load as loadInBrowser } from "../../npm/yuku-core/browser.js";
import { corpusFiles, corpusPresent, projectFiles } from "../corpus";
import { jsSource, tsSource } from "./sources";

const native = load();
const wasm = await wasmCore.load();
const bytes = readFileSync(new URL("../../npm/yuku-core-wasm/yuku-core.wasm", import.meta.url));

const OPTION_SETS = [
  {},
  { attachComments: true, tokens: true, semanticErrors: true },
  { preserveParens: false, sourceType: "script" },
] as const;

function differences(source: string, options: object): string[] {
  const resolved = fileOptions(options);
  const found: string[] = [];
  for (const entry of ["parse", "analyze"] as const) {
    const expected = Buffer.from(native[entry](source, resolved));
    if (!expected.equals(Buffer.from(wasm[entry](source, resolved)))) found.push(entry);
  }
  return found;
}

describe("the WebAssembly core matches the native core", () => {
  test("on JavaScript and TypeScript samples under every option set", () => {
    for (const options of OPTION_SETS) {
      expect(differences(jsSource, { ...options, lang: "js" })).toEqual([]);
      expect(differences(tsSource, { ...options, lang: "ts" })).toEqual([]);
    }
  });

  test("on strings of every UTF-16 sequence, lone surrogates read as U+FFFD", () => {
    const text = `é € 😀 \ud800 \udc00 ${"ascii past a chunk ".repeat(4)}😀`;
    expect(differences(`let s = "${text}";\nlet x = ;`, { lang: "ts" })).toEqual([]);
  });

  test.skipIf(!corpusPresent())(
    "on every corpus and project file",
    () => {
      const mismatched: string[] = [];
      for (const file of [...corpusFiles(), ...projectFiles()]) {
        const options = {
          lang: file.lang,
          sourceType: file.sourceType,
          attachComments: true,
          tokens: true,
        };
        const found = differences(readFileSync(file.path, "utf8"), options);
        if (found.length > 0) mismatched.push(`${file.path} ${found.join(", ")}`);
      }
      expect(mismatched.slice(0, 5)).toEqual([]);
    },
    300_000,
  );
});

describe("loading the WebAssembly core", () => {
  const options = fileOptions({ lang: "ts" });
  const expected = Buffer.from(native.parse(tsSource, options));

  test("from every source", async () => {
    const response = (type: string) =>
      new Response(bytes, { headers: { "content-type": type } });
    const cores = [
      await wasmCore.load(bytes),
      await wasmCore.load(response("application/wasm")),
      await wasmCore.load(response("application/octet-stream")),
      await wasmCore.load(new WebAssembly.Module(bytes)),
      wasmCore.loadSync(bytes),
      wasmCore.loadSync(new WebAssembly.Module(bytes)),
    ];
    for (const core of cores) expect(Buffer.from(core.parse(tsSource, options))).toEqual(expected);
  });

  test("fails on a failed response", async () => {
    const response = new Response("", { status: 404 });
    await expect(wasmCore.load(response)).rejects.toThrow("the response has status 404");
  });

  test("fails on bytes that are not WebAssembly", () => {
    expect(() => wasmCore.loadSync(new Uint8Array([0, 1, 2, 3]))).toThrow();
  });
});

describe("the packages run on the core they are given", () => {
  test("the parser", () => {
    expect(parse(tsSource, { lang: "ts", core: wasm })).toEqual(parse(tsSource, { lang: "ts" }));
  });

  test("the analyzer", () => {
    const bindings = (core?: typeof wasm) =>
      analyze(tsSource, { path: "a.ts", core }).rootScope.bindings.map((binding) => binding.name);
    expect(bindings(wasm)).toEqual(bindings());
    expect(bindings(wasm).length).toBeGreaterThan(0);
  });

  test("a runtime without the native core asks for one", () => {
    expect(() => loadInBrowser()).toThrow("@yuku-core/wasm");
  });
});
