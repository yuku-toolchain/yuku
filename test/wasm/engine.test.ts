// The WebAssembly engine against the native one. Both must return byte-identical parse and
// analyze buffers.

import { describe, expect, test } from "bun:test";
import { readFileSync } from "node:fs";
import * as wasm from "@yuku-engine/wasm";
import { load } from "yuku-engine/binding.js";
import { corpusFiles, corpusPresent } from "../corpus";
import { jsSource, tsSource } from "./sources";

const native = load();
const encoder = new TextEncoder();

const OPTION_SETS = [
  {},
  { attachComments: true, tokens: true, semanticErrors: true },
  { preserveParens: false, sourceType: "script" },
] as const;

function differences(source: string, options: object): string[] {
  const bytes = encoder.encode(source);
  const found: string[] = [];
  for (const entry of ["parse", "analyze"] as const) {
    const expected = Buffer.from(native[entry](bytes, options));
    if (!expected.equals(Buffer.from(wasm[entry](bytes, options)))) found.push(entry);
  }
  return found;
}

describe("the WebAssembly engine matches the native engine", () => {
  test("on JavaScript and TypeScript samples under every option set", () => {
    for (const options of OPTION_SETS) {
      expect(differences(jsSource, { ...options, lang: "js" })).toEqual([]);
      expect(differences(tsSource, { ...options, lang: "ts" })).toEqual([]);
    }
  });

  test.skipIf(!corpusPresent())(
    "on every corpus file",
    () => {
      const mismatched: string[] = [];
      for (const file of corpusFiles()) {
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
