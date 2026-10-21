import { expect, test } from "bun:test";
import { readFileSync } from "node:fs";
import { parse, walk } from "yuku";

// Yuku with src/parser/testing/dialect.zig compiled in, which test:dialect builds
const bytes = readFileSync(new URL("../../zig-out/dialect/bin/yuku.wasm", import.meta.url));
const wasm = new WebAssembly.Module(bytes);
const source = "each (item of items) { assert item |> f(%); }";

test("dialect nodes decode in the shapes their dialect declares", () => {
  const { program, diagnostics } = parse(source, { wasm });
  expect(diagnostics).toEqual([]);
  expect(program.body[0]).toMatchObject({
    type: "EachStatement",
    item: { type: "Identifier", name: "item" },
    list: { type: "Identifier", name: "items" },
    body: [
      {
        type: "AssertStatement",
        argument: {
          type: "PipelineExpression",
          operator: "|>",
          left: { type: "Identifier", name: "item" },
          right: { type: "CallExpression", arguments: [{ type: "TopicReference" }] },
        },
      },
    ],
  });
});

test("dialect nodes walk and analyze like standard ones", () => {
  const types: string[] = [];
  walk(parse(source, { wasm }).program, {
    enter(node) {
      types.push(node.type);
    },
  });
  expect(types).toEqual([
    "Program",
    "EachStatement",
    "Identifier",
    "Identifier",
    "AssertStatement",
    "PipelineExpression",
    "Identifier",
    "CallExpression",
    "Identifier",
    "TopicReference",
  ]);
  const file = parse(source, { wasm, semantic: true });
  expect(file.diagnostics).toEqual([]);
  const resolved = file.references.map((reference) => [reference.name, reference.binding?.name]);
  expect(resolved).toEqual([
    ["items", undefined],
    ["item", "item"],
    ["f", undefined],
  ]);
});

test("the dialect reads its own files, and other files and a given lang parse as standard", () => {
  expect(parse(source, { wasm, path: "a.jsx" }).diagnostics).toEqual([]);
  expect(parse(source, { wasm, path: "a.js" }).diagnostics.length).toBeGreaterThan(0);
  expect(parse(source, { wasm, lang: "jsx" }).diagnostics.length).toBeGreaterThan(0);
});
