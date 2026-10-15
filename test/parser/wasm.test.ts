import { expect, test } from "bun:test";
import { readFileSync } from "node:fs";
import { parse } from "yuku";
import { decodeBase64 } from "../../packages/yuku/core/base64";

const shipped = readFileSync(new URL("../../packages/yuku/dist/yuku.wasm", import.meta.url));

test("the inlined module parses as the shipped yuku.wasm does", () => {
  const wasm = new WebAssembly.Module(shipped);
  const source = "const x: number = 1;";
  expect(parse(source, { lang: "ts", wasm })).toEqual(parse(source, { lang: "ts" }));
});

test("workerd and edge-light import yuku.wasm in the form their bundlers compile", () => {
  const manifest = new URL("../../packages/yuku/package.json", import.meta.url);
  const loaders = JSON.parse(readFileSync(manifest, "utf8")).imports["#module"];
  expect(Object.keys(loaders)).toEqual(["workerd", "edge-light", "default"]);
  expect(loaders["edge-light"]).toBe(loaders.workerd);
  const loader = readFileSync(new URL(loaders.workerd, manifest), "utf8");
  expect(loader).toContain('"../yuku.wasm?module"');
  expect(loader).not.toContain("new WebAssembly.Module");
});

test("the JavaScript base64 decoder matches Buffer", () => {
  for (let length = 0; length <= 64; length++) {
    const bytes = Buffer.from(Array.from({ length }, (_, i) => (i * 167 + length * 31) & 255));
    expect(Buffer.from(decodeBase64(bytes.toString("base64")))).toEqual(bytes);
  }
  expect(Buffer.from(decodeBase64(shipped.toString("base64")))).toEqual(shipped);
});

test("grown memory is released once it stops growing for ten seconds", () => {
  const wasm = new WebAssembly.Module(shipped);
  const Instance = WebAssembly.Instance;
  const now = Date.now;
  const made: WebAssembly.Instance[] = [];
  let clock = now();
  WebAssembly.Instance = function (module: WebAssembly.Module) {
    const instance = new Instance(module);
    made.push(instance);
    return instance;
  } as unknown as typeof WebAssembly.Instance;
  Date.now = () => clock;
  const bytesOf = (instance: WebAssembly.Instance) =>
    (instance.exports.memory as WebAssembly.Memory).buffer.byteLength;
  const large = "a = b + c;\n".repeat(100_000);
  try {
    expect(parse(large, { wasm }).program.body).toHaveLength(100_000);
    expect(made).toHaveLength(1);
    expect(bytesOf(made[0]!)).toBeGreaterThan(128 * 1024 * 1024);

    for (let round = 0; round < 3; round++) {
      expect(parse("a;", { wasm }).program.body).toHaveLength(1);
      expect(parse(large, { wasm }).program.body).toHaveLength(100_000);
    }
    expect(made).toHaveLength(1);

    clock += 10_000;
    expect(parse("a;", { wasm }).program.body).toHaveLength(1);
    expect(made).toHaveLength(1);

    clock += 1;
    expect(parse("a;", { wasm }).program.body).toHaveLength(1);
    expect(made).toHaveLength(2);
    expect(bytesOf(made[1]!)).toBeLessThan(128 * 1024 * 1024);
  } finally {
    WebAssembly.Instance = Instance;
    Date.now = now;
  }
});

test("an empty source parses empty when the writer repeats its last count", () => {
  const encodeInto = TextEncoder.prototype.encodeInto;
  const write = Buffer.prototype.write;
  let encoded = { read: 0, written: 0 };
  let wrote = 0;
  TextEncoder.prototype.encodeInto = function (source, view) {
    if (source) encoded = encodeInto.call(this, source, view);
    return encoded;
  };
  Buffer.prototype.write = function (this: Buffer, ...args: Parameters<typeof write>) {
    if (args[0]) wrote = write.apply(this, args);
    return wrote;
  } as typeof write;
  try {
    expect(parse("const answer = 42;").program.body).toHaveLength(1);
    expect(parse("").program.body).toEqual([]);
    expect(parse("").diagnostics).toEqual([]);
  } finally {
    TextEncoder.prototype.encodeInto = encodeInto;
    Buffer.prototype.write = write;
  }
});
