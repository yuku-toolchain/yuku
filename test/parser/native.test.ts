import { afterAll, describe, expect, test } from "bun:test";
import { existsSync, readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { Core, parse, type ParseOptions, type SourceFile } from "yuku";
import { astDiffPath } from "../ast-helpers-for-test";
import { corpusAndProjectFiles, corpusPresent, forEachCorpusFile } from "../corpus";

const packages = new URL("../../npm/", import.meta.url);
const addon = `${process.platform}-${process.arch}/yuku.node`;
const shipsAddon = existsSync(new URL(addon, packages));
const wasm = new WebAssembly.Module(readFileSync(new URL("wasm/yuku.wasm", packages)));
const core = new Core({ wasm });
const root = fileURLToPath(new URL("../..", import.meta.url));
const SAMPLE_MAX = 8;
const SEMANTIC_EVERY = 8;

function child(script: string): string {
  const run = Bun.spawnSync(["bun", "--eval", script], { cwd: root });
  expect(run.stderr.toString()).toBe("");
  return run.stdout.toString();
}

function summary(file: SourceFile, options: ParseOptions): unknown {
  return {
    program: file.program,
    comments: file.comments,
    diagnostics: file.diagnostics,
    tokens: options.tokens === true ? tokensOf(file) : null,
    semantic: options.semantic === true ? semanticOf(file) : null,
  };
}

function tokensOf(file: SourceFile): [number, number, number, string][] {
  const out: [number, number, number, string][] = [];
  const { tokens } = file;
  for (let i = 0; i < tokens.length; i++) {
    out.push([tokens.kind(i), tokens.start(i), tokens.end(i), tokens.text(i)]);
  }
  return out;
}

function semanticOf(file: SourceFile): unknown {
  return {
    scopes: file.scopes.map((scope) => [scope.kind, scope.node.start, scope.parent?.id ?? -1]),
    bindings: file.bindings.map((binding) => [binding.name, binding.flags, binding.scope.id]),
    references: file.references.map((reference) => [
      reference.name,
      reference.node.start,
      reference.binding?.id ?? -1,
      reference.isWrite,
    ]),
    imports: file.imports.map((entry) => [entry.kind, entry.name, entry.specifier]),
    exports: file.exports.map((entry) => [entry.kind, entry.name, entry.specifier]),
  };
}

function agrees(source: string, options: ParseOptions): string | null {
  const native = summary(parse(source, options), options);
  const fallback = summary(parse(source, { ...options, core }), options);
  return astDiffPath(native, fallback, "result");
}

describe.skipIf(!shipsAddon)("the Node-API addon", () => {
  test("loads for this platform and parses in place of WebAssembly", () => {
    const loaded = child(`
      const dlopen = process.dlopen;
      const paths = [];
      const calls = [];
      process.dlopen = function (module, path, ...rest) {
        paths.push(path);
        dlopen.call(this, module, path, ...rest);
        const { parse, analyze } = module.exports;
        module.exports = {
          parse: (...args) => (calls.push("parse"), parse(...args)),
          analyze: (...args) => (calls.push("analyze"), analyze(...args)),
        };
      };
      const { parse } = await import("yuku");
      parse("a;").program;
      parse("let b;", { semantic: true }).scopes;
      console.log(JSON.stringify({ paths, calls }));
    `);
    const { paths, calls } = JSON.parse(loaded) as { paths: string[]; calls: string[] };
    expect(paths).toHaveLength(1);
    expect(paths[0]!.replaceAll("\\", "/").endsWith(addon)).toBe(true);
    expect(calls).toEqual(["parse", "analyze"]);
  });

  test("agrees with WebAssembly under every option", () => {
    const sources = [
      "const café = '☕'; // ünïcödé\nlet 😀x = `𝒳${café}`, re = /é+/u;",
      "/* 中文 */ import { 名 as n } from './模块'; export { n as 名前 };",
      `const lone = "${String.fromCharCode(0xd800)}", after = lone;`,
      "const a = <p title='ü'>Grüße {a} 👋</p>;",
      "let é = ;",
      "let a = 1; let a = 2; with (a) {}",
      "function f<T>(this: Window, x?: T): asserts x is T { return (x as any)!; }",
      "",
    ];
    const variants: ParseOptions[] = [
      { lang: "tsx" },
      { lang: "tsx", tokens: true, attachComments: true },
      { lang: "tsx", semantic: true, semanticErrors: true },
      { lang: "jsx", sourceType: "script", preserveParens: false },
      { lang: "ts", sourceType: "commonjs", semantic: true, tokens: true },
    ];
    for (const source of sources) {
      for (const options of variants) expect(agrees(source, options)).toBeNull();
    }
  });
});

test("only runtimes with the node condition load an addon, each from its own package", () => {
  const yuku = new URL("yuku/", packages);
  const manifest = JSON.parse(readFileSync(new URL("package.json", yuku), "utf8"));
  const loaders = manifest.imports["#native"];
  expect(Object.keys(loaders)).toEqual(["node", "default"]);
  const loader = readFileSync(new URL(loaders.node, yuku), "utf8");
  expect(readFileSync(new URL(loaders.default, yuku), "utf8")).not.toContain("@yuku-toolchain");
  const calls = loader.matchAll(/addon\("([a-z0-9]+-[a-z0-9]+)"\)/g);
  const platforms = [...calls].map((match) => match[1]!);
  expect(platforms).toHaveLength(6);
  const dependencies = platforms.map((platform) => `@yuku-toolchain/${platform}`);
  expect(Object.keys(manifest.optionalDependencies)).toEqual(dependencies);
  for (const platform of platforms) {
    const [os, cpu] = platform.split("-");
    const path = new URL(`${platform}/package.json`, packages);
    const { name, ...fields } = JSON.parse(readFileSync(path, "utf8"));
    expect(fields).toMatchObject({ version: manifest.version, os: [os], cpu: [cpu] });
    expect(manifest.optionalDependencies[name]).toBe(manifest.version);
  }
});

test("the package parses with WebAssembly where no addon loads", () => {
  const output = child(`
    process.dlopen = () => {
      throw new Error("native addons are not supported here");
    };
    const { parse } = await import("yuku");
    const file = parse("let é = 1;", { semantic: true });
    console.log(JSON.stringify([file.program.body[0].end, file.bindings.map((b) => b.name)]));
  `);
  expect(JSON.parse(output)).toEqual([10, ["é"]]);
});

describe.skipIf(!shipsAddon || !corpusPresent())("the Node-API addon across the corpus", () => {
  const differences: string[] = [];
  let compared = 0;

  afterAll(() => {
    expect(compared).toBeGreaterThan(1000);
  });

  test("agrees with WebAssembly on every file", async () => {
    await forEachCorpusFile(corpusAndProjectFiles(), (file, source) => {
      const semantic = compared % SEMANTIC_EVERY === 0;
      const options: ParseOptions = { lang: file.lang, sourceType: file.sourceType, semantic };
      compared++;
      const difference = agrees(source, options);
      if (difference !== null && differences.length < SAMPLE_MAX) {
        differences.push(`${file.relative}: ${difference}`);
      }
    });
    expect(differences).toEqual([]);
  }, 1_800_000);
});
