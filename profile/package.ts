import "./v8.ts";
import { readFileSync } from "node:fs";
import { basename, join } from "node:path";
import { withCodSpeed } from "@codspeed/tinybench-plugin";
import { Bench } from "tinybench";
import {
  b,
  Core,
  parse,
  print,
  walk,
  type Diagnostic,
  type ParseOptions,
  type SourceFile,
} from "yuku";

const FILES = [
  "test/projects/zod/packages/zod/src/v4/core/schemas.ts",
  "test/projects/excalidraw/packages/excalidraw/components/App.tsx",
  "test/projects/three/src/renderers/WebGLRenderer.js",
  "test/projects/three/src/core/EventDispatcher.js",
];

interface Result {
  diagnostics: readonly Diagnostic[];
}

/** A file parsed once for the tasks that share it, held only while they run. */
interface Parsed {
  file: SourceFile | null;
  setup(): void;
  teardown(): void;
}

const NO_DIAGNOSTICS: Result = { diagnostics: [] };

const wasm = new WebAssembly.Module(readFileSync(join("npm", "wasm", "yuku.wasm")));
const core = new Core({ wasm });

const bench = withCodSpeed(new Bench({ time: 1000, warmupTime: 250, throws: true }));

for (const path of FILES) {
  const source = readFileSync(path, "utf8");
  const name = `${path.split("/")[2]}/${basename(path)}`;

  add(`parse ${name}`, () => parse(source, { path }));
  add(`parse ${name} with WebAssembly`, () => parse(source, { path, core }));
  add(`decode ${name}`, () => {
    const file = parse(source, { path });
    void file.program;
    return file;
  });
  add(`decode ${name} with comments`, () => {
    const file = parse(source, { path, attachComments: true });
    void file.program;
    return file;
  });
  add(`tokens ${name}`, () => {
    const file = parse(source, { path, tokens: true });
    void file.tokens;
    return file;
  });
  add(`analyze ${name}`, () => {
    const file = parse(source, { path, analyze: true });
    void file.scopes;
    return file;
  });
  add(`analyze ${name} with WebAssembly`, () => {
    const file = parse(source, { path, analyze: true, core });
    void file.scopes;
    return file;
  });
  add(`analyze ${name} with semantic errors`, () => {
    const file = parse(source, { path, analyze: true, semanticErrors: true });
    void file.scopes;
    return file;
  });
  add(`rename ${name}`, () => {
    const file = parse(source, { path, analyze: true });
    const binding = file.bindings.find((candidate) => candidate.references.length > 0);
    return { diagnostics: binding?.rename(`${binding.name}_renamed`) ?? [] };
  });

  const parsedWith = (options: ParseOptions): Parsed => {
    const parsed: Parsed = {
      file: null,
      setup() {
        parsed.file = parse(source, { path, ...options });
        void parsed.file.program;
      },
      teardown() {
        parsed.file = null;
      },
    };
    return parsed;
  };
  const parsed = parsedWith({});
  const commented = parsedWith({ attachComments: true });
  const wasmParsed = parsedWith({ core });
  add(`reanalyze ${name}`, () => reanalyze(parsed.file!), parsed);
  add(`reanalyze ${name} with WebAssembly`, () => reanalyze(wasmParsed.file!), wasmParsed);
  add(`print ${name}`, () => print(parsed.file!), parsed);
  add(`print ${name} with WebAssembly`, () => print(wasmParsed.file!), wasmParsed);
  add(`print ${name} with comments`, () => print(commented.file!), commented);
  add(`print ${name} minified`, () => print(parsed.file!, { minify: true }), parsed);
  add(`print ${name} with source map`, () => print(parsed.file!, { sourceMap: true }), parsed);
  add(
    `walk ${name}`,
    () => {
      let identifiers = 0;
      walk(parsed.file!.program, {
        Identifier() {
          identifiers++;
        },
      });
      return NO_DIAGNOSTICS;
    },
    parsed,
  );
}

add("build", () => {
  for (let i = 0; i < 1_000; i++) {
    const callee = b.Identifier({ name: "log" });
    void b.CallExpression({ callee, arguments: [b.Literal({ value: i })] });
  }
  return NO_DIAGNOSTICS;
});

bench.runSync();

if (bench.tasks.every((task) => task.result.state === "completed")) {
  console.table(bench.table());
}

function reanalyze(file: SourceFile): SourceFile {
  file.reanalyze();
  void file.scopes;
  return file;
}

function add(name: string, run: () => Result, parsed?: Parsed): void {
  bench.add(name, run, {
    beforeAll() {
      parsed?.setup();
      const { diagnostics } = run();
      if (diagnostics.length > 0) {
        throw new Error(`${name} reports "${diagnostics[0]!.message}"`);
      }
    },
    afterAll() {
      parsed?.teardown();
    },
  });
}
