import "./v8.ts";
import { readFileSync } from "node:fs";
import { basename, join } from "node:path";
import { withCodSpeed } from "@codspeed/tinybench-plugin";
import { Bench } from "tinybench";
import {
  Core,
  js,
  parse,
  print,
  walk,
  type Diagnostic,
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

interface Scope {
  setup?(): void;
  teardown?(): void;
}

const NO_DIAGNOSTICS: Result = { diagnostics: [] };

const wasm = new WebAssembly.Module(readFileSync(join("npm", "wasm", "yuku.wasm")));
const core = new Core({ wasm });

const bench = withCodSpeed(new Bench({ time: 1000, warmupTime: 250, throws: true }));

for (const path of FILES) {
  const source = readFileSync(path, "utf8");
  const name = `${path.split("/")[2]}/${basename(path)}`;
  const typed = /\.tsx?$/.test(path);

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
    const file = parse(source, { path, semantic: true });
    void file.scopes;
    return file;
  });
  add(`analyze ${name} with WebAssembly`, () => {
    const file = parse(source, { path, semantic: true, core });
    void file.scopes;
    return file;
  });
  add(`analyze ${name} with semantic errors`, () => {
    const file = parse(source, { path, semantic: true, semanticErrors: true });
    void file.scopes;
    return file;
  });
  add(`rename ${name}`, () => {
    const file = parse(source, { path, semantic: true });
    const binding = file.bindings.find((candidate) => candidate.references.length > 0);
    return { diagnostics: binding?.rename(`${binding.name}_renamed`) ?? [] };
  });

  let file: SourceFile | null = null;
  const parsed: Scope = {
    setup() {
      file = parse(source, { path });
      void file.program;
    },
    teardown() {
      file = null;
    },
  };
  let commented: SourceFile | null = null;
  const withComments: Scope = {
    setup() {
      commented = parse(source, { path, attachComments: true });
      void commented.program;
    },
    teardown() {
      commented = null;
    },
  };
  add(`print ${name}`, () => print(file!), parsed);
  add(`print ${name} with comments`, () => print(commented!), withComments);
  if (typed) add(`print ${name} stripped`, () => print(file!, { strip: true }), parsed);
  add(`print ${name} minified`, () => print(file!, { minify: true }), parsed);
  add(`print ${name} with source map`, () => print(file!, { sourceMap: true }), parsed);
  add(
    `walk ${name}`,
    () => {
      let identifiers = 0;
      walk(file!.program, {
        Identifier() {
          identifiers++;
        },
      });
      return NO_DIAGNOSTICS;
    },
    parsed,
  );
}

add("template", () => {
  for (let i = 0; i < 1_000; i++) void js`console.log(${"value"}, ${i})`;
  return NO_DIAGNOSTICS;
});

add("template with text", () => {
  for (let i = 0; i < 1_000; i++) void js`import data from "./data/${i}.json";`;
  return NO_DIAGNOSTICS;
});

bench.runSync();

if (bench.tasks.every((task) => task.result.state === "completed")) {
  console.table(bench.table());
}

function add(name: string, run: () => Result, scope: Scope = {}): void {
  bench.add(name, run, {
    beforeAll() {
      scope.setup?.();
      const { diagnostics } = run();
      if (diagnostics.length > 0) {
        throw new Error(`${name} reports "${diagnostics[0]!.message}"`);
      }
    },
    afterAll() {
      scope.teardown?.();
    },
  });
}
