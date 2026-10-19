// first, so the flags apply before yuku compiles its WebAssembly
import "./v8.ts";
import { readFileSync } from "node:fs";
import { basename } from "node:path";
import { withCodSpeed } from "@codspeed/tinybench-plugin";
import { Bench } from "tinybench";
import { type Diagnostic, langFromPath, parse, type Program } from "yuku";
import { analyze } from "yuku/analyzer";
import { generate } from "yuku/codegen";

const FILES = [
  "test/projects/zod/packages/zod/src/v4/core/schemas.ts",
  "test/projects/excalidraw/packages/excalidraw/components/App.tsx",
  "test/projects/three/src/renderers/WebGLRenderer.js",
  "test/projects/three/src/core/EventDispatcher.js",
];

interface Result {
  diagnostics: Diagnostic[];
}

interface Scope {
  setup?(): void;
  teardown?(): void;
}

const bench = withCodSpeed(new Bench({ time: 1000, warmupTime: 250, throws: true }));

for (const path of FILES) {
  const source = readFileSync(path, "utf8");
  const file = `${path.split("/")[2]}/${basename(path)}`;
  const lang = langFromPath(path);

  add(`parse ${file}`, () => parse(source, { path }));
  add(`analyze ${file}`, () => analyze(source, { path }));
  add(`analyze ${file} with semantic errors`, () =>
    analyze(source, { path, semanticErrors: true }),
  );

  let program: Program | null = null;
  const parsed: Scope = {
    setup() {
      program = parse(source, { path }).program;
    },
    teardown() {
      program = null;
    },
  };
  add(`generate ${file}`, () => generate(program!), parsed);
  if (lang === "ts" || lang === "tsx") {
    add(`generate ${file} stripped`, () => generate(program!, { strip: true }), parsed);
  }
  add(`generate ${file} minified`, () => generate(program!, { minify: true }), parsed);
  add(
    `generate ${file} with source map`,
    () => generate(program!, { sourceMap: { source } }),
    parsed,
  );
}

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
