// the Zig and JS printers must agree byte for byte
//   bun test/codegen/conformance.ts [plan...] [--file <path>] [--show <n>]

import { spawnSync } from "node:child_process";
import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { parse, sourceTypeFromPath, type ParseOptions } from "yuku-parser";
import { generate, type GenerateOptions } from "yuku-codegen";
import { corpusFiles, projectFiles, type CorpusFile } from "../corpus";
import { commentPlacements, deepChains, INSTANTIATIONS } from "./helpers";

const REFERENCE = join(
  "zig-out",
  "bin",
  process.platform === "win32" ? "codegen-reference.exe" : "codegen-reference",
);

export interface Plan {
  name: string;
  zig: string[];
  js: GenerateOptions;
  preserveParens: boolean;
  map: boolean;
}

export const PLANS: Plan[] = [
  {
    name: "jsx",
    zig: ["--jsx", "--comments=all", "--quotes=single"],
    js: { jsx: true, comments: "all", quotes: "single" },
    preserveParens: true,
    map: false,
  },
  {
    name: "jsx-pure",
    zig: ["--jsx-pragma=runtime.h", "--jsx-pragma-frag=runtime.Fragment", "--jsx-pure",
      "--comments=all"],
    js: { jsx: { pragma: "runtime.h", pragmaFrag: "runtime.Fragment", pure: true },
      comments: "all" },
    preserveParens: true,
    map: false,
  },
  {
    name: "jsx-everything",
    zig: ["--jsx-pragma=runtime.h", "--jsx-pragma-frag=runtime.Fragment",
      "--strip", "--minify", "--compact", "--source-map",
      "--no-preserve-parens", "--quotes=shortest", "--comments=all"],
    js: { jsx: { pragma: "runtime.h", pragmaFrag: "runtime.Fragment" },
      strip: true, minify: true, comments: "all" },
    preserveParens: false,
    map: true,
  },
  { name: "default", zig: [], js: {}, preserveParens: true, map: false },
  {
    name: "all",
    zig: ["--comments=all"],
    js: { comments: "all" },
    preserveParens: true,
    map: false,
  },
  {
    name: "compact",
    zig: ["--compact", "--comments=all"],
    js: { format: "compact", comments: "all" },
    preserveParens: true,
    map: false,
  },
  {
    name: "strip",
    zig: ["--strip", "--comments=all"],
    js: { strip: true, comments: "all" },
    preserveParens: true,
    map: false,
  },
  {
    name: "minify",
    zig: ["--minify", "--compact", "--quotes=shortest", "--comments=all"],
    js: { minify: true, comments: "all" },
    preserveParens: true,
    map: false,
  },
  {
    name: "map",
    zig: ["--source-map", "--comments=all"],
    js: { comments: "all" },
    preserveParens: true,
    map: true,
  },
  {
    name: "noparens",
    zig: ["--no-preserve-parens", "--comments=all", "--quotes=double"],
    js: { comments: "all", quotes: "double" },
    preserveParens: false,
    map: false,
  },
  {
    name: "line",
    zig: ["--comments=line", "--indent=4", "--quotes=single"],
    js: { comments: "line", indent: 4, quotes: "single" },
    preserveParens: true,
    map: false,
  },
  {
    name: "block",
    zig: ["--compact", "--comments=block", "--quotes=double"],
    js: { format: "compact", comments: "block", quotes: "double" },
    preserveParens: true,
    map: false,
  },
  {
    name: "everything",
    zig: [
      "--strip",
      "--minify",
      "--compact",
      "--source-map",
      "--no-preserve-parens",
      "--quotes=single",
      "--comments=some",
      "--indent=4",
    ],
    js: {
      strip: true,
      minify: { syntax: true },
      format: "compact",
      quotes: "single",
      comments: "some",
      indent: 4,
    },
    preserveParens: false,
    map: true,
  },
];

export interface Mismatch {
  path: string;
  what: "code" | "map" | "diagnostics" | "skip" | "threw" | "fixture";
  expected: string;
  actual: string;
}

export interface PlanResult {
  plan: string;
  compared: number;
  mismatches: Mismatch[];
}

interface Reference {
  printed: boolean;
  code: string;
  mappings: string;
  diagnostics: { start: number; end: number; message: string }[];
}

export interface Input extends CorpusFile {
  source?: string;
}

export function conformanceInputs(): Input[] {
  const chains = deepChains().map(({ source, lang }, i) => {
    const path = `chain-${i}.${lang}`;
    return { path, relative: path, lang, sourceType: sourceTypeFromPath(path), source };
  });
  const instantiations: Input = {
    path: "instantiations.ts",
    relative: "instantiations.ts",
    lang: "ts",
    sourceType: "module",
    source: INSTANTIATIONS.join("\n"),
  };
  const comments = commentPlacements().map(({ source, lang }, i) => {
    const path = `comment-${i}.${lang}`;
    return { path, relative: path, lang, sourceType: "module" as const, source };
  });
  const jsx: Input = {
    path: "jsx-lowering.tsx",
    relative: "jsx-lowering.tsx",
    lang: "tsx",
    sourceType: "module",
    source: [
      `<><div> hello </div><X/><Upper-case/><ui.Box/><this/><this.Box/><svg:path/></>;`,
      `<Box<number> enabled data-id="&amp;" {...props} value={(a, b)}>{(a, b)}{...items}</Box>;`,
      `<div __proto__={payload} title='&quot;&apos;&#x1F600;' raw={"&amp;"}/>;`,
      `<div>\n a &#32; \n{/* @keep */}x{// @line\n}y</div>;`,
      `<div>&amp;lt; &unknown; &AMP; &#xD800; &#x110000; &#X41; &#x; &#;</div>;`,
      `<div>&#${"0".repeat(1_000)}65;&#0;</div>;`,
      `new (<Box/>).constructor(); new (<></>)();`,
      `x = a / <div/>; x = a / b / <div/>; x = y in <div/>;`,
      `<this.foo-bar/>; <foo-bar.Box/>;`,
      `<a.b.c.d/>; <x /*mid*/ .y-z /*tail*/ .w/>;`,
      `<div>text</div /*closing*/>;`,
      `<>text</ /*frag_close*/>;`,
      `<Box<number /*type*/>>text</Box>;`,
      `<UI.Box<{x: A<B /*inner*/> /*member*/} /*outer*/> />;`,
      `<UI.Box>text</UI /*object*/.Box /*property*/>;`,
      `<x:y>text</x /*namespace*/:y /*local*/>;`,
      `<x /*tag_namespace*/:y /*tag_local*/ a /*attr_namespace*/:b /*attr_local*/="value" />;`,
      `<Box<{/*ordinary*/\n// @line\n/* @legal */}>>text</Box /*close*/>;`,
      `<Box<number // type\n> enabled>text</Box // close\n>;`,
      `<Box<${"number,".repeat(127)}number /*wide*/> />;`,
      `<Box<A${"[]".repeat(96)} /*deep*/> />;`,
    ].join("\n"),
  };
  return [...corpusFiles(), ...projectFiles(), ...chains, instantiations, ...comments, jsx];
}

export function runPlan(plan: Plan, files: Input[]): PlanResult {
  const references = runReference(plan, files);
  const mismatches: Mismatch[] = [];
  let compared = 0;
  for (let i = 0; i < files.length; i++) {
    const file = files[i]!;
    const reference = references[i]!;
    const source = file.source ?? readFileSync(file.path, "utf8");
    const parseOptions: ParseOptions = {
      lang: file.lang,
      sourceType: file.sourceType,
      attachComments: true,
      preserveParens: plan.preserveParens,
    };
    const parsed = parse(source, parseOptions);
    const skipped = parsed.diagnostics.length > 0;
    // synthetic inputs must parse cleanly. only corpus files may carry diagnostics
    if (skipped && file.source !== undefined) {
      mismatches.push({
        path: file.path,
        what: "fixture",
        expected: "",
        actual: parsed.diagnostics
          .map((d) => `${d.start}-${d.end} ${d.message}`)
          .join("\n"),
      });
      continue;
    }
    if (skipped !== !reference.printed) {
      mismatches.push({
        path: file.path,
        what: "skip",
        expected: reference.printed ? "printed" : "skipped",
        actual: skipped ? "skipped" : "printed",
      });
      continue;
    }
    if (skipped) continue;
    compared++;

    const options: GenerateOptions = plan.map ? { ...plan.js, sourceMap: { source } } : plan.js;
    let result;
    try {
      result = generate(parsed.program, options);
    } catch (error) {
      mismatches.push({
        path: file.path,
        what: "threw",
        expected: "",
        actual: String((error as Error).stack ?? error),
      });
      continue;
    }
    if (result.code !== reference.code) {
      mismatches.push({
        path: file.path,
        what: "code",
        expected: reference.code,
        actual: result.code,
      });
      continue;
    }
    if (plan.map && result.map?.mappings !== reference.mappings) {
      mismatches.push({
        path: file.path,
        what: "map",
        expected: reference.mappings,
        actual: result.map?.mappings ?? "",
      });
      continue;
    }
    const expected = referenceDiagnostics(reference, source);
    const actual = result.diagnostics.map((d) => `${d.start}-${d.end} ${d.message}`).join("\n");
    if (actual !== expected) {
      mismatches.push({ path: file.path, what: "diagnostics", expected, actual });
    }
  }
  return { plan: plan.name, compared, mismatches };
}

function runReference(plan: Plan, files: Input[]): Reference[] {
  if (!existsSync(REFERENCE)) {
    throw new Error(`${REFERENCE} is missing, build it with \`zig build codegen-reference\``);
  }
  const dir = mkdtempSync(join(tmpdir(), "codegen-conformance-"));
  try {
    const lines = files.map((file) => {
      if (file.source === undefined) return `${file.sourceType} ${file.path}`;
      const path = join(dir, file.path);
      writeFileSync(path, file.source);
      return `${file.sourceType} ${path}`;
    });
    const list = join(dir, "list.txt");
    const out = join(dir, "out.bin");
    writeFileSync(list, lines.join("\n") + "\n");
    const run = spawnSync(REFERENCE, [list, out, ...plan.zig], { stdio: "inherit" });
    if (run.status !== 0) throw new Error(`codegen-reference failed for plan ${plan.name}`);
    return readReference(readFileSync(out), files.length);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
}

function readReference(output: Buffer, count: number): Reference[] {
  const references: Reference[] = [];
  let offset = 0;
  const readU32 = (): number => {
    const value = output.readUInt32LE(offset);
    offset += 4;
    return value;
  };
  const readString = (): string => {
    const length = readU32();
    const text = output.toString("utf8", offset, offset + length);
    offset += length;
    return text;
  };
  for (let i = 0; i < count; i++) {
    const status = output[offset++];
    if (status !== 0) {
      references.push({ printed: false, code: "", mappings: "", diagnostics: [] });
      continue;
    }
    const code = readString();
    const mappings = readString();
    const diagnostics = [];
    for (let remaining = readU32(); remaining > 0; remaining--) {
      const start = readU32();
      const end = readU32();
      diagnostics.push({ start, end, message: readString() });
    }
    references.push({ printed: true, code, mappings, diagnostics });
  }
  if (offset !== output.length) throw new Error("codegen-reference output has trailing bytes");
  return references;
}

// the Zig side counts UTF-8 bytes, the JS side UTF-16 units
function referenceDiagnostics(reference: Reference, source: string): string {
  if (reference.diagnostics.length === 0) return "";
  const units = utf16Offsets(source);
  return reference.diagnostics
    .map((d) => `${units[d.start]}-${units[d.end]} ${d.message}`)
    .join("\n");
}

function utf16Offsets(source: string): number[] {
  const units: number[] = [];
  for (let i = 0; i < source.length; i++) {
    const code = source.codePointAt(i)!;
    const bytes = code < 0x80 ? 1 : code < 0x800 ? 2 : code < 0x10000 ? 3 : 4;
    for (let k = 0; k < bytes; k++) units.push(i);
    if (code >= 0x10000) i++;
  }
  units.push(source.length);
  return units;
}

function firstDifference(a: string, b: string): number {
  let i = 0;
  while (i < a.length && i < b.length && a[i] === b[i]) i++;
  return i;
}

export function describeMismatch(mismatch: Mismatch, context = 240): string {
  const { path, what, expected, actual } = mismatch;
  if (what === "threw" || what === "skip" || what === "fixture") {
    return `${path} ${what}\n${actual}`;
  }
  const at = firstDifference(expected, actual);
  const from = Math.max(0, at - context);
  return [
    `${path} ${what} differs at ${at}`,
    "--- zig",
    expected.slice(from, at + context),
    "--- js",
    actual.slice(from, at + context),
  ].join("\n");
}

if (import.meta.main) {
  const args = process.argv.slice(2);
  const fileAt = args.indexOf("--file");
  const showAt = args.indexOf("--show");
  const show = showAt >= 0 ? Number(args[showAt + 1]) : 3;
  const only = fileAt >= 0 ? args[fileAt + 1] : undefined;
  const values = new Set([fileAt, showAt].filter((at) => at >= 0).map((at) => at + 1));
  const names = args.filter((arg, i) => !arg.startsWith("--") && !values.has(i));
  const plans = names.length > 0 ? PLANS.filter((plan) => names.includes(plan.name)) : PLANS;
  const files = conformanceInputs().filter(
    (file) => only === undefined || file.path.includes(only),
  );
  let failed = false;
  for (const plan of plans) {
    const start = performance.now();
    const result = runPlan(plan, files);
    const ms = (performance.now() - start).toFixed(0);
    console.log(
      `${plan.name.padEnd(11)} ${result.compared} compared, ` +
        `${result.mismatches.length} mismatched (${ms} ms)`,
    );
    const counts = new Map<string, number>();
    for (const { what } of result.mismatches) counts.set(what, (counts.get(what) ?? 0) + 1);
    if (counts.size > 0) {
      console.log("  " + [...counts].map(([what, count]) => `${what} ${count}`).join(", "));
    }
    for (const mismatch of result.mismatches.slice(0, show)) {
      console.log(describeMismatch(mismatch) + "\n");
    }
    failed ||= result.mismatches.length > 0;
  }
  process.exit(failed ? 1 : 0);
}
