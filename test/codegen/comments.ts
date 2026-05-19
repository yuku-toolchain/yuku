import { parse, SourceLang } from "yuku-parser";
import { print, strip, minify, type CodegenOptions } from "yuku-codegen";

type Case = {
  name: string;
  source: string;
  options?: CodegenOptions;
  op?: "print" | "strip" | "minify";
  lang?: SourceLang;
  want: string;
};

const cases: Case[] = [
  {
    name: "default drops normal comments",
    source: "// hi\nconst x = 1;\n",
    want: "const x = 1;",
  },
  {
    name: "default keeps legal banner",
    source: "/*! ©2025 */\nconst x = 1;\n",
    want: "/*! ©2025 */\nconst x = 1;",
  },
  {
    name: "default keeps pure annotation",
    source: "const x = /*#__PURE__*/ foo();\n",
    want: "const x = /*#__PURE__*/ foo();",
  },
  {
    name: "comments:true emits leading line",
    source: "// hello\nconst x = 1;\n",
    options: { comments: true },
    want: "// hello\nconst x = 1;",
  },
  {
    name: "comments:true keeps trailing same-line",
    source: "foo(); // tail\nbar();\n",
    options: { comments: true },
    want: "foo(); // tail\nbar();",
  },
  {
    name: "comments:true preserves blank line groups",
    source: "foo();\n\n// after blank\nbar();\n",
    options: { comments: true },
    want: "foo();\n\n// after blank\nbar();",
  },
  {
    name: "pure annotation stays inline before call",
    source: "const x = /*#__PURE__*/ foo();\n",
    options: { comments: true },
    want: "const x = /*#__PURE__*/ foo();",
  },
  {
    name: "no_side_effects emits before function decl",
    source: "/*#__NO_SIDE_EFFECTS__*/\nfunction make() { return {}; }\n",
    options: { comments: true },
    want: "/*#__NO_SIDE_EFFECTS__*/\nfunction make() {\n  return {};\n}",
  },
  {
    name: "some mode keeps legal and drops noise",
    source: "/*! @license MIT */\n// noise\nconst x = 1;\n",
    options: { comments: "some" },
    want: "/*! @license MIT */\nconst x = 1;",
  },
  {
    name: "some mode keeps jsdoc",
    source: "/** @param x */\nfunction f(x) { return x; }\n",
    options: { comments: "some" },
    want: "/** @param x */\nfunction f(x) {\n  return x;\n}",
  },
  {
    name: "some mode preserves pure annotations",
    source: "const x = /*#__PURE__*/ foo();\n",
    options: { comments: "some" },
    want: "const x = /*#__PURE__*/ foo();",
  },
  {
    name: "line filter drops blocks",
    source: "// keep me\n/* drop me */\nconst x = 1;\n",
    options: { comments: "line" },
    want: "// keep me\nconst x = 1;",
  },
  {
    name: "block filter drops lines",
    source: "// drop me\n/* keep me */\nconst x = 1;\n",
    options: { comments: "block" },
    want: "/* keep me */\nconst x = 1;",
  },
  {
    name: "comments:false explicit drops",
    source: "// hidden\nconst x = 1;\n",
    options: { comments: false },
    want: "const x = 1;",
  },
  {
    name: "compact + some keeps pure inline",
    source: "/*! ©2025 */\nconst x = /*#__PURE__*/ make();\n",
    options: { format: "compact", comments: "some" },
    op: "minify",
    want: "/*! ©2025 */\nconst x=/*#__PURE__*/make()",
  },
  {
    name: "strip preserves comments alongside type stripping",
    source: "// types incoming\nconst x: number = 1;\n",
    options: { comments: true },
    op: "strip",
    lang: "ts",
    want: "// types incoming\nconst x = 1;",
  },
  {
    name: "hashbang + leading comment",
    source: "#!/usr/bin/env node\n// hi\nconst x = 1;\n",
    options: { comments: true },
    want: "#!/usr/bin/env node\n// hi\nconst x = 1;",
  },
];

let failed = 0;
for (const c of cases) {
  const ast = parse(c.source, { lang: c.lang ?? "js" });
  const op = c.op === "strip" ? strip : c.op === "minify" ? minify : print;
  const got = op(ast, c.options ?? {}).code.replace(/\s+$/, "");
  const want = c.want.replace(/\s+$/, "");
  if (got === want) {
    console.log(`  ✓ ${c.name}`);
  } else {
    failed++;
    console.log(`  ✗ ${c.name}`);
    console.log(`    want: ${JSON.stringify(want)}`);
    console.log(`    got:  ${JSON.stringify(got)}`);
  }
}

console.log(`\n  ${cases.length - failed}/${cases.length} passed`);
process.exit(failed > 0 ? 1 : 0);
