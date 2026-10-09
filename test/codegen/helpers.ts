import { parse, type ParseOptions, type SourceLang } from "yuku-parser";
import { generate, type GenerateOptions } from "yuku-codegen";

export function gen(
  source: string,
  options: GenerateOptions = {},
  path = "input.ts",
  parseOptions: Partial<ParseOptions> = {},
): string {
  const ast = parse(source, {
    path,
    attachComments: true,
    ...parseOptions,
  });
  return generate(ast.program, options).code;
}

const CHAIN_LINKS = 2_000;

export function deepChains(): { source: string; lang: SourceLang }[] {
  const links: [string, SourceLang][] = [
    [" + b", "js"],
    [" || b", "js"],
    [".b", "js"],
    ["()", "js"],
    ["[0]", "js"],
    ["!.b<T>(c)`t`", "ts"],
    [" as T satisfies U", "ts"],
    [" + (b<T>) - c", "ts"],
  ];
  return links.map(([link, lang]) => ({ source: `x = a${link.repeat(CHAIN_LINKS)};`, lang }));
}

export const INSTANTIATIONS: string[] = [
  "(f<T>)<U>;",
  "(f<T>)!;",
  "(f<T>)();",
  "(f<T>)<U>();",
  "(f<T>)`t`;",
  "(f<T>).x;",
  "(f<T>)?.x;",
  "(f<T>)[x];",
  "(f<T>)?.[x];",
  "new (f<T>)();",
  "class C extends (f<T>) {}",
  "class D extends (f<T>)<U> {}",
  "((f<T>)<U>)!<V>();",
  "(f<T>) < x;",
  "(f<T>) > x;",
  "(f<T>) >= x;",
  "(f<T>) >> x;",
  "(f<T>) >>> x;",
  "(f<T>) + x;",
  "(f<T>) - x;",
  "a * (f<T>) + x;",
  "a ** (f<T>) - x;",
  "typeof (f<T>) < x;",
  "-(f<T>) > x;",
  "<U>(f<T>) + x;",
  "async () => await (f<T>) - x;",
  "f<T>;",
  "f<T>?.();",
  "f<T>?.<U>();",
  "f<T> <= x;",
  "f<T> << x;",
  "f<T> == x;",
  "f<T> * x;",
  "f<T> as U;",
  "f<T> ? a : b;",
  "(a + f<T>) * x;",
  "x < f<T>;",
  "a?.b<T>;",
  "f<T>();",
  "new f<T>();",
  "f<T>`t`;",
];

const COMMENT_SNIPPETS: Record<"js" | "jsx" | "ts" | "tsx", string[]> = {
  js: [
    "[, , ,];",
    "[a, , b, ,];",
    "[];",
    "({});",
    "({ a, b: c, [d]: e, ...f, m() {}, get g() { return 1; }, set s(v) {}, async *h() {} });",
    "({ a = 1 } = {});",
    "const [] = x;",
    "const {} = x;",
    "const [a, , b = 1, ...c] = x;",
    "const { a, b: c, d = 1, ...e } = x;",
    "function f() {}",
    "function f(a, b = 1, ...c) {}",
    "(function () {});",
    "(async function* () {});",
    "(() => {});",
    "(async () => x);",
    "(async a => a);",
    "f(a, b);",
    "new F();",
    "new F;",
    "a?.b?.(c)?.[d];",
    "tag`a${b}c`;",
    "x = a ? b : c;",
    "x = !a + b * c ** d;",
    "x++, --y;",
    "x = (a, b);",
    "class A extends B { constructor() { super(); } }",
    "class A { m() {} static s() {} get g() { return 1; } set s(v) {} #p = 1; static { } }",
    "class A { async *m() {} 'k'() {} [k]() {} x; y = 1; }",
    "(class {});",
    "if (a) b; else c;",
    "for (let i = 0; i < n; i++) {}",
    "for (;;) {}",
    "for (const x in y) {}",
    "async function f() { for await (const x of y) {} }",
    "while (a) {}",
    "do {} while (a);",
    "switch (a) {}",
    "switch (a) { case 1: b; break; default: c; }",
    "switch (a) { default: case 1: }",
    "try {} catch {} finally {}",
    "try {} catch (e) {}",
    "l: for (;;) { break l; continue l; }",
    "for (;;) { break; continue; }",
    "function f() { return; }",
    "function f() { return a; }",
    "function* g() { yield; yield a; yield* b; }",
    "debugger;",
    "{}",
    "var a = 1, b;",
    "'use strict';",
    "function f() { 'use strict'; }",
    "x = /re/g + 1n + 'a' + null + true + this;",
    "x = new.target + import.meta;",
    "x = import('a', { with: {} });",
    "x = a[\"b\"] + { 'a': 1, [\"__proto__\"]: 2 };",
    "with (a) {}",
    "import 'a';",
    "import a, { b, c as d } from 'a' with { type: 'json' };",
    "import {} from 'a';",
    "import { a as a } from 'a';",
    "import * as ns from 'a';",
    "export {};",
    "export { a, b as c };",
    "export { a as a };",
    "export {} from 'a';",
    "export * as ns from 'a';",
    "export default a;",
    "export default function () {}",
    "export default class {}",
    "export const a = 1;",
    "@dec() class A { @dec m() {} @dec x; }",
    "@a.b(c) export class A {}",
  ],
  jsx: [
    "<div>{/* keep me */}</div>;",
    "<div>{}</div>;",
    "<div />;",
    "<div a b='x' c={d} {...e} />;",
    "<a.b.c x:y='1'></a.b.c>;",
    "<></>;",
    "<>{a}{...b}</>;",
    "<div>text{a}text</div>;",
  ],
  ts: [
    "let x: {};",
    "let x: { a: string; b?(): void; [k: string]: any; new (): X; (): Y };",
    "type A = [];",
    "type A = [a: string, b?: number, ...c[], string?];",
    "type A = string[] | number & boolean;",
    "type A = keyof typeof x;",
    "type A = (a: string) => void;",
    "type A = abstract new () => X;",
    "type A<T> = T extends infer U extends string ? U : never;",
    "type A = { readonly [K in keyof T as K]-?: T[K] };",
    "type A = import('a').B<C>;",
    "type A = `a${B}c`;",
    "type A = 'a' | -1 | true | null | (string);",
    "type A = unique symbol;",
    "let x: A[B][C];",
    "interface I {}",
    "interface I extends J, K<L> { a: string; m(): void; (): void; new (): I; }",
    "interface I { get g(): string; set s(v); }",
    "enum E {}",
    "enum E { A, B = 1, }",
    "namespace N {}",
    "declare module 'a';",
    "declare global {}",
    "declare const a: number;",
    "declare function f(a: string): void;",
    "function f(this: Window) {}",
    "function f<T>(a?: T): asserts a is T {}",
    "function f(a?, [b]?) {}",
    "function f(a): void; function f(a) {}",
    "abstract class A { abstract m(): void; abstract x: number; }",
    "class A { private x?: number; static readonly y = 1; declare z: string; w!: T; m?(): void; }",
    "class A { constructor(private a: string, public readonly b = 1) {} }",
    "class A<T> extends B<T> implements I, J {}",
    "x = a as T satisfies U;",
    "x = <T>a!;",
    "x = f<T>(a) + new F<T>();",
    "x = async <T,>(a: T): a is T => true;",
    "f(a as T, b);",
    "let x = (a as B).c;",
    "for (const x of y as T) {}",
    "import { type A, B } from 'a'; export { type C, D };",
    "import type { A } from 'a';",
    "import a = require('a');",
    "export = a;",
    "export as namespace N;",
    "let x!: number;",
    "function f({ a }: T, [b]: U) {}",
  ],
  tsx: ["<div<T> a='1' />;", "x = <T,>(a: T) => a;"],
};

export function commentPlacements(): { source: string; lang: SourceLang }[] {
  const placements: { source: string; lang: SourceLang }[] = [];
  for (const [lang, snippets] of Object.entries(COMMENT_SNIPPETS) as [SourceLang, string[]][]) {
    for (const snippet of snippets) {
      const base = parse(snippet, { lang, sourceType: "module", tokens: true });
      if (base.diagnostics.length > 0) throw new Error(`snippet does not parse: ${snippet}`);
      const tokens = base.tokens!;
      const gaps = new Set([snippet.length]);
      for (let i = 0; i < tokens.length; i++) gaps.add(tokens.start(i));
      for (const at of gaps) {
        for (const comment of [" /*c*/ ", " //c\n"]) {
          const source = snippet.slice(0, at) + comment + snippet.slice(at);
          const parsed = parse(source, { lang, sourceType: "module" });
          // a comment inside jsx text is text
          if (parsed.diagnostics.length > 0 || parsed.comments.length !== 1) continue;
          placements.push({ source, lang });
        }
      }
    }
  }
  return placements;
}
