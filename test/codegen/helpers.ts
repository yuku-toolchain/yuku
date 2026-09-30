import {
  parse,
  langFromPath,
  sourceTypeFromPath,
  type ParseOptions,
  type SourceLang,
} from "yuku-parser";
import { generate, type GenerateOptions } from "yuku-codegen";

export function gen(
  source: string,
  options: GenerateOptions = {},
  path = "input.ts",
  parseOptions: Partial<ParseOptions> = {},
): string {
  const ast = parse(source, {
    lang: langFromPath(path),
    sourceType: sourceTypeFromPath(path),
    attachComments: true,
    ...parseOptions,
  });
  return generate(ast.program, options).code;
}

const CHAIN_LINKS = 2_000;

/** Left-leaning chains nested far past the printers' recursion budgets. */
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

/**
 * TypeScript statements with an instantiation expression before each kind of token, written with
 * exactly the parens that keep TypeScript from reading on past its type arguments.
 */
export const INSTANTIATIONS: string[] = [
  // a postfix operator or a following operand would take over the type arguments
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
  // the operator follows the right edge of its left operand
  "a * (f<T>) + x;",
  "a ** (f<T>) - x;",
  "typeof (f<T>) < x;",
  "-(f<T>) > x;",
  "<U>(f<T>) + x;",
  "async () => await (f<T>) - x;",
  // the type arguments stay put
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
  // type arguments of a call, not instantiations
  "f<T>();",
  "new f<T>();",
  "f<T>`t`;",
];
