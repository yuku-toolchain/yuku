import {
  parse,
  langFromPath,
  sourceTypeFromPath,
  type ParseOptions,
  type Program,
} from "yuku-parser";
import { print, strip, minify, type CodegenOptions, type CodegenResult } from "yuku-codegen";

export type Op = "print" | "strip" | "minify";

const OPS: Record<Op, (program: Program, options: CodegenOptions) => CodegenResult> = {
  print,
  strip,
  minify,
};

export function gen(
  op: Op,
  source: string,
  options: CodegenOptions = {},
  path = "input.ts",
  parseOptions: Partial<ParseOptions> = {},
): string {
  const ast = parse(source, {
    lang: langFromPath(path),
    sourceType: sourceTypeFromPath(path),
    attachComments: true,
    ...parseOptions,
  });
  return OPS[op](ast.program, options).code;
}
