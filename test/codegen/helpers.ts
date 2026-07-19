import { parse, langFromPath, sourceTypeFromPath, type ParseOptions } from "yuku-parser";
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
