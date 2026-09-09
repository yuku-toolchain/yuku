import { expect, test } from "bun:test";
import { analyze, TokenKind } from "yuku-analyzer";

test("module tokens agree with the semantic model", () => {
  expect(analyze("let x = 1;", { path: "a.js" }).tokens).toBeUndefined();

  const module = analyze("const double = (n: number) => n * 2;\ndouble(21);", {
    path: "a.ts",
    tokens: true,
  });
  const tokens = module.tokens!;
  expect(tokens.length).toBe(18);
  expect(tokens.kind(8)).toBe(TokenKind.Arrow);
  expect(tokens.range(module.ast.body[1]!)).toEqual([13, 18]);

  const use = module.rootScope.find("double")!.references[0]!.node;
  expect(tokens.text(tokens.first(use))).toBe("double");
});
