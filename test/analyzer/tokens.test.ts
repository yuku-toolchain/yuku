import { expect, test } from "bun:test";
import { parse, TokenKind } from "yuku";

test("tokens agree with the semantic model", () => {
  const source = "const double = (n: number) => n * 2;\ndouble(21);";
  const file = parse(source, { path: "a.ts", tokens: true, semantic: true });
  const tokens = file.tokens;
  expect(file.tokens).toBe(tokens);
  expect(tokens.length).toBe(18);
  expect(tokens.kind(8)).toBe(TokenKind.Arrow);
  expect(tokens.range(file.program.body[1]!)).toEqual([13, 18]);

  const use = file.rootScope.find("double")!.references[0]!.node;
  expect(tokens.text(tokens.at(use.start))).toBe("double");
});
