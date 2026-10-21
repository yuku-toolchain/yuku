import { describe, expect, test } from "bun:test";
import { parse, TokenKind, type TokenList } from "yuku";

type Span = { start: number; end: number };

const names = new Map(Object.entries(TokenKind).map(([name, tag]) => [tag, name]));

function list(source: string | Uint8Array, options = {}): TokenList {
  return parse(source, { ...options, tokens: true }).tokens;
}

function values(tokens: TokenList): string[] {
  return Array.from({ length: tokens.length }, (_, i) => tokens.text(i));
}

function tags(tokens: TokenList): string[] {
  return Array.from({ length: tokens.length }, (_, i) => names.get(tokens.kind(i))!);
}

function spans(tokens: TokenList): number[][] {
  return Array.from({ length: tokens.length }, (_, i) => [tokens.start(i), tokens.end(i)]);
}

describe("tokens", () => {
  test("collected with tokens, and read without it throws", () => {
    const file = parse("let x = 1;", { tokens: true });
    expect(file.tokens).toBe(file.tokens);
    expect(file.tokens.length).toBe(5);
    expect(list("").length).toBe(0);
    expect(() => parse("let x = 1;").tokens).toThrow("parse with `tokens: true`");
  });

  test("every token in source order, without the closing eof", () => {
    const tokens = list("let x = 1 + y;");
    expect(values(tokens)).toEqual(["let", "x", "=", "1", "+", "y", ";"]);
    expect(tags(tokens)).toEqual([
      "Let", "Identifier", "Assign", "NumericLiteral", "Plus", "Identifier", "Semicolon",
    ]);
  });

  test("predicates", () => {
    const tokens = list("let x = 1 + y;");
    expect(tokens.isKeyword(0)).toBe(true);
    expect(tokens.isReserved(0)).toBe(true);
    expect(tokens.isUnconditionallyReserved(0)).toBe(false);
    expect(tokens.isStrictModeReserved(0)).toBe(true);
    expect(tokens.isKeyword(1)).toBe(false);
    expect(tokens.isIdentifierLike(1)).toBe(true);
    expect(tokens.isNumericLiteral(3)).toBe(true);
    expect(tokens.isBinaryOperator(4)).toBe(true);
    expect(tokens.isUnaryOperator(4)).toBe(true);
    expect(tokens.isAssignmentOperator(2)).toBe(true);
    expect(tokens.isLogicalOperator(4)).toBe(false);
    expect(tokens.precedence(4)).toBe(11);
    expect(tokens.precedence(1)).toBe(0);
  });

  test("rescanned tokens are final", () => {
    expect(values(list("x = /ab+/gi;"))).toEqual(["x", "=", "/ab+/gi", ";"]);
    expect(list("x = /ab+/gi;").kind(2)).toBe(TokenKind.RegexLiteral);
    expect(values(list("`a${b}c${d}e`;"))).toEqual(["`a${", "b", "}c${", "d", "}e`", ";"]);
    expect(values(list("let x: A<B<C>> = y;", { lang: "ts" }))).toEqual([
      "let", "x", ":", "A", "<", "B", "<", "C", ">", ">", "=", "y", ";",
    ]);
    const jsx = list(`<a href="x">hi {y}</a>;`, { lang: "jsx" });
    expect(values(jsx)).toEqual([
      "<", "a", "href", "=", '"x"', ">", "hi ", "{", "y", "}", "<", "/", "a", ">", ";",
    ]);
    expect(jsx.kind(6)).toBe(TokenKind.JSXText);
  });

  test("flags", () => {
    const tokens = list("a\nb \\u0061sync `\\unicode`");
    expect(tokens.newlineBefore(0)).toBe(false);
    expect(tokens.newlineBefore(1)).toBe(true);
    expect(tokens.escaped(2)).toBe(true);
    expect(tokens.kind(2)).toBe(TokenKind.Async);
    expect(tokens.invalidEscape(3)).toBe(true);
    expect(tokens.loneSurrogate(3)).toBe(false);
  });

  test("a rescanned token keeps the newline before the token it replaces", () => {
    expect(list("x =\n/a/;").newlineBefore(2)).toBe(true);
    expect(list("`a${b\n}c`;").newlineBefore(2)).toBe(true);
    const generic = list("let x: A<B<C\n>> = y;", { lang: "ts" });
    expect(generic.newlineBefore(8)).toBe(true);
    expect(generic.newlineBefore(9)).toBe(false);
    expect(list("<C<T>\nfoo />;", { lang: "tsx" }).newlineBefore(5)).toBe(true);
  });

  test("lone surrogates keep every span in UTF-16", () => {
    const source = `const s = "\uD800\uD800"; after;`;
    const { program, tokens } = parse(source, { tokens: true });
    expect(spans(tokens)).toEqual([
      [0, 5], [6, 7], [8, 9], [10, 14], [14, 15], [16, 21], [21, 22],
    ]);
    expect(program.body[1]).toMatchObject({ start: 16, end: 22 });
  });

  test("offsets are UTF-16 like nodes", () => {
    const source = `const s = "😀"; s;`;
    const { program, tokens } = parse(source, { tokens: true });
    expect(source.slice(tokens.start(3), tokens.end(3))).toBe(`"😀"`);
    expect(tokens.start(5)).toBe(program.body[1]!.start);
  });

  test("span queries", () => {
    const source = "const a = 1;\nfoo(a, 2);";
    const { program, tokens: t } = parse(source, { tokens: true });
    const [decl, call] = program.body as unknown as [Span, Span];

    expect(t.range(decl)).toEqual([0, 5]);
    expect(t.range(call)).toEqual([5, 12]);
    expect(t.before(call.start)).toBe(4);
    expect(t.after(decl.end)).toBe(5);
    expect(t.before(decl.start)).toBe(-1);
    expect(t.after(call.end)).toBe(-1);

    const paren = list("foo(a)");
    expect(paren.text(paren.before(4))).toBe("(");
    expect(paren.text(paren.after(4))).toBe("a");

    const foo = source.indexOf("foo");
    expect(t.at(foo)).toBe(5);
    expect(t.at(foo + 1)).toBe(5);
    expect(t.at(foo - 1)).toBe(-1);
    expect(t.at(source.length)).toBe(-1);
    expect(t.before(foo)).toBe(4);
    expect(t.after(foo)).toBe(5);

    const [from, to] = t.range({ start: foo + 1, end: foo + 1 });
    expect(to - from).toBe(0);
  });

  test("offset queries at the edges of the source", () => {
    const source = "a  +\n b";
    const t = list(source);
    expect([t.before(0), t.after(0), t.at(0)]).toEqual([-1, 0, 0]);
    expect([t.before(2), t.after(2), t.at(2)]).toEqual([0, 1, -1]);
    const end = source.length;
    expect([t.before(end), t.after(end), t.at(end)]).toEqual([2, -1, -1]);
    const empty = list("");
    expect([empty.before(0), empty.after(0), empty.at(0)]).toEqual([-1, -1, -1]);
    expect(empty.range({ start: 0, end: 0 })).toEqual([0, 0]);
  });

  test("tokens survive syntax errors", () => {
    const { tokens, diagnostics } = parse("let = ) ;\nfoo(1);", { tokens: true });
    expect(diagnostics.length).toBeGreaterThan(0);
    expect(values(tokens)).toEqual(["let", "=", ")", ";", "foo", "(", "1", ")", ";"]);
  });

  test("an out of range index throws", () => {
    const tokens = list("a");
    expect(() => tokens.kind(1)).toThrow(RangeError);
    expect(() => tokens.text(-1)).toThrow(RangeError);
  });

  test("a UTF-8 byte source reads back like its string", () => {
    const source = "\uFEFFlet après = '🎉'; // ü";
    const bytes = new TextEncoder().encode(source);
    expect(parse(bytes).program).toEqual(parse(source).program);
    expect(values(list(bytes))).toEqual(values(list(source)));
  });
});
