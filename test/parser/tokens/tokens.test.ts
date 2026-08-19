import { describe, expect, test } from "bun:test";
import { parse, type SourceLang, type Token } from "yuku-parser";

function tokens(source: string, lang: SourceLang = "js"): Token[] {
  return parse(source, { tokens: true, lang }).tokens;
}

function shapeOf(list: Token[]): string[] {
  return list.map((t) => `${t.type}(${t.value})`);
}

function shape(source: string, lang: SourceLang = "js"): string[] {
  return shapeOf(tokens(source, lang));
}

function types(source: string, lang: SourceLang = "js"): string[] {
  return tokens(source, lang).map((t) => t.type);
}

describe("collection", () => {
  test("off by default", () => {
    expect(parse("let x = 1;").tokens).toEqual([]);
  });

  test("nothing to collect yields nothing", () => {
    expect(tokens("")).toEqual([]);
    expect(tokens("   \n\t ")).toEqual([]);
    // no eof token either
    expect(shape("x")).toEqual(["Identifier(x)"]);
  });

  test("trivia is not a token", () => {
    const result = parse("#!/usr/bin/env node\n/* a */ x; // b", {
      tokens: true,
      sourceType: "commonjs",
    });
    expect(shapeOf(result.tokens)).toEqual(["Identifier(x)", "Punctuator(;)"]);
    expect(result.comments.map((c) => c.value)).toEqual([" a ", " b"]);
  });

  test("tokens never overlap comments", () => {
    const result = parse("a /* x */ = /* y */ 1; // z", { tokens: true });
    for (const t of result.tokens) {
      for (const c of result.comments) {
        expect(t.start >= c.end || t.end <= c.start).toBe(true);
      }
    }
  });

  test("tokens survive a syntax error", () => {
    const result = parse("let x = ;", { tokens: true });
    expect(result.diagnostics.length).toBeGreaterThan(0);
    expect(shapeOf(result.tokens)).toEqual([
      "Keyword(let)",
      "Identifier(x)",
      "Punctuator(=)",
      "Punctuator(;)",
    ]);
  });

  test("tokens are sorted, non-empty, and never overlap", () => {
    const source = "class A { #p = /a/g; m() { return `x${this.#p}y`; } } <b c='d'>e</b>;";
    const list = tokens(source, "tsx");
    expect(list.length).toBeGreaterThan(10);
    let previousEnd = 0;
    for (const t of list) {
      expect(t.end).toBeGreaterThan(t.start);
      expect(t.start).toBeGreaterThanOrEqual(previousEnd);
      previousEnd = t.end;
    }
  });

  test("offsets are UTF-16 code units", () => {
    const source = "const é = '𝒳'; let z = 1;";
    for (const t of tokens(source)) {
      expect(t.value).toBe(source.slice(t.start, t.end));
    }
  });
});

describe("classification", () => {
  test("a reserved word read as a name is an identifier", () => {
    // the lexer classifies from the text, then the parser retypes what it reads
    // as a name, which is what ESLint reports
    expect(shape("p.catch(f).default;").filter((s) => s.startsWith("Identifier"))).toEqual([
      "Identifier(p)",
      "Identifier(catch)",
      "Identifier(f)",
      "Identifier(default)",
    ]);
    expect(types("o = { return: 1, true: 2, null: 3 };").filter((t) => t === "Keyword")).toEqual([]);
  });

  test("let, static and yield are keywords wherever they appear", () => {
    // never reserved to ESLint's tokenizer, so reported from their spelling
    expect(shape("o = { let: 1, static: 2, yield: 3 };").filter((s) => s.startsWith("Keyword")))
      .toEqual(["Keyword(let)", "Keyword(static)", "Keyword(yield)"]);
  });

  test("a meta property keeps its keyword", () => {
    // the parser names the node after a keyword it consumed as a keyword
    expect(shape("function f(){ return new.target; }").slice(-5, -1)).toEqual([
      "Keyword(new)",
      "Punctuator(.)",
      "Identifier(target)",
      "Punctuator(;)",
    ]);
    expect(shape("import.meta.url;").slice(0, 3)).toEqual([
      "Keyword(import)",
      "Punctuator(.)",
      "Identifier(meta)",
    ]);
    // the same words read as names are identifiers
    expect(shape("class C { new() {} import() {} }").filter((s) => s.startsWith("Identifier")))
      .toEqual(["Identifier(C)", "Identifier(new)", "Identifier(import)"]);
  });

  test("identifier values resolve unicode escapes", () => {
    expect(shape("var a\\u0061 = \\u{41}BC;").slice(1, 4)).toEqual([
      "Identifier(aa)",
      "Punctuator(=)",
      "Identifier(ABC)",
    ]);
    // a string or template keeps the source spelling
    expect(shape("x = '\\u0061' + `\\u0062`;").slice(2, 5)).toEqual([
      "String('\\u0061')",
      "Punctuator(+)",
      "Template(`\\u0062`)",
    ]);
  });

  test("private identifiers drop the hash", () => {
    const [first] = tokens("class A { #p = 1; }").filter((t) => t.type === "PrivateIdentifier");
    expect(first!.value).toBe("p");
    // the span still covers the `#`
    expect(first!.end - first!.start).toBe(2);
  });

  test("only regular expressions carry pattern and flags", () => {
    const [regex] = tokens("x = /ab+c/gi;").filter((t) => t.type === "RegularExpression");
    expect(regex!.value).toBe("/ab+c/gi");
    expect(regex!.regex).toEqual({ pattern: "ab+c", flags: "gi" });
    for (const t of tokens("x = /a/g + 1 + 'y';")) {
      if (t.type !== "RegularExpression") expect(t.regex).toBeUndefined();
    }
  });
});

describe("typescript", () => {
  test("TS-only words are identifiers", () => {
    for (const word of ["type", "readonly", "declare", "abstract", "public", "interface", "any"]) {
      expect(types(`a; ${word}`, "ts")[2]).toBe("Identifier");
    }
  });

  test("a nested type argument list closes with two separate tokens", () => {
    expect(shape("let a: Foo<Bar<T>> = x;", "ts").slice(-5)).toEqual([
      "Punctuator(>)",
      "Punctuator(>)",
      "Punctuator(=)",
      "Identifier(x)",
      "Punctuator(;)",
    ]);
  });

  test("a type argument list opening on a fused angle splits it", () => {
    expect(shape("f<<T>(v: T) => void>();", "ts").slice(0, 4)).toEqual([
      "Identifier(f)",
      "Punctuator(<)",
      "Punctuator(<)",
      "Identifier(T)",
    ]);
  });

  test("a fused shift operator stays fused when it is a shift", () => {
    expect(shape("q = p << 1 >> 2;", "ts").slice(3, 7)).toEqual([
      "Punctuator(<<)",
      "Numeric(1)",
      "Punctuator(>>)",
      "Numeric(2)",
    ]);
  });

  test("a shift after an optional chain survives the speculative type parse", () => {
    // the speculative type argument list splits the `<<`, then rewinds
    expect(shape("a?.b << c;", "ts")).toEqual([
      "Identifier(a)",
      "Punctuator(?.)",
      "Identifier(b)",
      "Punctuator(<<)",
      "Identifier(c)",
      "Punctuator(;)",
    ]);
  });

  test("a bare instantiation expression keeps its angles", () => {
    expect(shape("a?.b<T>;", "ts").slice(3, 6)).toEqual([
      "Punctuator(<)",
      "Identifier(T)",
      "Punctuator(>)",
    ]);
  });
});
