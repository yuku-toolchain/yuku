import { describe, expect, test } from "bun:test";
import { js, nameOf, parse, pattern, print, walk, type Node } from "yuku";

function matches(source: string, code: string): Node[] {
  const { program } = parse(source, { path: "input.ts" });
  const compiled = pattern(code);
  const found: Node[] = [];
  walk(program, {
    [compiled.type]: (node: Node) => {
      if (compiled.match(node) !== null) found.push(node);
    },
  });
  return found;
}

function texts(source: string, code: string): string[] {
  return matches(source, code).map((node) => source.slice(node.start, node.end));
}

describe("pattern", () => {
  test("captures one node per `$NAME` and a list per `$$$NAME`", () => {
    const method = pattern("$OBJ.$METHOD($$$ARGS)");
    const [log, f, run] = parse(`console.log(a, b); f(); obj.run();`).program.body.map((node) =>
      node.type === "ExpressionStatement" ? method.match(node.expression) : null,
    );
    expect(nameOf(log!.METHOD)).toBe("log");
    expect(log!.ARGS.map((node) => print(node).code)).toEqual(["a", "b"]);
    expect(f).toBeNull();
    expect(run!.ARGS).toEqual([]);
  });

  test("a list metavariable takes any run of items around fixed ones", () => {
    expect(texts(`f(1, 2, last); f(last); f(1, other);`, "f($$$BEFORE, last)")).toEqual([
      "f(1, 2, last)",
      "f(last)",
    ]);
  });

  test("a name used twice matches equal nodes, `$_` anything uncaptured", () => {
    expect(texts(`a === a; a === b; f(x) === f(x);`, "$X === $X")).toEqual([
      "a === a",
      "f(x) === f(x)",
    ]);
    const [statement] = parse(`a === b;`).program.body;
    if (statement?.type !== "ExpressionStatement") throw new Error("expected an expression");
    expect(pattern("$_ === $_").match(statement.expression)).toEqual({});
  });

  test("a repeated name compares captures of any depth, as code and not as patterns", () => {
    const deep = Array.from({ length: 5_000 }, () => "a").join(" + ");
    expect(texts(`${deep} === ${deep};`, "$X === $X")).toHaveLength(1);
    expect(texts(`$A === $A; $A === $B;`, "$X === $X")).toEqual(["$A === $A"]);
    expect(texts(`f((a)) === f(a);`, "$X === $X")).toEqual(["f((a)) === f(a)"]);
  });

  test("a pattern that backtracks past its budget throws", () => {
    const call = `f(${Array.from({ length: 200 }, (_, i) => `a${i}`).join(", ")});`;
    expect(() => texts(call, "f($$$A, $$$B, $$$C, x)")).toThrow("backtracks past");
  });

  test("a pattern ending in `;` matches statements, parentheses never count", () => {
    expect(texts(`f(); g(f());`, "f();")).toEqual(["f();"]);
    expect(texts(`f(); g(f());`, "f()")).toEqual(["f()", "f()"]);
    expect(texts(`x = (a + b);`, "$A + $B")).toEqual(["a + b"]);
    expect(texts(`if (a) b();`, "if ($C) $S;")).toEqual(["if (a) b();"]);
  });

  test("literal values and type syntax", () => {
    expect(texts(`f(1); f(2); f("1");`, "f(1)")).toEqual(["f(1)"]);
    expect(texts(`let x: string = y as string;`, "$E as string")).toEqual(["y as string"]);
  });

  test("patterns test nodes inside one walk, which edits what they match", () => {
    const file = parse(`console.log(a); console.warn(b); f(c);`);
    const log = pattern("console.log($$$ARGS)");
    const warn = pattern("console.warn($$$ARGS)");
    walk(file.program, {
      CallExpression(node, ctx) {
        const captures = log.match(node) ?? warn.match(node);
        if (captures !== null) ctx.replace(js`logger.info(${captures.ARGS})`);
      },
    });
    expect(print(file).code).toBe("logger.info(a);\nlogger.info(b);\nf(c);");
    expect(log.match(file.program)).toBeNull();
    expect(log.type).toBe("CallExpression");
  });

  test("malformed patterns throw", () => {
    expect(() => pattern("a; b;")).toThrow("one expression or statement, not 2");
    expect(() => pattern("let = ;")).toThrow(SyntaxError);
    expect(() => pattern("$X")).toThrow("matches any node");
    expect(() => pattern("$S;")).toThrow("matches any node");
  });
});
