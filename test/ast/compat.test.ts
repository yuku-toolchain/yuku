import { describe, expect, test } from "bun:test";
import { WalkContext as AstWalkContext } from "yuku-ast";
import { parse, walk, WalkContext } from "yuku-parser";

describe("deprecated yuku-parser walking re-exports", () => {
  test("walk delegates to yuku-ast and warns once", () => {
    const { program } = parse(`let a = 1; a;`);
    const names: string[] = [];
    walk(program, { Identifier: (node) => names.push(node.name) });
    walk(program, { Identifier: () => {} });
    expect(names).toEqual(["a", "a"]);
  });

  test("WalkContext is the yuku-ast class", () => {
    expect(WalkContext).toBe(AstWalkContext);
  });
});
