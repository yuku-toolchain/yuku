import { expect, test } from "bun:test";
import { parse } from "yuku-parser";
import { generate } from "yuku-codegen";
import { astDiffPath } from "../ast-helpers-for-test";

test("async function primary expressions parse and round-trip", () => {
    // check restricted expression positions without evaluating non-constructible functions
    const sources = [
        "new async function(){};",
        "new async function named(){};",
        "new async function*(){};",
        "new async function* named(){};",
        'new async function(){}.constructor("return 1");',
        "new async function*(){}.constructor();",
        "new (async function(){})();",
        "new async /* comment */ function(){};",
        "new async\nfunction f(){}",
        "new async /*\n*/ function f(){}",
        "class C extends async function(){} {}",
        "class C extends async function(){}.constructor {}",
        "class C extends async function*(){} {}",
        "class C extends async function*(){}.constructor {}",
        "class C extends (async function(){}) {}",
        "class C extends async /* comment */ function(){} {}",
    ];

    for (const lang of ["js", "jsx", "ts", "tsx"] as const) {
        for (const sourceType of ["script", "module"] as const) {
            const options = { lang, sourceType, semanticErrors: true, preserveParens: false };
            for (const source of sources) {
                const original = parse(source, options);
                expect(original.diagnostics, source).toEqual([]);
                const code = generate(original.program).code;
                const reparsed = parse(code, options);
                expect(reparsed.diagnostics, code).toEqual([]);
                expect(astDiffPath(original.program, reparsed.program), code).toBeNull();
            }
        }
    }
});

test("async primary expressions retain identifier and function shapes", () => {
    // constructor arguments belong to new and heritage calls remain ordinary calls
    const options = { sourceType: "script", preserveParens: false } as const;
    const cases = [
        ["new async(1);", "Identifier", false],
        ["new async function(){};", "FunctionExpression", false],
        ["new async function*(){};", "FunctionExpression", true],
    ] as const;
    for (const [source, type, generator] of cases) {
        const result = parse(source, options);
        expect(result.diagnostics, source).toEqual([]);
        const statement = result.program.body[0];
        expect(statement?.type).toBe("ExpressionStatement");
        if (statement?.type !== "ExpressionStatement") throw new Error("expected expression");
        const expression = statement.expression;
        expect(expression.type).toBe("NewExpression");
        if (expression.type !== "NewExpression") throw new Error("expected new expression");
        expect(expression.callee.type).toBe(type);
        if (expression.callee.type === "FunctionExpression") {
            expect(expression.callee.async).toBe(true);
            expect(expression.callee.generator).toBe(generator);
        } else {
            expect(expression.arguments).toHaveLength(1);
        }
    }
    const result = parse("class C extends async() {}", options);
    expect(result.diagnostics).toEqual([]);
    const statement = result.program.body[0];
    expect(statement?.type).toBe("ClassDeclaration");
    if (statement?.type !== "ClassDeclaration") throw new Error("expected class declaration");
    expect(statement.superClass?.type).toBe("CallExpression");
    if (statement.superClass?.type !== "CallExpression") throw new Error("expected call");
    expect(statement.superClass.callee.type).toBe("Identifier");
});

test("async primary expressions enforce keyword and arrow restrictions", () => {
    // async functions share escape and await rules but restricted positions never admit arrows
    const sources = [
        String.raw`new as\u0079nc function(){};`,
        String.raw`class C extends as\u0079nc function(){} {}`,
        String.raw`(as\u0079nc function(){});`,
        "new async () => {};",
        "new async x => x;",
        "class C extends async () => {} {}",
        "class C extends async x => x {}",
        "class C extends async\nfunction(){} {}",
        "class C extends async /*\n*/ function(){} {}",
        "new async function(await){};",
        "new async function* (yield){};",
        "class C extends async function await(){} {}",
    ];
    for (const lang of ["js", "ts"] as const) {
        for (const sourceType of ["script", "module"] as const) {
            for (const source of sources) {
                const result = parse(source, { lang, sourceType, semanticErrors: true });
                expect(result.diagnostics.length, source).toBeGreaterThan(0);
            }
        }
    }
});
