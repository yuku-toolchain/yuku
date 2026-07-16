// exercises large first IndexRange fields through parser, codegen, and decoder boundaries
// repeated tokens keep every boundary length and representative span deterministic

import { copyFile } from "node:fs/promises";
import { describe, expect, test } from "vitest";
import { Analyzer } from "../../npm/yuku-analyzer/index.js";
import { print } from "../../npm/yuku-codegen/index.js";
import { parse } from "../../npm/yuku-parser/index.js";

await Promise.all([
    copyFile(
        new URL("../../zig-out/bin/yuku-parser.wasm", import.meta.url),
        new URL("../../npm/yuku-parser-wasm/yuku-parser.wasm", import.meta.url),
    ),
    copyFile(
        new URL("../../zig-out/bin/yuku-codegen.wasm", import.meta.url),
        new URL("../../npm/yuku-codegen-wasm/yuku-codegen.wasm", import.meta.url),
    ),
]);

const { parse: parseWasm } = await import("../../npm/yuku-parser-wasm/index.js");
const { print: printWasm } = await import("../../npm/yuku-codegen-wasm/index.js");

const parseOptions = {
    sourceType: "module",
    lang: "js",
    preserveParens: true,
    allowReturnOutsideFunction: false,
    semanticErrors: false,
    attachComments: false,
} as const;
const analyzerFileOptions = {
    sourceType: "module",
    lang: "js",
    preserveParens: true,
    allowReturnOutsideFunction: false,
    attachComments: false,
} as const;
const codegenOptions = {
    format: "compact",
    indent: 2,
    quotes: "preserve",
    sourceMaps: undefined,
    comments: "some",
} as const;

describe("large AST ranges", () => {
    test.each([65_535, 65_536, 65_537, 70_000])(
        "preserves %i top-level statements",
        (statementCount) => {
            const result = parse(";".repeat(statementCount), parseOptions);
            const middle = Math.floor(statementCount / 2);

            expect(result.diagnostics).toHaveLength(0);
            expect(result.program.body).toHaveLength(statementCount);
            expect(result.program.body[0]).toMatchObject({
                type: "EmptyStatement",
                start: 0,
                end: 1,
            });
            expect(result.program.body[middle]).toMatchObject({
                type: "EmptyStatement",
                start: middle,
                end: middle + 1,
            });
            expect(result.program.body[statementCount - 1]).toMatchObject({
                type: "EmptyStatement",
                start: statementCount - 1,
                end: statementCount,
            });
        },
    );

    test("preserves 65,536 statements in a block", () => {
        const statementCount = 65_536;
        const source = `{${";".repeat(statementCount)}}`;
        const result = parse(source, parseOptions);
        const block = result.program.body[0];

        expect(result.diagnostics).toHaveLength(0);
        expect(block).toMatchObject({
            type: "BlockStatement",
            start: 0,
            end: source.length,
        });
        if (block?.type !== "BlockStatement") throw new Error("Expected a block statement");
        expect(block.body).toHaveLength(statementCount);
        expect(block.body[0]).toMatchObject({ type: "EmptyStatement", start: 1, end: 2 });
        expect(block.body[32_768]).toMatchObject({
            type: "EmptyStatement",
            start: 32_769,
            end: 32_770,
        });
        expect(block.body[65_535]).toMatchObject({
            type: "EmptyStatement",
            start: 65_536,
            end: 65_537,
        });
    });

    test("preserves 65,536 array elements", () => {
        const elementCount = 65_536;
        const source = `[${"0,".repeat(elementCount - 1)}0]`;
        const result = parse(source, parseOptions);
        const statement = result.program.body[0];

        expect(result.diagnostics).toHaveLength(0);
        if (statement?.type !== "ExpressionStatement") {
            throw new Error("Expected an expression statement");
        }
        const expression = statement.expression;
        if (expression.type !== "ArrayExpression") throw new Error("Expected an array expression");
        expect(expression.elements).toHaveLength(elementCount);
        expect(expression.elements[0]).toMatchObject({ type: "Literal", start: 1, end: 2 });
        expect(expression.elements[32_768]).toMatchObject({
            type: "Literal",
            start: 65_537,
            end: 65_538,
        });
        expect(expression.elements[65_535]).toMatchObject({
            type: "Literal",
            start: 131_071,
            end: 131_072,
        });
    });

    test("preserves 65,536 function parameters", () => {
        const parameterCount = 65_536;
        const source = `function f(${"a,".repeat(parameterCount - 1)}a){}`;
        const result = parse(source, parseOptions);
        const declaration = result.program.body[0];

        expect(result.diagnostics).toHaveLength(0);
        if (declaration?.type !== "FunctionDeclaration") {
            throw new Error("Expected a function declaration");
        }
        expect(declaration.params).toHaveLength(parameterCount);
        expect(declaration.params[0]).toMatchObject({ type: "Identifier", start: 11, end: 12 });
        expect(declaration.params[32_768]).toMatchObject({
            type: "Identifier",
            start: 65_547,
            end: 65_548,
        });
        expect(declaration.params[65_535]).toMatchObject({
            type: "Identifier",
            start: 131_081,
            end: 131_082,
        });
    });

    test("prints 65,536 top-level statements", () => {
        const statementCount = 65_536;
        const program = parse(";".repeat(statementCount), parseOptions).program;
        const printed = print(program, codegenOptions);

        expect(printed.errors).toHaveLength(0);
        expect(parse(printed.code, parseOptions).program.body).toHaveLength(statementCount);
    });

    test("analyzes parent links for 65,536 top-level statements", () => {
        const statementCount = 65_536;
        const analyzer = new Analyzer({ resolve: () => null });
        const module = analyzer.addFile(
            "large.js",
            ";".repeat(statementCount),
            analyzerFileOptions,
        );
        const lastStatement = module.ast.body[statementCount - 1];

        expect(module.diagnostics).toHaveLength(0);
        expect(module.ast.body).toHaveLength(statementCount);
        expect(lastStatement).toMatchObject({
            type: "EmptyStatement",
            start: statementCount - 1,
            end: statementCount,
        });
        expect(lastStatement).toBeDefined();
        expect(module.parentOf(lastStatement!)).toBe(module.ast);
    });

    test("parses 65,536 top-level statements with WebAssembly", () => {
        const statementCount = 65_536;
        const result = parseWasm(";".repeat(statementCount), parseOptions);

        expect(result.diagnostics).toHaveLength(0);
        expect(result.program.body).toHaveLength(statementCount);
        expect(result.program.body[statementCount - 1]).toMatchObject({
            type: "EmptyStatement",
            start: statementCount - 1,
            end: statementCount,
        });
    });

    test("prints 65,536 top-level statements with WebAssembly", () => {
        const statementCount = 65_536;
        const program = parse(";".repeat(statementCount), parseOptions).program;
        const code = printWasm(program);

        expect(parse(code, parseOptions).program.body).toHaveLength(statementCount);
    });
});
