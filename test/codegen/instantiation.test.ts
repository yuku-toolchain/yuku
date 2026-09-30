import { expect, test } from "bun:test";
import { parse, type ParseOptions } from "yuku-parser";
import { generate, type GenerateOptions } from "yuku-codegen";
import { astDiffPath } from "../ast-helpers-for-test";

test("instantiation operands keep their grammar boundaries", () => {
    // compare the AST and printing fixed point after rebuilding required parentheses
    const sources = [
        "(f<T>)<U>();",
        "(f<T>)!;",
        "(f<T>)<U>;",
        "new (f<T>)<U>();",
        "(f<T>)<U>`x`;",
        "class C extends (f<T>)<U> {}",
        "((f<T>)!)<U>();",
        "((f<T>)!)!;",
        "(((f<T>)<U>)!)<V>();",
        "(f<T>).x;",
        "(f<T>)[x];",
        "(f<T>)();",
        "new (f<T>)();",
        "(f<T>)`x`;",
        "class C extends (f<T>) {}",
        "(f<T>)?.<U>();",
        "(f<T>)?.x;",
        "(f<T>)?.[x];",
        "f<T>();",
        "new f<T>();",
        "f<T>`x`;",
        "f!<T>();",
        "f<T>;",
    ];
    const formats: GenerateOptions[] = [{}, { format: "compact" }, { minify: true }];

    for (const lang of ["ts", "tsx"] as const) {
        const parseOptions: ParseOptions = { lang, preserveParens: false };
        for (const source of sources) {
            const original = parse(source, parseOptions);
            expect(original.diagnostics, source).toEqual([]);
            const plans: GenerateOptions[] = [...formats, { sourceMap: { source } }];
            for (const options of plans) {
                const code = generate(original.program, options).code;
                const reparsed = parse(code, parseOptions);
                expect(reparsed.diagnostics, code).toEqual([]);
                expect(astDiffPath(original.program, reparsed.program), code).toBeNull();
                expect(generate(reparsed.program, options).code, source).toBe(code);
            }
            const stripped = generate(original.program, { strip: true }).code;
            expect(parse(stripped, { lang: "js" }).diagnostics, stripped).toEqual([]);
        }
    }
});

test("instantiation parentheses are inserted only where needed", () => {
    // ordinary generic calls and optional chains already disambiguate their type arguments
    for (const source of ["f<T>();", "new f<T>();", "f<T>`x`;", "f<T>;", "f<T>?.<U>();"]) {
        const original = parse(source, { lang: "ts", preserveParens: false });
        expect(original.diagnostics, source).toEqual([]);
        expect(generate(original.program).code).toBe(source);
    }
    for (const source of ["(f<T>)!;", "(f<T>)<U>();", "(f<T>).x;"]) {
        const original = parse(source, { lang: "ts", preserveParens: false });
        expect(original.diagnostics, source).toEqual([]);
        expect(generate(original.program).code).toBe(source);
    }
});
