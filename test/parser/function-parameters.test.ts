import { expect, test } from "bun:test";
import { parse } from "yuku-parser";
import { generate } from "yuku-codegen";

const functionHeads = ["function", "function*", "async function", "async function*"];

test("sloppy functions allow duplicate simple parameters", () => {
    // declarations and expressions use the same parameter grammar regardless of function flags
    for (const lang of ["js", "jsx", "ts", "tsx"] as const) {
        const options = { lang, sourceType: "script", semanticErrors: true } as const;
        for (const head of functionHeads) {
            for (const source of [
                `${head} f(a,a){return a;}`,
                `(${head}(a,a){return a;});`,
                `(${head} f(a,a,a){return a;});`,
                `${head} f(a,a){"use asm"; return a;}`,
            ]) {
                const result = parse(source, options);
                expect(result.diagnostics, source).toEqual([]);
                const code = generate(result.program).code;
                expect(parse(code, options).diagnostics, code).toEqual([]);
            }
        }
    }
});

test("strict functions still reject duplicate simple parameters", () => {
    // strictness comes from the module, the enclosing scope, or the function body directive
    for (const lang of ["js", "ts"] as const) {
        for (const head of functionHeads) {
            for (const source of [
                `${head} f(a,a){}`,
                `(${head}(a,a){});`,
            ]) {
                const result = parse(source, { lang, sourceType: "module", semanticErrors: true });
                expect(result.diagnostics.map((error) => error.message), source)
                    .toContain("Identifier 'a' has already been declared");
            }
            for (const source of [
                `"use strict"; ${head} f(a,a){}`,
                `${head} f(a,a){"use strict";}`,
                `(${head}(a,a){"use strict";});`,
                `function outer(){"use strict"; ${head} f(a,a){}}`,
            ]) {
                const result = parse(source, { lang, sourceType: "script", semanticErrors: true });
                expect(result.diagnostics.map((error) => error.message), source)
                    .toContain("Identifier 'a' has already been declared");
            }
        }
    }
});

test("non-simple function parameters still require unique names", () => {
    // one initializer, rest element, or binding pattern makes the whole list non-simple
    const parameters = ["a,a,b=0", "a=0,a", "a,...a", "a,a,...rest", "a,{a}", "{a,x:a}"];
    for (const lang of ["js", "ts"] as const) {
        for (const head of functionHeads) {
            for (const list of parameters) {
                for (const source of [`${head} f(${list}){}`, `(${head}(${list}){});`]) {
                    const result = parse(source, {
                        lang,
                        sourceType: "script",
                        semanticErrors: true,
                    });
                    expect(result.diagnostics.map((error) => error.message), source)
                        .toContain("Identifier 'a' has already been declared");
                }
            }
        }
    }
});

test("methods and arrows still reject duplicate parameters in sloppy scripts", () => {
    // uniqueness belongs to the grammar production rather than the async or generator flag
    const sources = [
        "({f(a,a){}});",
        "({*f(a,a){}});",
        "({async f(a,a){}});",
        "({async *f(a,a){}});",
        "(a,a)=>{};",
        "async (a,a)=>{};",
        "class C {constructor(a,a){}}",
        "class C {f(a,a){}}",
        "class C {*f(a,a){}}",
        "class C {async f(a,a){}}",
        "class C {async *f(a,a){}}",
    ];
    for (const lang of ["js", "ts"] as const) {
        for (const source of sources) {
            const result = parse(source, { lang, sourceType: "script", semanticErrors: true });
            expect(result.diagnostics.map((error) => error.message), source)
                .toContain("Identifier 'a' has already been declared");
        }
    }
});
