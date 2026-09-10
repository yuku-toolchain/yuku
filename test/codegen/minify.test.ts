import { expect, test } from "bun:test";
import { gen } from "./helpers";

const MINIFY = { minify: true } as const;

test("keeps a legal banner and a pure annotation", () => {
  expect(gen(`/*! ©2025 */\nconst x = /*#__PURE__*/ make();`, MINIFY))
    .toMatchInlineSnapshot(`
      "/*! ©2025 */
      const x=/*#__PURE__*/make()"
    `);
});

test("shortens numbers to their shortest form", () => {
  expect(
    gen(
      `const a = 1000000000000000000000;
const b = 0.0001;
const c = 1500000;
const d = 123400000;
const e = 0.5;
const f = 1000;
const g = 1234.5;`,
      MINIFY,
    ),
  ).toMatchInlineSnapshot(
    `"const a=1e21;const b=1e-4;const c=15e5;const d=1234e5;const e=.5;const f=1e3;const g=1234.5"`,
  );
});

test("escapes script-close sequences in strings, keeps template raw", () => {
  expect(
    gen(
      'const a = "</script>";\nconst b = "<!-- c -->";\nconst c = `</script>${1}`;\nconst d = "x-->y";',
      MINIFY,
    ),
  ).toMatchInlineSnapshot(
    `"const a="<\\/script>";const b="<\\!-- c --\\>";const c=\`</script>\${1}\`;const d="x--\\>y""`,
  );
});

test("rewrites only where the meaning is preserved", () => {
  expect(
    gen(
      `function f(undefined) { return undefined; }
const o = { undefined, Infinity, ["__proto__"]: a, "__proto__": b, "$ref": 1, ["_p"]: 2 };
export { undefined };
obj["_x"]; obj["$y"];
b = 010; c = 08;
const t = String.raw\`a\\nb\\x41\`; tag\`\\unicode\`;`,
      MINIFY,
      "input.js",
    ),
  ).toMatchInlineSnapshot(
    `"function f(undefined){return undefined}const o={undefined,Infinity,["__proto__"]:a,__proto__:b,$ref:1,_p:2};export{undefined};obj._x;obj.$y;b=010;c=08;const t=String.raw\`a\\nb\\x41\`;tag\`\\unicode\`"`,
  );
});

test("keeps banner comments around a leading semicolon", () => {
  expect(gen(`const x = 1; /*! keep */\nfoo();\na();\n/*! banner */\nb();`, MINIFY))
    .toMatchInlineSnapshot(`
      "const x=1;/*! keep */foo();a();
      /*! banner */
      b()"
    `);
});

test("a string literal keeps its trailing space in compact output", () => {
  expect(gen(`throw new RangeError("token index " + i + " is out of range");`, MINIFY))
    .toMatchInlineSnapshot(`"throw new RangeError("token index "+i+" is out of range")"`);
});

test("template text keeps a space before an interpolation and the closing backtick", () => {
  expect(gen("const s = `a ${b} c `;", MINIFY)).toMatchInlineSnapshot(
    '"const s=`a ${b} c `"',
  );
});

test("a regex pattern ending in a space keeps it before the closing slash", () => {
  expect(gen("const r = /a /g;", MINIFY)).toMatchInlineSnapshot('"const r=/a /g"');
});
