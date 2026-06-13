import { expect, test } from "bun:test";
import { gen } from "./helpers";

const COMPACT = { format: "compact" } as const;

test("keeps a legal banner and a pure annotation", () => {
  expect(gen("minify", `/*! ©2025 */\nconst x = /*#__PURE__*/ make();`, COMPACT))
    .toMatchInlineSnapshot(`
      "/*! ©2025 */
      const x=/*#__PURE__*/make()"
    `);
});

test("shortens numbers to their shortest form", () => {
  expect(
    gen(
      "minify",
      `const a = 1000000000000000000000;
const b = 0.0001;
const c = 1500000;
const d = 123400000;
const e = 0.5;
const f = 1000;
const g = 1234.5;`,
      COMPACT,
    ),
  ).toMatchInlineSnapshot(
    `"const a=1e21;const b=1e-4;const c=15e5;const d=1234e5;const e=.5;const f=1e3;const g=1234.5"`,
  );
});

test("escapes script-close sequences in strings and templates", () => {
  expect(
    gen(
      "minify",
      'const a = "</script>";\nconst b = "<!-- c -->";\nconst c = `</script>${1}`;\nconst d = "x-->y";',
      COMPACT,
    ),
  ).toMatchInlineSnapshot(
    `"const a="<\\/script>";const b="<\\!-- c --\\>";const c=\`<\\/script>\${1}\`;const d="x--\\>y""`,
  );
});

test("keeps banner comments around a leading semicolon", () => {
  expect(gen("minify", `const x = 1; /*! keep */\nfoo();\na();\n/*! banner */\nb();`, COMPACT))
    .toMatchInlineSnapshot(`
      "const x=1;/*! keep */foo();a();
      /*! banner */
      b()"
    `);
});
