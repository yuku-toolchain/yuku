import { parse } from "yuku-parser";
import { generate } from "yuku-codegen";

const source = `
function greet(name: string) {
  const greeting = "hello, " + name;
  return greeting;
}

const numbers = [1, 2, 3, 4];
const doubled = numbers.map(n => n * 2);

if (doubled.length > 0) {
  console.log("nonempty");
} else {
  console.log("empty");
}

class Counter {
  count: number = 0;
  increment(): void {
    this.count = this.count + 1;
  }
}
`.trim();

function run(label: string, mutate: (program: any) => void) {
  const ast = parse(source, { lang: "ts" });
  mutate(ast.program);
  const out = generate(ast.program, { format: { format: "pretty" } });

  // Validate: the regenerated source must parse cleanly again.
  const back = parse(out.code, { lang: "ts" });
  const ok = back.diagnostics.length === 0;

  console.log(`--- ${label} ---`);
  console.log(out.code);
  console.log(`[reparse: ${ok ? "ok" : back.diagnostics.length + " errors"}]\n`);
}

// 1. Rename a function declaration through the proxy.
run("rename function", (program) => {
  for (const stmt of program.body) {
    if (stmt.type === "FunctionDeclaration" && stmt.id?.name === "greet") {
      stmt.id.name = "salute";
    }
  }
});

// 2. Flip a binary operator deep in the tree.
run("flip + to -", (program) => {
  const walk = (n: any) => {
    if (!n || typeof n !== "object") return;
    if (n.type === "BinaryExpression" && n.operator === "+") n.operator = "-";
    for (const k of Object.keys(n)) {
      const v = n[k];
      if (Array.isArray(v)) v.forEach(walk);
      else if (v && typeof v === "object") walk(v);
    }
  };
  walk(program);
});

// 3. Replace a numeric literal value (literals are leaves).
run("multiply array literals by 10", (program) => {
  const walk = (n: any) => {
    if (!n || typeof n !== "object") return;
    if (n.type === "Literal" && typeof n.value === "number") {
      n.value = n.value * 10;
      n.raw = String(n.value);
    }
    for (const k of Object.keys(n)) {
      const v = n[k];
      if (Array.isArray(v)) v.forEach(walk);
      else if (v && typeof v === "object") walk(v);
    }
  };
  walk(program);
});

// 4. Insert a statement at the top of the program body (array push).
run("insert console.log at top", (program) => {
  const newStmt = {
    type: "ExpressionStatement",
    start: 0,
    end: 0,
    expression: {
      type: "CallExpression",
      start: 0,
      end: 0,
      optional: false,
      callee: {
        type: "MemberExpression",
        start: 0,
        end: 0,
        computed: false,
        optional: false,
        object: { type: "Identifier", start: 0, end: 0, name: "console" },
        property: { type: "Identifier", start: 0, end: 0, name: "log" },
      },
      arguments: [
        { type: "Literal", start: 0, end: 0, value: "loaded", raw: '"loaded"' },
      ],
    },
  };
  program.body.unshift(newStmt);
});

// 5. Replace a node entirely. Wrap the `if/else` block in a function call.
run("wrap if/else in IIFE", (program) => {
  const ifIdx = program.body.findIndex((n: any) => n.type === "IfStatement");
  const ifStmt = program.body[ifIdx];
  const block = {
    type: "BlockStatement",
    start: 0,
    end: 0,
    body: [ifStmt],
  };
  const iife = {
    type: "ExpressionStatement",
    start: 0,
    end: 0,
    expression: {
      type: "CallExpression",
      start: 0,
      end: 0,
      optional: false,
      arguments: [],
      callee: {
        type: "ParenthesizedExpression",
        start: 0,
        end: 0,
        expression: {
          type: "ArrowFunctionExpression",
          start: 0,
          end: 0,
          async: false,
          expression: false,
          id: null,
          params: [],
          body: block,
        },
      },
    },
  };
  program.body[ifIdx] = iife;
});

// 6. Cache-hit demo. No mutation -> the cached parse buffer is reused.
run("no mutation (cache hit)", () => {});
