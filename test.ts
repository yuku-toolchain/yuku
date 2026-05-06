import { strip } from "./yuku-strip";

const result = await strip(`
  import assert from "node:assert/strict";

  // 1. Recursive conditional types — type-level arithmetic via tuple length
  type Tup<N extends number, A extends 0[] = []> = A["length"] extends N ? A : Tup<N, [...A, 0]>;
  type Add<A extends number, B extends number> = [...Tup<A>, ...Tup<B>]["length"] & number;
  const _add: Add<3, 4> = 7;

  // 2. Generic recursive tree + variadic constructor
  type Tree<V> = { v: V; c: Tree<V>[] };
  const node = <V,>(v: V, ...c: Tree<V>[]): Tree<V> => ({ v, c });
  const sum = (n: Tree<number>): number => n.v + n.c.reduce((a, x) => a + sum(x), 0);

  const t = node(1, node(2, node(4), node(5, node(8))), node(3, node(6), node(7, node(9), node(10))));
  assert.strictEqual(sum(t), 55);

  // 3. Triple-nested top-level await with Promise.all
  assert.deepStrictEqual(
    await Promise.all([1, 2, 3].map(async (i) =>
      await Promise.all([1, 2, 3].map(async (j) =>
        await Promise.all([1, 2, 3].map(async (k) => ({ k: i * 100 + j * 10 + k })))
      ))
    )),
    [[[{k:111},{k:112},{k:113}],[{k:121},{k:122},{k:123}],[{k:131},{k:132},{k:133}]],
     [[{k:211},{k:212},{k:213}],[{k:221},{k:222},{k:223}],[{k:231},{k:232},{k:233}]],
     [[{k:311},{k:312},{k:313}],[{k:321},{k:322},{k:323}],[{k:331},{k:332},{k:333}]]]
  );

  // 4. Reduce with comma-operator nested assert as a side effect
  const get = (o: any, p: string): any =>
    p.split(".").reduce((a, k) => (assert(a != null, \`nil@\${k}\`), a[k]), o);

  const o = { a: { b: { c: { d: { e: 42 } } } } } as const;
  assert.strictEqual(get(o, "a.b.c.d.e"), 42);
  assert.throws(() => assert.strictEqual(get(o, "a.b.c.d.e"), 43), /43/);
  assert.throws(() => get(o, "a.b.x.y"), /nil@y/);

  // 5. User-defined assertion function w/ variadic predicates
  function check<T>(x: T, ...fs: ((x: T) => boolean)[]): asserts x is T {
    fs.forEach((f, i) => assert(f(x), \`pred \${i}\`));
  }

  const m = new Map<string, Map<string, number>>([
    ["a", new Map([["x", 1], ["y", 2]])],
    ["b", new Map([["x", 3], ["y", 4]])],
  ]);
  check(m,
    (m) => m.size === 2,
    (m) => [...m.values()].every((v) => v.size === 2),
    (m) => [...m.values()].flatMap((v) => [...v.values()]).reduce((s, n) => s + n, 0) === 10,
  );

  // 6. Generic class w/ chained map (functor-ish)
  class Box<T> {
    constructor(public v: T) {}
    map<U>(f: (x: T) => U): Box<U> { return new Box(f(this.v)); }
  }
  assert.strictEqual(new Box(2).map((x) => x + 1).map((x) => x * x).map((x) => \`\${x}!\`).v, "9!");

  // 7. Tagged-tuple discriminated union w/ nested switch + exhaustiveness
  type E = ["num", number] | ["str", string] | ["pair", E, E];
  const evalE = (e: E): string => {
    switch (e[0]) {
      case "num": return \`\${e[1]}\`;
      case "str": return e[1];
      case "pair": return \`(\${evalE(e[1])},\${evalE(e[2])})\`;
      default: { const _x: never = e; throw _x; }
    }
  };
  assert.strictEqual(evalE(["pair", ["pair", ["num", 1], ["str", "x"]], ["num", 2]]), "((1,x),2)");

  console.log("✓ all passed");
`, {
  format: "compact"
});

console.log(result);
