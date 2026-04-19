function isString(x: unknown): x is string {
  return typeof x === "string";
}

function assertString(x: unknown): asserts x is string {
  if (typeof x !== "string") throw new TypeError();
}

function assert(x: unknown): asserts x {
  if (!x) throw new Error();
}

function isStr(string: unknown): string is string {
  return typeof string === "string";
}

let justAsserts: asserts;
