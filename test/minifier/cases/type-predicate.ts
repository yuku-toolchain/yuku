function isStringly(value: unknown): value is string {
  return typeof value === "string";
}

export function run() {
  return [isStringly("hi"), isStringly(42), isStringly(null)];
}
