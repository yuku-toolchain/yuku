// All valid: interfaces with the same name merge.
interface I {
  a: number;
}
interface I {
  b: string;
}
interface I {
  c: boolean;
}
