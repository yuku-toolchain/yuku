// explicit function expression
const a = function(this: void, x: number) { return x };

// method
class C {
  m(this: C, y: number) { return y }
  // abstract / declare / overload form
  abstract g(this: this): void;
}

// interface method
interface I {
  m(this: this): void;
  call(this: void, x: number): string;
}

// function types
type H = (this: Element, e: Event) => void;
type F = new (this: void) => object;

// bare this (no annotation)
function bare(this) {}

// call signature in type literal
type T = { (this: void): number };

// construct signature in type literal
type U = { new (this: void): number };
