// Valid: function overloads at module scope (TS allows; sloppy JS Annex B
// also allows at function/global scope).
function overload(x: number): number;
function overload(x: string): string;
function overload(x: any): any {
  return x;
}

// Valid: overloads inside a function body.
function outer() {
  function inner(x: number): number;
  function inner(x: string): string;
  function inner(x: any): any {
    return x;
  }
  return inner;
}
