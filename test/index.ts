// the case the bug fix addresses: function type in extends slot with a
// conditional return type. with the old code this failed at `extends`;
// tsc-go (and now us) parse it as:
//   T extends (() => (A extends U ? X : Y)) ? P : Q
let edge1: T extends () => A extends U ? X : Y ? P : Q;

// the simple cases that already worked must still work
let simple1: T extends () => A ? X : Y;
let simple2: T extends (x: U) => V ? true : false;
let simple3: T extends string ? () => number : () => string;
let simple4: (() => void) | null;
let simple5: T extends () => infer R ? R : never;
