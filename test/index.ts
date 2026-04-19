// setter with return type in source: previously broke parsing; now
// keeps the annotation on the node, matching TSC/oxc/typescript-eslint.
let s1: { set foo(v: number): void };

// setter without return type: returnType should be null.
let s2: { set bar(v: number) };

// readonly across a newline: `readonly` is the property name, not a modifier.
let r1: {
  readonly
  x: number
};

// readonly as modifier on a property.
let r2: { readonly x: number };

// readonly on same line before `{` (unusual but TSC accepts syntactically).
let r3: { readonly x: number };

// `get` followed by `;` on same line: `get` is the property name.
let g1: { get; set };

// `get` followed by `foo`: accessor.
let g2: { get foo(): number };

// method with generics and `?`.
let m1: { foo?<T>(x: T): T };
