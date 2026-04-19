// bare mapped type: no modifiers, no as, no annotation omitted
let m0: { [K in keyof T]: T[K] };
// with as clause (nameType)
let m1: { [K in keyof T as `get_${string & K}`]: () => T[K] };
// plain optional
let m2: { [K in keyof T]?: T[K] };
// plus optional
let m3: { [K in keyof T]+?: T[K] };
// minus optional
let m4: { [K in keyof T]-?: T[K] };
// plain readonly
let m5: { readonly [K in keyof T]: T[K] };
// plus readonly
let m6: { +readonly [K in keyof T]: T[K] };
// minus readonly
let m7: { -readonly [K in keyof T]: T[K] };
// readonly and optional combined
let m8: { readonly [K in keyof T]?: T[K] };
// readonly minus, optional plus
let m9: { -readonly [K in keyof T]+?: T[K] };
// omitted value annotation
let m10: { [K in keyof T] };
// constraint that is not keyof
let m11: { [P in K]: P };
// nested mapped types
let m12: { [K in keyof T]: { [J in keyof T[K]]: T[K][J] } };
// mapped type as a member of a union
let m13: { [K in keyof T]: T[K] } | null;
// mapped type under a type operator
let m14: keyof { [K in keyof T]: T[K] };
// not-a-mapped-type: type literal with an index signature
let L0: { [k: string]: number };
// not-a-mapped-type: type literal with a computed member key
let L1: { [expr]: number };
