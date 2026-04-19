let f1: () => void;
let f2: (x: number) => string;
let f3: (x: number, y: string) => boolean;
let f4: <T>(x: T) => T;
let f5: <T extends string, U>(x: T, y: U) => [T, U];
let f6: (...args: number[]) => void;
let f8: (a: number, ...rest: string[]) => void;
let f9: (a) => a;
let f10: () => number | string;
let f11: () => (x: number) => string;
let f12: Array<(n: number) => boolean>;

let c1: new () => Foo;
let c2: new (x: number) => Foo;
let c3: new <T>(x: T) => T;
let c4: abstract new () => Foo;
let c5: abstract new <T>(...args: T[]) => T;

let paren: (() => void) | null;
let arr: ((x: number) => string)[];

let cond: T extends () => infer R ? R : never;
let condext: T extends (x: U) => V ? true : false;
let fnret: T extends string ? () => number : () => string;
