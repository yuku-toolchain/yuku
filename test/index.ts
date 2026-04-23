// overload signatures (no declare keyword) → TSDeclareFunction declare:false
function foo(x: number): string;
function foo(x: string): number;
function foo(x: any): any { return x; }

// declare function → TSDeclareFunction declare:true
declare function bar(x: number): string;
declare function bar<T>(x: T): T;

// declare class → ClassDeclaration declare:true
declare class A {
  x: number;
  m(): void;
}

// declare abstract class → ClassDeclaration declare:true abstract:true
declare abstract class B<T> extends C<T> implements I {
  abstract m(): T;
}

// declare namespace
declare namespace NS {
  function inner(): void;
}

// declare module
declare module M {
  const x: number;
}

declare module "ext-mod" {
  export const x: number;
}

// declare global
declare global {
  interface Window {
    foo: number;
  }
}

declare var v1: number;
declare let v2: string;
declare const v3: boolean;
declare const v4: number, v5: string;

declare interface I1 {
  x: number;
}

declare type T1 = number | string;

declare enum E1 { A, B = 2 }
declare const enum E2 { X, Y }

// export default function overload
export default function f();
export default function f(x: string);
export default function f(...args: any[]) {}
