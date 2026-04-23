// 8.4a Export dispatch for TS declarations

// type alias
export type A = number;
// interface
export interface B { x: number; }
// enum
export enum C { A, B }
// const enum
export const enum D { A, B }
// namespace
export namespace N { export const x = 1; }
// module (deprecated namespace form)
export module M { const z = 1; }
// abstract class
export abstract class E { }

// declare variants
export declare var v: number;
export declare let l: number;
export declare const c: number;
export declare function f(): void;
export declare class G { }
export declare abstract class H { }
export declare enum I { A, B }
export declare const enum J { A }
export declare namespace K { }
export declare interface L { }
export declare type T = number;

// nested inside namespace
namespace Outer {
  export interface Inner { x: number; }
  export type AliasInner = number;
  export enum EnumInner { A }
  export class ClassInner { }
  export const vInner = 1;
}
