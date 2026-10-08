// Valid: a type and a value can share an exported name.
const v = 1;
type t = 1;

// exported type + local value exported
export type M1 = { open: boolean };
const M1 = () => null;
export { M1 };

// exported interface + local value exported
export interface M2 {}
const M2 = 1;
export { M2 };

// exported type + re-export
export type M3 = 1;
export { M3 } from "./a";

// empty namespace + alias
export namespace M4 {}
export { v as M4 };

// ambient function + alias of a type
export declare function M5(): void;
export { t as M5 };

// overloads
export function M6(): void;
export function M6() {}

// default expression + default interface
export default 1;
export default interface D {}
