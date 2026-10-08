// Error: exports of one name that TypeScript rejects.
const v = 1;

// re-export + type re-export
export { E1 } from "./a";
export type { E1 } from "./b";

// var + var
export var E2 = 1;
export var E2 = 2;

// function implementation + function implementation
export function E3() {}
export function E3() {}

// local type exported + exported const
type E4 = 1;
export { E4 };
export const E4 = 1;

// type alias + two values
export type E5 = 1;
export const E5 = 1;
export { v as E5 };

// local interface exported + exported interface
interface E6 {}
export { E6 };
export interface E6 {}

// exported const + export import
namespace N {
  export const a = 1;
}
export const E7 = 1;
export import E7 = N.a;

// default interface + default expression
export default interface D {}
export default 1;
