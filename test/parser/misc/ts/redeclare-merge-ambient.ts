// Valid: ambient declarations that merge.

// declare class + function
declare class M1 {}
function M1() {}

// function + declare class
function M2() {}
declare class M2 {}

// declare class + declare function
declare class M3 {}
declare function M3(): void;

// declare var + var
declare var M4: number;
var M4: number;

// declare class + interface
declare class M5 {}
interface M5 {}

// declare namespace + namespace
declare namespace M6 {
  const a: 1;
}
namespace M6 {
  export const b = 1;
}
