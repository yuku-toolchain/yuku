// Error: ambient declarations conflict like any other.

// declare let + let
declare let A1: number;
let A1 = 0;

// declare const + var
declare const A2: number;
var A2 = 0;

// declare class + class
declare class A3 {}
class A3 {}

// declare function + let
declare function A4(): void;
let A4 = 0;

// declare class + declare class
declare class A5 {}
declare class A5 {}

// declare enum + declare let
declare enum A6 {
  A,
}
declare let A6: number;

// class + declare function (only an ambient class merges with a function)
class A7 {}
declare function A7(): void;
