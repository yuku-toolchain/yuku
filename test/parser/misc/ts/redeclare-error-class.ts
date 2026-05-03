// Error: class redeclaration cases.

// class + class
class B1 {}
class B1 {}

// class + let
class B2 {}
let B2 = 0;

// let + class
let B3 = 0;
class B3 {}

// class + function (function is value, class is value+type. Both occupy value
// space, no overload merge across kinds.)
class B4 {}
function B4() {}

// class + enum (both occupy value+type)
class B5 {}
enum B5 {
  A,
}
