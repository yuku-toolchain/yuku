// plain enum, bare members
enum Color {
  Red,
  Green,
  Blue,
}

// enum with initializers
enum Direction {
  Up = 1,
  Down = 2,
  Left = 3,
  Right = 4,
}

// enum with string initializers
enum Status {
  Active = "active",
  Inactive = "inactive",
}

// enum with string-literal member names
enum AgeGroups {
  "0-17",
  "18-22",
  "23-27",
}

// trailing comma
enum TrailingComma {
  A,
  B,
}

// no trailing comma
enum NoTrailing {
  A,
  B
}

// empty enum
enum Empty {}

// const enum
const enum Flags {
  A = 1,
  B = 2,
  C = A | B,
}

// declare enum
declare enum Ambient {
  X,
  Y,
  Z,
}

// declare const enum
declare const enum AmbientConst {
  One = 1,
  Two = 2,
}

// function with enum inside
function wrap() {
  enum Inner {
    a,
    b = 5,
  }
  return Inner;
}
