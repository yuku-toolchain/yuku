// Error: type aliases do not merge with anything in the type space.

// type alias + type alias
type A1 = number;
type A1 = string;

// type alias + interface
type A2 = number;
interface A2 {
  x: number;
}

// interface + type alias (reverse order)
interface A3 {
  x: number;
}
type A3 = number;

// type alias + class
type A4 = number;
class A4 {}
