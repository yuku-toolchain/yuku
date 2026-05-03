// Error: enum redeclaration cases.

// regular enum + const enum (different constness)
enum C1 {
  A,
}
const enum C1 {
  B,
}

// const enum + regular enum (reverse)
const enum C2 {
  A,
}
enum C2 {
  B,
}

// const enum + class (both occupy value+type)
const enum C3 {
  A,
}
class C3 {}

// regular enum + interface (regular enum occupies type space too)
enum C4 {
  A,
}
interface C4 {}
