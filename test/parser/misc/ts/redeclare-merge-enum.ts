// Valid: regular enums with the same name merge.
enum E1 {
  A,
}
enum E1 {
  B = 10,
}

// Valid: const enums with the same name merge.
const enum E2 {
  X,
}
const enum E2 {
  Y = 5,
}
