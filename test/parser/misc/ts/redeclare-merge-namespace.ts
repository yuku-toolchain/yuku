// Valid: namespace + namespace merge.
namespace N1 {
  export const a = 1;
}
namespace N1 {
  export const b = 2;
}

// Valid: namespace + class merge (instantiated namespace augments class).
class N2 {
  x: number;
}
namespace N2 {
  export const helper = 1;
}

// Valid: namespace + function merge.
function N3() {}
namespace N3 {
  export const meta = "fn";
}

// Valid: namespace + enum merge.
enum N4 {
  A,
}
namespace N4 {
  export const description = "e";
}
