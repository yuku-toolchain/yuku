// Error: namespace conflicts.

// instantiated namespace + var (instantiated namespace occupies value space)
namespace D1 {
  export const a = 1;
}
var D1 = 0;

// var + instantiated namespace
var D2 = 0;
namespace D2 {
  export const a = 1;
}

// instantiated namespace + let
namespace D3 {
  export const a = 1;
}
let D3 = 0;

// instantiated namespace + const
namespace D4 {
  export const a = 1;
}
const D4 = 0;
