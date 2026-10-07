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

// let across namespace bodies
namespace D5 {
  export let a = 1;
}
namespace D5 {
  export let a = 2;
}

// var + function across namespace bodies
namespace D6 {
  export var a = 1;
}
namespace D6 {
  export function a() {}
}

// class across declare namespace bodies (ambient members are exported)
declare namespace D7 {
  class a {}
}
declare namespace D7 {
  class a {}
}
