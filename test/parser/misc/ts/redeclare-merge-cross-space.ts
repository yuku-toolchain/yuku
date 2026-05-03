// Valid: type-only declarations don't conflict with value-only ones.

// type alias (type) + var (value)
type T1 = number;
var t1 = 0;

// interface (type) + const (value)
interface T2 {}
const t2 = 0;

// interface (type) + let (value)
interface T3 {}
let t3 = 0;

// interface (type) + function (value)
interface T4 {}
function t4() {}

// non-instantiated namespace (type+namespace, no value) + var (value)
namespace T5 {
  export type Member = number;
}
var t5 = 0;

// type alias + namespace (type-only namespace)
type T6 = string;
namespace T6 {
  export type Inner = number;
}
