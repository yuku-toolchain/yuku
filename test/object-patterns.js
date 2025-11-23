let {a} = obj
let {a, b} = obj
let {a, b, c} = obj
let {a: x} = obj
let {a: x, b: y} = obj
let {"key": value} = obj
let {[expr]: value} = obj
let {a = 1} = obj
let {a = 1, b = 2} = obj
let {a: x = 1} = obj
let {a: {b}} = obj
let {a: {b: c}} = obj
let {a, b: {c, d}} = obj
let {...rest} = obj
let {a, ...rest} = obj
let {a, b, ...rest} = obj
const {x} = obj
var {y} = obj
let {1: a} = obj
let {0xFF: a} = obj
