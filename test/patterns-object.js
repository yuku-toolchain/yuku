let {a} = x
let {a, b} = x
let {a: b} = x
let {a: b, c: d} = x
let {a, b: c} = x
let {"key": a} = x
let {123: a} = x
let {[expr]: a} = x
let {...rest} = x
let {a, ...rest} = x
let {a: b, ...rest} = x
let {a: {b}} = x
let {a: {b: c}} = x
let {a = 1} = x
let {a: b = 1} = x
let {a = 1, b = 2} = x
let {a: {b = 1}} = x

