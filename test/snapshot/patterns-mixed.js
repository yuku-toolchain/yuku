let [{a}] = x
let {a: [b]} = x
let [{a: b}] = x
let {a: [b, c]} = x
let [{a}, {b}] = x
let {a: [b], c: {d}} = x
let [{a = 1}, b = 2] = x
let {a: [b = 1], c = 2} = x

