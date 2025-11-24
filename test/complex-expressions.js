x = a + b * c - d / e % f
x = 1 + 2 * 3 - 4 / 5 % 6
x = a && b || c && d
x = !a && b || !c
x = ++a + b++ - --c
x = a ** b ** c
x = a << b | c >> d & e >>> f
x = a === b && c !== d || e < f
let [a, {b, c: [d, e]}] = obj
let {a: [b, {c}], d} = obj
const [[a, b], [c, d]] = arr
var {a = 1, b: {c = 2}} = obj
x += y *= z
