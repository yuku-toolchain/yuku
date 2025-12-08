let {} = x
let {a} = x
let {a, b} = x
let {a, b, c} = x
let {a,} = x
let {a, b,} = x
let {a: b} = x
let {a: b, c: d} = x
let {a: b, c: d, e: f} = x
let {a, b: c} = x
let {a: b, c} = x
let {a, b: c, d} = x
let {"key": a} = x
let {'key': b} = x
let {"a": b, 'c': d} = x
let {123: a} = x
let {1.5: b} = x
let {0: c} = x
let {[expr]: a} = x
let {[a + b]: c} = x
let {["computed"]: d} = x
let {[123]: e} = x
let {[a]: b, c: d} = x
let {a: b, [c]: d} = x
let {[a]: b, [c]: d} = x
let {...rest} = x
let {a, ...rest} = x
let {a, b, ...rest} = x
let {a: b, ...rest} = x
let {a: {}} = x
let {a: {b}} = x
let {a: {b: c}} = x
let {a: {b}, c: {d}} = x
let {a: {b, c}} = x
let {a: {...rest}} = x
let {a: {b: {...rest}}} = x
let {a: []} = x
let {a: [b]} = x
let {a: [b, c]} = x
let {a: [{b}]} = x
let {a: [b], c: [d]} = x
let {a, b: {c}} = x
let {a: {b}, c} = x
let {a: {b: {c}}, d: {e: {f}}} = x
let {a = 1} = x
let {a = 1, b = 2} = x
let {a = 1, b = 2, c = 3} = x
let {a: b = 1} = x
let {a: b = 1, c: d = 2} = x
let {a = 1, b: c = 2} = x
let {a: {b = 1}} = x
let {a: {b: c = 1}} = x
let {a: {b = 1, c = 2}} = x
let {a: [b = 1]} = x
let {a: [b = 1, c = 2]} = x
let {a = 1, b: {c = 2}} = x
