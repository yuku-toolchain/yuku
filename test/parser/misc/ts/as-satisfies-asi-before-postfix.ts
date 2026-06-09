// `as`/`satisfies` sit at the relational level, so a `[`, `.`, or call on the
// next line starts a new statement under ASI instead of binding as a member
// access on the cast. these are two statements each, not one member expression.

x as T
[1, 2]

x satisfies T
[3]

// parenthesize the cast to apply member access on its result
const member = (x as T)[0]
