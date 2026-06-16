// TypeScript assertion wrappers as assignment targets.
//
// `a!`, `a as T`, `<T>a`, and `a satisfies T` are valid simple assignment
// targets so long as what they wrap is itself a simple target (an identifier
// or member access). yuku must accept these in assignable position, matching
// tsc and oxc. Regression guard for `grammar.expressionToPattern`, which used
// to reject every bare assertion in pattern position.

// valid: non-null assertion wrapping an identifier or member access
a! = b;
[a!] = xs;
({ p: a! } = o);
o.p! = b;

// valid: type assertions and `satisfies` behave the same way
(a as T) = b;
[a as T] = xs;
(<T>a) = c;
(a satisfies T) = b;

// valid: a parenthesized assertion is equivalent to the bare form
(a!) = b;

// invalid: the assertion wraps something that is not a simple target, so the
// whole expression is still not assignable (oxc rejects this too).
f()! = b;

// invalid: assertions may only appear in assignment targets, never in a
// binding pattern such as a `const` destructuring declaration.
const [d!] = xs;
