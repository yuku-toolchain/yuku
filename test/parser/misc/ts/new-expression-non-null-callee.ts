// non-null assertion binds into the new-expression callee chain, so the
// trailing parens are constructor arguments, not a call on the result
new (a?.b)!()
new a!()
new a!.b()

// a line break before ! ends the new expression and ASI splits the statements
new a
!(0)
