// empty type-argument and type-parameter lists are syntax errors. the `<>>`
// form, where the two closing `>` fuse into a single `>>` token, must error the
// same as the spaced `<> >` and the plain `<>` forms.

type A = Array<numbe<>>
type B = Array<numbe<> >
let c: numbe<> = d
function f<>(): void {}
