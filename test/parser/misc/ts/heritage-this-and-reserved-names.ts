// a `this` heritage root and reserved type query names parse, the type
// checker rejects them later
interface A extends this.B {}

type T = typeof var.bar;
