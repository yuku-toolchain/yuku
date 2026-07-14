// sloppy mode keeps `let` an identifier wherever no binding can follow,
// in both statement and for-loop-head positions
var let = 10;
let = 30;
let.foo;
let++;
let in {};
let
next;
for (let; let < 5; let++) {}
for (let = 0; ; ) break;
for (let in {}) {}
for (let of = 1; ; ) break;
