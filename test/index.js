// Should error: non-simple params
(async function f(a,...rest) { "use strict"; });

// Should NOT error: simple params
function g(a, b) { "use strict"; }

// Should error: destructuring
function h({x}) { "use strict"; }

// Should error: arrow with block body (not realistic but test)
// Arrow bodies don't have directives in practice, skip this
