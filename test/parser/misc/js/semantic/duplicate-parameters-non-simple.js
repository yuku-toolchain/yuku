function initializer(a, a, b = 0) {}
function* leadingInitializer(a = 0, a) {}
async function restElement(a, ...a) {}
async function* trailingRest(a, a, ...rest) {}
function objectPattern(a, { a }) {}
function* objectPatternOnly({ a, x: a }) {}

(async function (a, a, b = 0) {});
(async function* (a = 0, a) {});
(function (a, ...a) {});
(function* (a, a, ...rest) {});
(async function (a, { a }) {});
(async function* ({ a, x: a }) {});
