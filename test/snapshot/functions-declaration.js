function foo() {}
function bar(a) {}
function baz(a, b) {}
function qux(a, b, c) {}
function trailing(a, b,) {}
function withRest(...args) {}
function withRestAfter(a, ...args) {}
function withDefault(a = 1) {}
function withDefaults(a = 1, b = 2) {}
function withPattern([a, b]) {}
function withObjectPattern({a, b}) {}
function complex(a, b = 1, ...rest) {}

function*gen() {}
function *gen() {}
function* gen() {}
function * gen() {}

async function*asyncGen() {}
async function *asyncGen() {}
async function* asyncGen() {}
async function * asyncGen() {}
