// allowed redeclarations
var x = 1;
var x = 2;

function foo() {}
function foo() {}

var bar = 1;
function bar() {}

// disallowed redeclarations
let a = 1;
let a = 2;

const b = 1;
let b = 2;

let foo = 3;

// scoped — no conflict
let c = 1;
{
    let c = 2;
}

function test(param) {
    let param = 1;
}

// exports
export function exported() {}
export const val = 42;
