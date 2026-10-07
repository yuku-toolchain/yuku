// Error: var and function never merge in TypeScript.

// var + function
var F1 = 0;
function F1() {}

// function + var
function F2() {}
var F2 = 0;

// var + function in a function body
function outer() {
  var F3 = 0;
  function F3() {}
}

// var + function in a namespace
namespace F4 {
  var F5 = 0;
  function F5() {}
}
