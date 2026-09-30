function declaration(a, a) { return a; }
function* generatorDeclaration(a, a) { return a; }
async function asyncDeclaration(a, a) { return a; }
async function* asyncGeneratorDeclaration(a, a) { return a; }

(function (a, a) { return a; });
(function* (a, a) { return a; });
(async function (a, a) { return a; });
(async function* (a, a) { return a; });

(function expression(a, a, a) { return a; });
(function* generatorExpression(a, a, a) { return a; });
(async function asyncExpression(a, a, a) { return a; });
(async function* asyncGeneratorExpression(a, a, a) { return a; });

function asmDeclaration(a, a) { "use asm"; return a; }
function* asmGeneratorDeclaration(a, a) { "use asm"; return a; }
async function asmAsyncDeclaration(a, a) { "use asm"; return a; }
async function* asmAsyncGeneratorDeclaration(a, a) { "use asm"; return a; }
