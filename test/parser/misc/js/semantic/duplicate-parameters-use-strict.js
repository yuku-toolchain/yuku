function declaration(a, a) { "use strict"; }
function* generatorDeclaration(a, a) { "use strict"; }
async function asyncDeclaration(a, a) { "use strict"; }
async function* asyncGeneratorDeclaration(a, a) { "use strict"; }

(function (a, a) { "use strict"; });
(function* (a, a) { "use strict"; });
(async function (a, a) { "use strict"; });
(async function* (a, a) { "use strict"; });

function outer() {
    "use strict";
    function declaration(a, a) {}
    function* generatorDeclaration(a, a) {}
    async function asyncDeclaration(a, a) {}
    async function* asyncGeneratorDeclaration(a, a) {}
}
