({ method(a, a) {} });
({ *generatorMethod(a, a) {} });
({ async asyncMethod(a, a) {} });
({ async *asyncGeneratorMethod(a, a) {} });

(a, a) => {};
async (a, a) => {};

class C {
    constructor(a, a) {}
    method(a, a) {}
    *generatorMethod(a, a) {}
    async asyncMethod(a, a) {}
    async *asyncGeneratorMethod(a, a) {}
}
