// simple namespace
namespace Foo {
    var x = 1;
    class C {}
}

// dotted namespace
namespace A.B.C {
    export var y = 2;
}

// declare namespace
declare namespace Ambient {
    function f(): void;
}

// string-named module
declare module "./mod" {
    var z: string;
}

// bare-identifier module (deprecated)
module LegacyMod {
    var w = 3;
}

// declare global
declare global {
    interface Window { yuku: number }
}

// nested namespace
namespace Outer {
    namespace Inner {
        export var v = 4;
    }
    declare global {
        interface Array<T> { xyz: T }
    }
}

// empty body
namespace EmptyNs {}

// forward-declared ambient module (no body)
declare module "./forward";
