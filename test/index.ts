// import equals tests
import fs = require("fs");
import alias = Foo;
import path = Foo.Bar.Baz;
import type T = Foo.Bar;

namespace M {
    import b = a.b;
    export import re = Foo;
}

export import exp = Lib;
