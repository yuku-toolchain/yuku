// Imports are permissive: a parser cannot tell whether an imported name
// resolves to a value, a type, or both. The original bug case --
// `import { nice } from "cool"; const nice = class implements nice {}`
// must NOT fire a redeclaration error.

import { nice } from "cool";
const nice = class implements nice {
  declare _symbolLinksBrand: any;
};

// import + class with same name: still no error.
import T1 from "./mod";
class T1 {}

// import + const
import T2 from "./mod";
const T2 = 0;

// import + function
import { T3 } from "./mod";
function T3() {}

// import + let (namespace import)
import * as T4 from "./mod";
let T4 = 0;

// import + interface (the import might be a type re-export)
import T5 from "./mod";
interface T5 {
  x: number;
}

// import + type alias
import T6 from "./mod";
type T6 = number;

// Two imports of the same name DO conflict (unambiguous duplicate).
import T7 from "./a";
import T7 from "./b";
