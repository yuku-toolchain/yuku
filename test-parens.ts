import { parse } from "yuku-parser";

const result = parse("(1 + 2)", { preserveParens: false });
console.log(JSON.stringify(result, null, 2));
