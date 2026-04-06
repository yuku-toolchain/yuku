import { parse } from "yuku-parser"

const result = await parse("import defer * as x from 'cool'")

console.log(JSON.stringify(result, null, 2))
