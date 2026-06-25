// assert after a line break is a statement while with remains an attribute
import value from "pkg"
assert.equal(value, 1)

import data from "./data.json"
with { type: "json" }

export { named } from "pkg"
assert.equal(named, 2)

export * from "pkg"
with { type: "json" }
