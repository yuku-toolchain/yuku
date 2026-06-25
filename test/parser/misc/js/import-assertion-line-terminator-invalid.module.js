// assert import assertions cannot cross a line break
import value from "pkg"
assert { type: "json" }

export { named } from "pkg"
assert { type: "json" }
