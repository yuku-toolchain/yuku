// declaration spans must include the attributes clause, with and without ASI
import foo from "foo" with {}
import bar from "bar" with { type: "json" }
import baz from "baz" with { type: "json" };
import "side-effect" with {}
export * from "all" with {}
export { a } from "named" with {}
