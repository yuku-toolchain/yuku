import defer * as ns from "./throws_FIXTURE.js";

let err1, err2;
try {
	ns.foo;
} catch (e) {
	err1 = e;
}
assert.deepEqual(
	err1,
	{ someError: "the error from throws_FIXTURE" },
	"Evaluation errors are thrown when evaluating",
);
try {
	ns.foo;
} catch (e) {
	err2 = e;
}
assert.sameValue(
	err1,
	err2,
	"Evaluation errors are thrown for already evaluated modules",
);
