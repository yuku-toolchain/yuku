const flag = true;
const value = 1;
let result;

const parenThenArrow = flag ? (value) : (other) => other;
const identTypeThenArrow = flag ? (value) : other => other;
const assignThenArrow = flag ? result = (value) : other => other;
const unaryThenArrow = flag ? !(value) : other => other;
const binaryThenArrow = flag ? 1 + (value) : other => other;
const bodyThenArrow = flag ? (outer) => (outer) : inner => inner;

const kept = flag ? (count): number => count : value;
