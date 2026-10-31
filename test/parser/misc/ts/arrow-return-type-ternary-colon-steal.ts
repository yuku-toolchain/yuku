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

const computedObject = flag ? (result = { [(x): T => x]: 0 }) : other => other;
const computedClass = flag ? (result = class { [(x): T => x] = 0; }) : other => other;
const computedTypeArgument = flag ? (result = f<{ [(x): T => x]: 0 }>()) : other => other;
const computedRelational = flag ? (result = f < { [(x): T => x]: 0 } > value) : other => other;
const computedAsync = flag ? (result = { [async (x): T => x]: 0 }) : other => other;
