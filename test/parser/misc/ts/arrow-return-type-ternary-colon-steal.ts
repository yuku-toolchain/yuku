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

const inArray = flag ? (result = [(x): T => x]) : other => other;
const inCall = flag ? (result = f((x): T => x)) : other => other;
const inMember = flag ? (result = a[(x): T => x]) : other => other;
const inProperty = flag ? (result = { a: (x): T => x }) : other => other;
const inTemplate = flag ? (result = `${(x): T => x}`) : other => other;
const inParens = flag ? (result = ((x): T => x)) : other => other;
const inBody = flag ? (result = function () { return (x): T => x; }) : other => other;
const inField = flag ? (result = class { p = (x): T => x; }) : other => other;

const badPattern = flag ? ([(x)]) : other => (y): T => y;
const badPatternTyped = flag ? ([(x): T => x]) : other => other;
const definiteBody = flag ? () => inner ? ((x): T => x) : y : z;
const definiteAsyncBody = flag ? (x) : y => inner ? z : async (): T => w;
