// basic non-null assertion
const a = value!;

// chained postfix
const b = obj.prop!;
const c = fn()!;
const d = arr[0]!;

// stacked
const e = x!!;

// `!` lifts inside chain expression for `?.`
const f = obj?.prop!;
const g = obj?.[0]!;
const h = obj?.fn()!;

// chain continues after `!` -- one ChainExpression covering the whole thing
const f1 = obj?.x!.y;
const f2 = obj?.x!.y!.z;

// member / call / computed bind tighter on the result of `!`
const i = x!.prop;
const j = x!();
const k = x![0];

// composes with as / satisfies / type assertion
const l = null! as Foo;
const m = value! satisfies Bar;
const n = <Foo>x!;

// before `++`
function f1() { x!++; }

// asi: `x\n!y` is two statements
const x = 1
!true
