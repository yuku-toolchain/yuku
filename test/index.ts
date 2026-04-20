// basic type assertions
const a = <number>value;
const b = <string>value;
const c = <any>{};

// with complex types
const d = <Foo<T>>value;
const e = <A | B>x;
const f = <readonly number[]>list;

// binds as a unary prefix, postfix operators bind to the expression
const g = <Foo>obj.prop;
const h = <Foo>fn();
const i = <Foo>arr[0];

// chained prefix assertions
const j = <A><B>value;

// nested inside other expressions
const k = (<number>x) + 1;
const l = foo(<number>x);
