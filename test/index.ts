// simple interface
interface Foo {
  a: number;
  b: string;
}

// generic interface
interface Bar<T> {
  value: T;
}

// single extends, no type args
interface I2 extends I1 {
  x: number;
}

// multiple extends with type args
interface I3<T> extends A<T>, B<T> {
  c: T;
}

// dotted extends (MemberExpression)
interface I4 extends Ns.Base {}

// deep dotted extends with type args
interface I5<T> extends Outer.Inner.Thing<T> {}

// interface with mixed signatures
interface Signatures {
  prop: number;
  method(): void;
  (x: string): boolean;
  new (x: number): Signatures;
  [key: string]: any;
  readonly ro: number;
  opt?: string;
}

// declare interface
declare interface Ambient {
  x: number;
}

// extends with generics closing with >>
interface Nested<T> extends Outer<Inner<T>> {}
