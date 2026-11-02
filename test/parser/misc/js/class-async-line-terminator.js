// async is [no LineTerminator here] so a line break makes it a field,
// while static, get, and set stay one method across a line break
class A {
  async
  foo() {}
}

class B {
  async
  *gen() {}
}

class C {
  async foo() {}
  static
  bar() {}
  get
  baz() {
    return 1
  }
  set
  qux(value) {}
}

// an escaped word never matches a modifier, so each is a field ended by the line break
class D {
  st\u0061tic
  foo() {}
  g\u0065t
  bar() {}
  s\u0065t
  baz = 1
}
