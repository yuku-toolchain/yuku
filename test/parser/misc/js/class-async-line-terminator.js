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
