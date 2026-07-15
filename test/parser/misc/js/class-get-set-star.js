// get/set before a generator star is a field, never an accessor modifier
class A {
  get
  *a() {}
}

class B {
  set
  *b() {}
}
