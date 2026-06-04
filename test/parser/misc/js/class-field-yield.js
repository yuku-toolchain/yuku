function* g() {
  class C {
    x = yield;
    [yield]() {}
  }
}
class D {
  y = yield + 1;
}
