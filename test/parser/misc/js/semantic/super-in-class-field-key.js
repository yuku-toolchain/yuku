class B {}

class A extends B {
  constructor() {
    class C {
      [super()];
      static [super()] = 1;
      accessor [super()] = 1;
      [super.x] = 1;
    }
  }
}

function f() {
  class C {
    [super.x];
  }
}

class D extends B {
  x = super();
  [super.y];
}
