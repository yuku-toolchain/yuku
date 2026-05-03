class Foo {
  x: any;
  constructor(x: number);
  constructor(x: string);
  constructor(x: any) {
    this.x = x;
  }
}

class Bar extends Foo {
  constructor(y: number);
  constructor(y: string);
  constructor(y: any) {
    super(y);
  }
}
