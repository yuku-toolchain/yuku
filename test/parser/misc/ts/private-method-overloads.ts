class A {
  #f(x: string): number;
  #f(x: number): number;
  #f(x: string | number) {
    return Number(x);
  }
  static #g(x: string): void;
  static #g(x: any) {}
  m() {
    return this.#f("1");
  }
}

class B {
  #f(): void;
  static #f() {}
  #g(): void;
  get #g() {
    return 1;
  }
  #h(): void;
  #h() {}
  #h() {}
}
