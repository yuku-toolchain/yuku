async function f() {
  await 0;
  class C {
    static {
      async function g() { await 1; }
      const x = async () => await 2;
    }
  }
}
