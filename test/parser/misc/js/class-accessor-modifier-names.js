class C {
  get async() { return 1; }
  set get(v) {}
  async get() { return 2; }
  get set() { return 3; }
  set async(v) {}
  async set() { return 4; }
  get get() { return 5; }
  set set(v) {}
  async() { return 6; }
  get() { return 7; }
  set(v) {}
  static async *gen() {}
}
