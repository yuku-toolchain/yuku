// getters and setters cannot carry an `async` or generator (`*`) modifier.

class C {
  async get a() {}
  async set b(v) {}
  *get c() {}
  static async get d() {}
}
