import { run } from "./api";
class Store {
  async load(id) {
    return (await run(id));
  }
  *[Symbol.iterator]() {
    for (const [, v] of this.index) yield v;
  }
}
function ensure(xs, is) {
  if (!xs.every(is)) throw new Error();
}
const fns = { ok: (v) => ({ tag: "ok", v }), err: (r) => ({ tag: "err", r }) };
