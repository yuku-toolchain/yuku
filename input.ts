import { run, type Awaitable, type Result as Res } from "./api"

export type Unwrap<T> = T extends Awaitable<infer U> ? Unwrap<U> : T
export type Tagged<T> = { readonly [K in keyof T as `${string & K}$`]: T[K] }

class Store<T extends { id: `id_${string}` }> implements Iterable<T> {
  declare readonly index!: ReadonlyMap<T["id"], T>

  async load<R extends T = T>(id: T["id"]): Promise<R | null> {
    return (await run<R>(id))! satisfies R | null
  }

  *[Symbol.iterator](): IterableIterator<T> {
    for (const [, v] of this.index!) yield v
  }
}

function ensure<T, U extends T>(
  xs: readonly T[],
  is: (x: T) => x is U,
): asserts xs is readonly U[] {
  if (!xs.every(is)) throw new Error()
}

const fns = {
  ok: <const T>(v: T) => ({ tag: "ok" as const, v }),
  err: (r: string) => ({ tag: "err" as const, r }),
} satisfies Record<string, (...a: never[]) => Res<unknown>>
