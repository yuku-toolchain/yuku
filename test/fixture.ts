class Store<T extends { id: `id_${string}` }> implements Iterable<T> {
  declare readonly index!: ReadonlyMap<T["id"], T>

  async load<R extends T = T>(id: T["id"]): Promise<R | null> {
    return (await run<R>(id))! satisfies R | null
  }

  *[Symbol.iterator](): IterableIterator<T> {
    for (const [, v] of this.index!) yield v
  }
}
