// legacy (experimentalDecorators) parameter decorators on binding patterns,
// with type annotations and defaults, so walk-order checks exercise the
// decorators-first field order on ObjectPattern, ArrayPattern, and
// AssignmentPattern
class C {
  constructor(
    @dec { a }: { a: number },
    @dec [b]: string[],
    @dec { c }: { c: boolean } = { c: true },
    @dec [d]: number[] = [0],
    @dec e: string = "x",
  ) {}
}
