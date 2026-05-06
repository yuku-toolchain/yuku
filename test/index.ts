interface User<T> {
  id: number;
  name?: string;
  greet(): T;
  readonly tag: `${string}-id`;
}

type Maybe<T> = T extends null | undefined ? never : T;
type Pair<A, B = A> = [first: A, second?: B, ...rest: A[]];

declare function f(x: number): string;
declare const config: { port: number };

class Service<T> extends Base<T> implements User<T> {
  static readonly NAME = "svc";
  declare config: { port: number };
  protected override id!: number;
  abstract handle(): T;
  constructor(public readonly token: string) { super(); }
  fn = <U>(x: U): U => x;
}

const v = (x as number) + (y satisfies Z) + obj!;
const f2 = makeBox<number>;
const t: Tools.Pos = { x: 1 } as const;

namespace Foo { export const x = 1; }
declare module "ext" {}
declare global { interface Window {} }

import type { T } from "m";
import { type T2, value } from "n";
export type { Id } from "p";
export = MyLib;
