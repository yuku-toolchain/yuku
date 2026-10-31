type LastFn<Fns> = Fns extends [...any[], infer Last extends () => void] ? Last : never;
type PipeResult<Fns extends ((...args: any[]) => any)[]> =
  Fns extends [...any[], infer Last extends (...args: any[]) => any]
    ? Last
    : never;
type LastReturn<Fns> = ReturnType<Fns extends [...any[], infer Last extends () => void] ? Last : never>;
type FnConstraint<T> = T extends infer F extends () => void ? F : never;
type FnWithParams<T> = T extends infer F extends (value: string) => number ? F : never;
type CtorConstraint<T> = T extends infer C extends new () => object ? C : never;
type AbstractCtorConstraint<T> = T extends infer C extends abstract new () => object ? C : never;
type GenericFnConstraint<T> = T extends infer F extends <U>(value: U) => U ? F : never;
type ParenFnConstraint<T> = T extends infer F extends (() => void) ? F : never;
type FnUnionConstraint<T> = T extends infer F extends (() => void) | null ? F : never;
type FnArrayConstraint<T> = T extends infer F extends (() => void)[] ? F : never;
type PredicateFnConstraint<T> = T extends infer F extends (value: unknown) => value is string ? F : never;

type UnionFnConstraint<T> = T extends infer F extends string | () => void ? F : never;
type UnionCtorConstraint<T> = T extends infer C extends string | new () => object ? C : never;
type UnionGenericFnConstraint<T> = T extends infer F extends string | <U>(value: U) => U ? F : never;

type InObject<T> = T extends { handler: infer F extends () => void } ? F : never;
type InTuple<T> = T extends [infer F extends () => void, string] ? F : never;
type InTypeArgument<T> = T extends Promise<infer F extends () => void> ? F : never;
type InFalseBranch<T> = T extends string ? never : T extends infer F extends () => void ? F : never;
type InReturnType<T> = () => T extends infer F extends () => void ? F : never;
type Chained<T> = T extends infer F extends () => void
  ? F extends infer G extends (value: string) => void
    ? G
    : never
  : never;

type NoConstraint<T> = T extends infer U ? U : never;
type KeywordConstraint<T> = T extends infer U extends string ? U : never;
type UnionConstraint<T> = T extends infer U extends string | number ? U : never;
type ObjectConstraint<T> = T extends infer U extends { length: number } ? U : never;
type ArrayConstraint<T> = T extends infer U extends readonly unknown[] ? U : never;
type KeyofConstraint<T, K> = K extends infer U extends keyof T ? U : never;
type TemplateConstraint<T> = T extends infer U extends `a${string}` ? U : never;

type RewoundConstraint<T> = T extends (infer U extends string ? 1 : 0) ? U : never;
type RewoundBeforeFn<T> = T extends (infer U extends () => void ? 1 : 0) ? U : never;
type ConditionalReturn<T> = T extends infer F extends () => T extends U ? X : Y ? F : never;
type TupleTypeArgument<T> = T extends [infer F extends Box<T extends U ? X : Y>] ? F : never;
