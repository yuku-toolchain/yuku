const ctor = <new () => C>value;
const ctorParams = <new (name: string) => C>value;
const ctorGeneric = <new <T>(value: T) => C>value;
const abstractCtor = <abstract new () => C>value;
const fn = <() => void>value;
const fnParams = <(name: string) => void>value;
const keyword = <string>value;
const operator = <keyof C>value;
const array = <C[]>value;
const parenthesized = <(C)>value;
const assertedCall = <new () => C>(value);
const assertedParens = <T>(value);

const arrow = <T>(value: T) => value;
const arrowTrailingComma = <T,>(value: T) => value;
const arrowConstrained = <T extends object>(value: T) => value;
const arrowConst = <const T>(value: T) => value;
const arrowAsync = async <T>(value: T) => value;
const arrowReturnType = <T>(value: T): T => value;

declare const value: unknown;
declare class C {}
declare type T = unknown;

async (result = (input = await / divisor / scale): T => input);
async (result = <U>(input = await / divisor / scale): U => input);
