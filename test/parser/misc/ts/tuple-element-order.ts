type Valid = [x: A, y?: B, z?: C];
type ValidUnnamed = [A, B?, C?];
type ValidRestAfterOptional = [x?: A, ...rest: B[]];
type ValidRestAfterRequired = [...rest: A[], y: B];
type ValidVariadic = [A, ...T, C];
type ValidVariadicAfterOptional = [A?, ...T];
type ValidOptionalAfterRest = [...A[], B?];

type RequiredAfterOptional = [x?: A, y: B];
type RequiredAfterOptionalUnnamed = [A?, B];
type RequiredAfterOptionalTrailing = [A?, B, C];
type RequiredAfterVariadic = [A?, ...T, C];
type RequiredAfterOptionalNested = [[x?: A, y: B]];
type RequiredAfterOptionalInUnion = [A?, B] | [C?, D];

type NamedPrefixes = [value: A.B<C<D>>[K][], optional?: T extends U ? X : Y, ...rest: T[]];
type UnnamedPrefixes = [A.B<C<D>>[K][] & C | D, T extends U ? X : Y, ...Tail[]];
type PostfixPrefixes = [A!, B?];
type KeywordLabels = [string: T, readonly?: U, ...infer: V[]];
type EscapedPrefixes = [\u0054, \u006cabel?: U];
type PrimitiveSlots = [string, number[], undefined?];
type PrimitiveLabels = [string: number, null?: void, ...symbol: bigint[]];
type LeadingSlots = [| A | B, & C & D, (| A | B)[]];
