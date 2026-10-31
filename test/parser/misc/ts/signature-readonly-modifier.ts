interface Valid {
  readonly property: number;
  readonly [key: string]: unknown;
}

interface OnSignatures {
  readonly method();
  readonly generic<T>(value: T): T;
  readonly get getter(): number;
  readonly set setter(value: number);
  readonly new (): Valid;
}

type ValidLiteral = {
  readonly property: number;
  readonly [key: string]: unknown;
};

type LiteralOnSignatures = {
  readonly method(): void;
};

type ObjectPrefixes = { plain: T; readonly: U; [key: string]: V };
type MappedPrefixes<T> = { -readonly [K in keyof T as K]+?: T[K] };
type BareMapped<T> = { [K in T]: V };
