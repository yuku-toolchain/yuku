const BIG_INT_PREFIX = "(BigInt) ";
const REGEXP_PREFIX = "(RegExp) ";

/**
 * Parses AST JSON that Yuku passes from Zig (via estree.toJSON) to JS.
 *
 * BigInt and RegExp values are encoded as tagged strings:
 * - `(BigInt) 10n`
 * - `(RegExp) /pattern/flags`
 */
export function parseYukuForeignAstJson<T = unknown>(jsonString: string): T {
  return JSON.parse(jsonString, (_, value) => {
    if (typeof value !== "string") {
      return value;
    }

    if (value.startsWith(BIG_INT_PREFIX)) {
      return BigInt(value.slice(BIG_INT_PREFIX.length).slice(0, -1));
    }

    if (value.startsWith(REGEXP_PREFIX)) {
      const literal = value.slice(REGEXP_PREFIX.length);

      if (literal.startsWith("/")) {
        const lastSlashIndex = literal.lastIndexOf("/");

        if (lastSlashIndex > 0) {
          const pattern = literal.slice(1, lastSlashIndex);
          const flags = literal.slice(lastSlashIndex + 1);

          return new RegExp(pattern, flags);
        }
      }
    }

    return value;
  }) as T;
}

/**
 * Stringifies AST JSON for transport between JS and Zig while preserving
 * BigInt and RegExp values as tagged strings that can be converted back by
 * parseYukuForeignAstJson.
 */
export function stringifyYukuForeignAstJson(
  obj: unknown,
  space?: string | number,
): string {
  return JSON.stringify(
    obj,
    (_, value) => {
      if (typeof value === "bigint") {
        return `${BIG_INT_PREFIX}${value}n`;
      }

      if (value instanceof RegExp) {
        return `${REGEXP_PREFIX}${value.toString()}`;
      }

      return value;
    },
    space,
  );
}
