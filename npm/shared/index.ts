const BIG_INT_PREFIX = "(BigInt) ";
const REGEXP_PREFIX = "(RegExp) ";
const REGEXP_LITERAL = /^\/(.+)\/([dgimsuyv]*)$/;

/**
 * Deserializes AST JSON that Yuku passes from Zig (via estree.toJSON) to JS.
 *
 * BigInt and RegExp values are encoded as tagged strings:
 * - `(BigInt) 10n`
 * - `(RegExp) /pattern/flags`
 */
export function deserializeAstJson<T = unknown>(jsonString: string): T {
	return JSON.parse(jsonString, (_, value) => {
		if (
			typeof value === "object" &&
			value !== null &&
			typeof value.value === "bigint"
		) {
			value.bigint = value.value.toString();
			return value;
		}

		if (typeof value !== "string") {
			return value;
		}

		if (value.startsWith(BIG_INT_PREFIX)) {
			return BigInt(
				value
					.slice(BIG_INT_PREFIX.length)
					.replace(/n$/, "")
					.replaceAll("_", ""),
			);
		}

		if (value.startsWith(REGEXP_PREFIX)) {
			const match = value.slice(REGEXP_PREFIX.length).match(REGEXP_LITERAL);

			if (match) {
				try {
					return new RegExp(match[1]!, match[2]);
				} catch {
					return null;
				}
			}

			return null;
		}

		return value;
	}) as T;
}

/**
 * Serializes AST JSON for transport between JS and Zig while preserving
 * BigInt and RegExp values as tagged strings that can be converted back by
 * deserializeAstJson.
 */
export function serializeAstJson(
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
