import type { Node, NodeOfType, NodeType } from "@yuku-toolchain/types";
import { NODE_TYPES } from "./generated.ts";

// distributes over unions, so b.Literal accepts each literal shape
type BuilderInput<T> = T extends Node
  ? Omit<T, "type" | "start" | "end"> & { start?: number; end?: number }
  : never;

type Builders = {
  [K in NodeType]: (fields: BuilderInput<NodeOfType<K>>) => NodeOfType<K>;
};

/**
 * One constructor per node type, its fields derived from the node type
 * itself. `start` and `end` default to 0, which `ctx.replace` treats
 * as span-less and fills from the replaced node.
 *
 * @example
 * b.Identifier({ name: "x" })
 * b.ExpressionStatement({ expression: b.Identifier({ name: "x" }) })
 */
export const b = Object.fromEntries(
  NODE_TYPES.map((type) => [
    type,
    (fields: Record<string, unknown>) => ({ type, start: 0, end: 0, ...fields }),
  ]),
) as unknown as Builders;
