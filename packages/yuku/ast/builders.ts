import type { Node, NodeOfType, StandardNodeType } from "../types";
import { NODE_TYPES } from "./generated";

// distributes over unions, so b.Literal accepts each literal shape
type BuilderInput<T> = T extends Node
  ? Omit<T, "type" | "start" | "end"> & { start?: number; end?: number }
  : never;

type Builders = {
  [K in StandardNodeType]: (fields: BuilderInput<NodeOfType<K>>) => NodeOfType<K>;
};

/** A node constructor per `type`, with `start` and `end` defaulting to 0. */
export const b = Object.fromEntries(
  NODE_TYPES.map((type) => [
    type,
    (fields: Record<string, unknown>) => ({ type, start: 0, end: 0, ...fields }),
  ]),
) as unknown as Builders;
