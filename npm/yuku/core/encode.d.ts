import type { Node } from "../types";

/** A tree in the transfer layout, each string a range of UTF-16 units in `pool`. */
export interface Encoded {
  nodes: Int32Array;
  extras: Int32Array;
  comments: Int32Array;
  pool: Uint16Array;
  /** The node at each index, when kept. */
  objects: (Node | undefined)[] | null;
}

export declare function encode(root: Node, ts: boolean, keep: boolean): Encoded;
