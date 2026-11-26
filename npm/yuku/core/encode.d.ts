import type { Node } from "../types";

export interface Encoded {
  nodes: Int32Array;
  extras: Int32Array;
  comments: Int32Array;
  pool: Uint16Array;
  objects: (Node | undefined)[] | null;
}

export declare function encode(root: Node, ts: boolean, keep: boolean): Encoded;
