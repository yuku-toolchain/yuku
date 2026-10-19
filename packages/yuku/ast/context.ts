import type { Node } from "../types";

/** @internal */
export interface Frame {
  i: number;
}

/** A walk's current position and tree edits, valid only during the visit that receives it. */
export class WalkContext<T extends Node = Node, S = unknown> {
  /** @internal */
  _ancestors: Node[] = [];
  /** @internal */
  _node: Node | null = null;
  /** @internal */
  _key: string | null = null;
  /** @internal */
  _list: Node[] | null = null;
  /** @internal */
  _frame: Frame | null = null;
  /** @internal */
  _skip = false;
  /** @internal */
  _stopped = false;
  /** @internal */
  _removed = false;
  /** @internal */
  _replacement: Node | null = null;
  /** The third `walk` argument. */
  state!: S;

  /** The node being visited. */
  get node(): T {
    return this._node as T;
  }

  /** Null at the walk root. */
  get parent(): Node | null {
    return this._ancestors[this._ancestors.length - 1] ?? null;
  }

  /** The field of {@link parent} holding the node, null at the root. */
  get key(): string | null {
    return this._key;
  }

  /** The index in an array field, null in a plain field. */
  get index(): number | null {
    return this._list === null ? null : this._frame!.i;
  }

  /** From the walk root down to {@link parent}. */
  ancestors(): Node[] {
    return [...this._ancestors];
  }

  /** Skips the current node's children. */
  skip(): void {
    this._skip = true;
  }

  /** Stops the walk. */
  stop(): void {
    this._stopped = true;
  }

  /** Replaces the current node and walks the replacement, which takes the old span if 0 to 0. */
  replace(node: Node): void {
    if (node === null || typeof node !== "object" || typeof node.type !== "string") {
      throw new TypeError("replace: expected an AST node");
    }
    if (this.parent === null) {
      throw new TypeError("replace: cannot replace the walk root");
    }
    if (node.start === 0 && node.end === 0) {
      node.start = this.node.start;
      node.end = this.node.end;
    }
    this._replacement = node;
  }

  /** Removes the current node without walking its children or firing `leave`. */
  remove(): void {
    if (this.parent === null) {
      throw new TypeError("remove: cannot remove the walk root");
    }
    this._removed = true;
  }

  /** Inserts a sibling before the current node, which the walk skips. */
  insertBefore(node: Node): void {
    insertSibling(this, node, 0);
    this._frame!.i++;
  }

  /** Inserts a sibling after the current node, which the walk visits. */
  insertAfter(node: Node): void {
    insertSibling(this, node, 1);
  }
}

function insertSibling(ctx: WalkContext, node: Node, offset: number): void {
  if (ctx._list === null) {
    throw new TypeError("insertBefore/insertAfter require a node in an array field");
  }
  ctx._list.splice(ctx._frame!.i + offset, 0, node);
}
