import type {
  BindingFlagMap,
  ExportKind,
  ImportKind,
  ModuleFlags,
  ScopeKind,
  Space,
} from "../file/semantic";
import type {
  Comment,
  Diagnostic,
  Identifier,
  JSXIdentifier,
  Node,
  Program,
  TokenKindMap,
  TokenList,
} from "../types";

export interface Semantic {
  scope: {
    count: number;
    kind(id: number): ScopeKind;
    strict(id: number): boolean;
    node(id: number): Node;
    nodeIndex(id: number): number;
    parentId(id: number): number | null;
    hoistTargetId(id: number): number;
    nextBodyId(id: number): number | null;
  };
  symbol: {
    count: number;
    name(id: number): string;
    flags(id: number): number;
    scopeId(id: number): number;
    declCount(id: number): number;
    declNode(id: number, index: number): Node;
    declNodeIndex(id: number, index: number): number;
  };
  reference: {
    count: number;
    name(id: number): string;
    scopeId(id: number): number;
    node(id: number): Identifier | JSXIdentifier;
    nodeIndex(id: number): number;
    space(id: number): Space;
    inTypePosition(id: number): boolean;
    isWrite(id: number): boolean;
    symbolId(id: number): number | null;
  };
  import: {
    count: number;
    kind(id: number): ImportKind;
    symbolId(id: number): number | null;
    name(id: number): string;
    specifier(id: number): string;
    typeOnly(id: number): boolean;
    phase(id: number): "source" | "defer" | null;
    node(id: number): Node;
    scopeId(id: number): number | null;
  };
  export: {
    count: number;
    kind(id: number): ExportKind;
    typeOnly(id: number): boolean;
    name(id: number): string;
    fromName(id: number): string;
    specifier(id: number): string;
    symbolId(id: number): number | null;
    node(id: number): Node;
  };
  moduleFlags: ModuleFlags;
  nodeScope(index: number): number;
}

export interface Decoded {
  program(): Program;
  tokens(): TokenList | null;
  comments(): readonly Comment[];
  diagnostics(): Diagnostic[];
  semantic(): Semantic | null;
  nodeOf(index: number): Node;
  indexOf(node: Node): number | undefined;
  /** -1 at the root. */
  parentIndex(index: number): number;
}

/** A tree `encode` wrote, with the nodes it read, by index. */
export interface EncodedNodes {
  nodes: Int32Array;
  extras: Int32Array;
  objects: (Node | undefined)[];
}

export declare function decode(
  buffer: ArrayBuffer,
  source: string,
  path: string | null,
  encoded?: EncodedNodes,
): Decoded;
export declare function decodeDiagnostics(
  buffer: ArrayBuffer,
  offset: number,
  count: number,
  path: string | null,
): Diagnostic[];
export declare const TokenKind: TokenKindMap;
export declare const BindingFlags: BindingFlagMap;
