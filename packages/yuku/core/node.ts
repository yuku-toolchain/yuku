interface NodeBufferConstructor {
  from(
    buffer: ArrayBuffer,
    offset: number,
    length: number,
  ): { write(text: string, encoding: "utf16le"): number };
  from(text: string, encoding: "base64"): Uint8Array<ArrayBuffer>;
}

interface NodeGlobals {
  process?: { versions?: { node?: string } };
  Buffer?: NodeBufferConstructor;
}

export const NodeBuffer: NodeBufferConstructor | undefined =
  (globalThis as NodeGlobals).process?.versions?.node === undefined
    ? undefined
    : (globalThis as NodeGlobals).Buffer;
