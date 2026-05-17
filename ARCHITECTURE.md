# Architecture

Internal design notes for contributors. More sections will be added as
they're written; for now this document covers a single subsystem.

## JS ↔ Zig AST transfer

The native parser produces a Zig `ast.Tree`. JS-side tools need that tree
as an ESTree object, may mutate it, and may want to render it back to
source. The transfer layer is the bridge.

### Pipeline

```
       source (string)
            │
            ▼  binding.parse
       ArrayBuffer  (v4 wire format)
            │
            ▼  decode.js
       ESTree POJO
            │  (each node carries Symbol.for("yuku.bufIdx"))
            │
            ▼  yuku-parser wraps the program in a Proxy
       ESTree program (mutation-tracked)
            │
            │  user traverses and mutates
            ▼
       ESTree program with [DIRTY] flipped along the mutation path
            │
            ▼  yuku-codegen.print / strip / minify
            │
            ├─ clean tree   → cached buffer straight to native
            └─ dirty tree   → encode.js seeds working buffers from cached
                              buffer, dispatcher returns [BUFIDX] for clean
                              subtrees, dirty nodes append at the end
            │
            ▼  binding.{print,strip,minify}
       generated source
```

### Wire format

Spec lives at the top of `src/parser/ffi/transfer.zig` (v4). Briefly: a
fixed `Header` followed by a packed `nodes` array, a flat `extras` u32
array (backing IndexRange children), a string pool, comments, and
diagnostics. Each `PackedNode` is 48 bytes: tag, 16 flag bits, a u16
`field0` (length of the first IndexRange in the node), eight u32 slots,
and span start/end. Slot positions and flag-bit positions for each AST
field are derived at comptime from the `ast.NodeData` union by
`rt.u32SlotForField` and `rt.flagBitForField`.

### Generators

The JS encoder and decoder are not hand-written. They're emitted from
`tools/gen_estree_encoder.zig` and `tools/gen_estree_decoder.zig`, both
of which use the same comptime helpers as `transfer.zig`. This is the
drift safety: the only way for encoder and decoder to disagree about a
slot position is for both helpers to return different values, which they
can't because they share the same code path.

Regenerate after touching `ast.zig`, `transfer.zig`, or either generator:

```bash
bun run build:npm
```

That runs `napi build`, regenerates `decode.js` and `encode.js`, and
copies them into the npm packages.

### Global-symbol contract

Three globally-registered symbols form the contract between layers:

| Symbol | Set by | Read by | Purpose |
| --- | --- | --- | --- |
| `yuku.bufIdx` | decoder | encoder | Parse-buffer index of each decoded node. Lets the encoder short-circuit clean subtrees on diff-encode. |
| `yuku.dirty` | yuku-parser proxy | encoder | `{ self, subtree }` flags flipped on mutation. Absence means "untouched since decode". |
| `yuku.bufRef`, `yuku.source` | yuku-parser | yuku-codegen | The original `ArrayBuffer` and source string on the program root. Drives both cache-hit and diff-encode. |

Because these symbols are `Symbol.for(...)`, any tool can interoperate
with the transfer format without importing types from yuku-parser or
yuku-codegen. The two generated JS files (`decode.js`, `encode.js`) have
no imports and can be dropped into any package that needs them.

### Mutation tracking

`npm/yuku-parser/proxy.js` wraps the program root in a `Proxy`. Children
are wrapped lazily on first access through the proxy. Writes hit the
`set` trap, flip `[DIRTY].self` on the target, and bubble
`[DIRTY].subtree` up through cached `[PARENT]` links until they hit a
node that's already marked.

Untouched trees pay almost nothing: the parser only constructs one
`Proxy` (the root) and two hidden properties. Reads are transparent;
writes are the only thing that materialises further proxies down the
chain.

### Codegen fast paths

1. **Cache hit.** Clean tree → cached buffer + source go straight to
   native. Zero JS-side encode.
2. **Diff encode.** Dirty tree → `encode(estree, srcBuffer)` initialises
   its working buffers by copying the cached parse buffer (three
   memcpys), then the dispatcher returns each clean node's cached
   `[BUFIDX]` instead of walking POJOs. Dirty nodes go through the
   per-tag encoder and append at the end. Output preserves
   `srcLen` so source-byte string refs stay valid against the original
   source string.

### Drift safety

- **Comptime** validation in `transfer.zig` (`validateAllNodeLayouts`)
  rejects any AST struct that overruns the 8-slot / 16-flag-bit packed
  budget.
- **Shared derivation** between encoder and decoder (and the Zig-side
  serializer/deserializer) means slot and bit positions cannot disagree.
- **Codegen conformance test** (`bun run test:codegen`) walks the full
  parser corpus through parse → print → reparse and expects zero
  diagnostics. The matching CI workflow is
  `.github/workflows/codegen-conformance.yml`.

### Files

| Path | Role |
| --- | --- |
| `src/parser/ffi/transfer.zig` | Wire format spec, Zig serializer + deserializer |
| `tools/estree_meta.zig` | Shared metadata between the two generators |
| `tools/gen_estree_decoder.zig` → `decode.js` | Buffer → ESTree POJOs |
| `tools/gen_estree_encoder.zig` → `encode.js` | ESTree POJOs → buffer |
| `npm/yuku-parser/proxy.js` | Lazy mutation-tracking proxy |
| `npm/yuku-parser/index.js` | `parse` (wires the proxy on top of decode) |
| `npm/yuku-codegen/index.js` | `print` / `strip` / `minify` (cache hit + diff encode) |
| `src/parser/ffi/codegen.zig` | Native codegen entry points |
