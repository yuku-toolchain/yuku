# Yuku C Bindings

## Status: Implemented

A C API that exposes Yuku's JavaScript/TypeScript parser to C consumers via a shared library (`libyuku-c`) and an auto-generated header (`yuku-c.h`).

---

## 1. Build

Requires Zig 0.16.0-dev (same as the main project).

```bash
# Generate yuku-c.h (auto-generated from AST definitions)
zig build gen-c-header
# → zig-out/yuku-c.h

# Build shared library
zig build c-lib
# → zig-out/lib/libyuku-c.{dylib,so,dll}

# Optimized build
zig build c-lib -Doptimize=ReleaseFast
```

### Compile & link a C consumer

```bash
clang -o myapp myapp.c -I zig-out -L zig-out/lib -lyuku-c -Wl,-rpath,zig-out/lib
```

### Re-generating the header

Re-run `zig build gen-c-header` whenever the AST definition (`ast.zig`) or binary format (`napi/transfer.zig`) changes upstream. The generated accessors are guaranteed to match the binary format because both the serializer and the generator use the same comptime layout functions.

---

## 2. API

### Functions

```c
#include "yuku-c.h"

YukuResult yuku_parse(const char* source, size_t source_len,
                      YukuParseSourceType source_type, YukuParseLang lang);
void yuku_free(YukuResult* result);
```

### YukuResult

```c
typedef struct {
    uint8_t*    buf;        // binary AST buffer (heap-allocated)
    size_t      size;       // buffer size in bytes
    int         has_errors; // 1 if any error-severity diagnostics
    const char* source;     // saved source pointer for string resolution
} YukuResult;
```

### Options

```c
typedef enum {
    YUKU_SOURCE_SCRIPT = 0,
    YUKU_SOURCE_MODULE = 1,
} YukuParseSourceType;

typedef enum {
    YUKU_LANG_JS  = 0,
    YUKU_LANG_TS  = 1,
    YUKU_LANG_JSX = 2,
    YUKU_LANG_TSX = 3,
    YUKU_LANG_DTS = 4,
} YukuParseLang;
```

### Memory ownership

- `yuku_parse()` allocates the buffer internally. The AST tree is created, serialized, and freed inside `yuku_parse()`. Only the flat buffer survives.
- `yuku_free()` frees the buffer and zeroes the result struct.
- The `source` pointer must outlive the buffer — AST strings reference it by offset. Free source only after `yuku_free()`.

---

## 3. Walking the AST

All accessors are `static inline` — zero FFI overhead after `yuku_parse()`.

### Generic accessors

```c
uint32_t root = yuku_header_program_index(buf);
uint32_t count = yuku_header_node_count(buf);

YukuNodeType type = yuku_node_type(buf, idx);
uint32_t start = yuku_node_span_start(buf, idx);
uint32_t end = yuku_node_span_end(buf, idx);

uint32_t child = yuku_slot(buf, idx, slot);
bool flag = yuku_flag(buf, idx, bit);
uint32_t enum_val = yuku_flag_bits(buf, idx, bit, width);
```

### Per-node macros (generated)

Field names match the original Zig AST exactly. Each macro maps to the correct slot/flag offset computed at comptime.

```c
// BinaryExpression
uint32_t left  = YUKU_binary_expression_left(buf, idx);
uint32_t right = YUKU_binary_expression_right(buf, idx);
uint32_t op    = YUKU_binary_expression_operator(buf, idx);

// IfStatement
uint32_t test       = YUKU_if_statement_test(buf, idx);
uint32_t consequent = YUKU_if_statement_consequent(buf, idx);
uint32_t alternate  = YUKU_if_statement_alternate(buf, idx);

// Function
uint32_t func_type = YUKU_function_type(buf, idx);
uint32_t id        = YUKU_function_id(buf, idx);
bool generator     = YUKU_function_generator(buf, idx);
bool async         = YUKU_function_async(buf, idx);
uint32_t params    = YUKU_function_params(buf, idx);
uint32_t body      = YUKU_function_body(buf, idx);

// IndexRange children
uint32_t body_start = YUKU_program_body_START(buf, idx);
uint32_t body_len   = YUKU_program_body_LEN(buf, idx);
for (uint32_t i = 0; i < body_len; i++) {
    uint32_t child = yuku_extra(buf, body_start, i);
    // walk child...
}

// Null check
bool is_null = yuku_is_null(some_index);
```

### String resolution

`yuku_str_ptr` resolves both source strings and pool strings uniformly:

```c
YukuString name = YUKU_identifier_reference_name(buf, idx);
size_t len;
const char* ptr = yuku_str_ptr(result.source, buf, name, &len);
printf("%.*s\n", (int)len, ptr);
```

- If the string references source text → returns `source + offset`
- If the string is in the pool → returns pointer into the buffer's string pool
- Always returns a valid pointer (never NULL for non-empty strings)

---

## 4. Approach

### Binary Buffer + Auto-Generated Header

```
yuku_parse(source)
  │
  ├─ 1. parser.parse(source) → Tree
  │     Internal: MultiArrayList, arena-allocated. Not walkable from C.
  │
  ├─ 2. Allocate flat output buffer
  │
  ├─ 3. transfer.serializeInto(tree, buffer)
  │     Flattens internal layout into C-walkable binary format.
  │
  ├─ 4. tree.deinit()
  │     Tree freed. It was temporary.
  │
  └─ 5. Return buffer pointer + source pointer to C.
```

The buffer IS the output — no extra copy. The serialization step is the same one used by the Node.js N-API binding (`napi/transfer.zig`).

### Why this over accessor functions

An opaque-handle + FFI-accessor approach was considered but rejected:

| | Binary buffer (chosen) | Accessor functions |
|---|---|---|
| FFI calls | 2 total (`parse` + `free`) | ~25,000 per file |
| Zig code | ~90 lines | ~400 lines |
| Overhead | Zero (inline C access) | ~0.1-0.25ms per file |
| Maintenance | Auto-generated header | Manual accessor wrappers |

---

## 5. Binary Buffer Format

Defined and versioned in `napi/transfer.zig`.

```
Offset   Section        Size
0        Header         40 bytes (fixed)
40       Nodes          node_count × 32 bytes
...      Extra          extra_count × 4 bytes (u32 array)
...      String Pool    string_pool_len bytes (raw UTF-8)
...      Comments       comment_count × 20 bytes
...      Diagnostics    variable length
```

### Header (40 bytes)

| Offset | Size | Field |
|--------|------|-------|
| 0      | 4    | Magic `"YUKU"` |
| 4      | 4    | Version (1) |
| 8      | 4    | node_count |
| 12     | 4    | extra_count |
| 16     | 4    | string_pool_len |
| 20     | 4    | source_len |
| 24     | 4    | comment_count |
| 28     | 4    | diagnostic_count |
| 32     | 4    | program_index |
| 36     | 4    | flags (bit 0: all-ASCII) |

### Node (32 bytes each)

| Offset | Size | Field |
|--------|------|-------|
| 0      | 1    | tag (YukuNodeType) |
| 1      | 1    | flags (bools + enums packed) |
| 2      | 2    | field0 (first IndexRange len) |
| 4      | 4    | field1 (u32 slot 0) |
| 8      | 4    | field2 (u32 slot 1) |
| 12     | 4    | field3 (u32 slot 2) |
| 16     | 4    | field4 (u32 slot 3) |
| 20     | 4    | field5 (u32 slot 4) |
| 24     | 4    | span_start |
| 28     | 4    | span_end |

Field encoding (from `transfer.zig` comptime layout):

- **`bool`** → 1 bit in flags
- **`enum`** → `ceil(log2(N))` bits in flags
- **`NodeIndex`** → 1 u32 slot (`0xFFFFFFFF` = null)
- **`IndexRange`** → first range: len in `field0` + start in 1 slot; subsequent: start + len in 2 slots
- **`String`** → 2 u32 slots (start, end). If `start < source_len` → source text. Otherwise → string pool at offset `(start - source_len)`.
- **`?OptionalEnum`** → 1 flag bit (presence) + enum bits

All integers are little-endian.

---

## 6. Semantic Analysis — Not Needed for Engines

Yuku's semantic analysis (`semantic_checker.zig`) is designed for linter/formatter use cases. For an execution engine, it is unnecessary:

- **Scope/symbol resolution** — the engine's compiler handles this with its own scope model
- **Strict mode** — detectable from raw AST:
  - `source_type == module` → always strict
  - `Directive` nodes with `value == "use strict"` → strict for scripts
- **Early errors** — enforced during compilation, not parsing

---

## 7. File Layout

```
src/parser/
├── capi/                          C API binding
│   └── root.zig                   yuku_parse() / yuku_free() exports
├── napi/                          Node.js N-API binding (existing)
│   ├── root.zig
│   └── transfer.zig               shared binary format serializer
├── ast.zig                        AST definitions (single source of truth)
└── ...

tools/
├── gen_c_header.zig               generates yuku-c.h from AST definitions
└── gen_estree_decoder.zig         existing: generates decode.js

test/c/
└── e2e.c                          end-to-end test (parse → walk → print)
```

`transfer.zig` is shared between `napi/` and `capi/` — both bindings serialize the same binary format.

---

## 8. Naming Convention

All public symbols use `yuku_` prefix:

- Functions: `yuku_parse`, `yuku_free`
- Types: `YukuResult`, `YukuNodeType`, `YukuParseSourceType`
- Macros: `YUKU_binary_expression_left`, `YUKU_function_async`
- Enum values: `YUKU_NODE_binary_expression`, `YUKU_BINOP_plus`

Field names in macros match the original Zig AST field names exactly (snake_case).

---

## 9. Usage Example

```c
#include "yuku-c.h"
#include <stdio.h>
#include <string.h>

void walk(const uint8_t* buf, const char* source, uint32_t idx, int depth) {
    if (yuku_is_null(idx)) return;

    switch (yuku_node_type(buf, idx)) {
    case YUKU_NODE_binary_expression: {
        uint32_t left  = YUKU_binary_expression_left(buf, idx);
        uint32_t right = YUKU_binary_expression_right(buf, idx);
        uint32_t op    = YUKU_binary_expression_operator(buf, idx);
        walk(buf, source, left, depth + 1);
        walk(buf, source, right, depth + 1);
        emit_binary_op(op);
        break;
    }
    case YUKU_NODE_identifier_reference: {
        YukuString name = YUKU_identifier_reference_name(buf, idx);
        size_t len;
        const char* ptr = yuku_str_ptr(source, buf, name, &len);
        printf("%.*s\n", (int)len, ptr);
        break;
    }
    case YUKU_NODE_program: {
        uint32_t start = YUKU_program_body_START(buf, idx);
        uint32_t len   = YUKU_program_body_LEN(buf, idx);
        for (uint32_t i = 0; i < len; i++)
            walk(buf, source, yuku_extra(buf, start, i), depth + 1);
        break;
    }
    // ... all 70+ node types
    default:
        break;
    }
}

int main() {
    const char* source = "const x = 1 + 2;";
    YukuResult result = yuku_parse(source, strlen(source),
                                   YUKU_SOURCE_MODULE, YUKU_LANG_JS);

    if (result.has_errors) { /* check diagnostics */ }
    if (!result.buf) { /* allocation failure */ }

    walk(result.buf, result.source,
         yuku_header_program_index(result.buf), 0);

    yuku_free(&result);
}
```

---

## 10. Testing

```bash
# Build everything
zig build c-lib -Doptimize=ReleaseFast
zig build gen-c-header

# Compile and run the e2e test
clang -o test_e2e test/c/e2e.c -I zig-out -L zig-out/lib \
      -lyuku-c -Wl,-rpath,zig-out/lib
./test_e2e test/parser/misc/js/async-function.js
```

Expected output:

```
parsing: test/parser/misc/js/async-function.js (68 bytes)

buffer size: 404 bytes
nodes: 11
comments: 0
diagnostics: 0

Program [async function testFunction() { ... }]
  Function [...]
    BindingIdentifier [testFunction]
      -> "testFunction"
    FormalParameters [)]
    FunctionBody [{...}]
      ExpressionStatement [console.log('Hello, world!');]
        CallExpression [console.log('Hello, world!')]
          MemberExpression [console.log]
          StringLiteral ['Hello, world!']
            -> "Hello, world!"

OK
```
