# Yuku Feedback

Notes from porting `rolldown-plugin-dts` from `@babel/parser` + `@babel/generator`
to `yuku-parser` + `yuku-codegen`. Prioritized by impact (time lost, adoption
blocker).

---

## What's already great

- [x] Parser raw speed (11× steady-state)
- [x] Error tolerance + structured `diagnostics`
- [x] `langFromPath` / `sourceTypeFromPath` helpers
- [x] README has runnable examples

---

## Tier 1 — Highest impact (do these first)

### Codegen overhead

The single thing that determines whether yuku is worth migrating to. End-to-end
my plugin showed no improvement because the codegen tax cancelled the parse
wins.

- [ ] **Profile yuku-codegen end-to-end** on a realistic workload to find out
      whether the cost is NAPI serialization, AST-walking, or formatter logic.
- [ ] **Batch print API**: `print(files: ParseResult[])` returning concatenated
      output + one source map, amortizing NAPI cost per call.
- [ ] **Faster path when `sourceMaps: false`** — skip all source-map machinery.
- [ ] **Investigate a parse-handle API** where the AST lives in Zig memory and
      JS gets a typed proxy. If NAPI serialization is the bottleneck, this
      makes print near-free.

### Comments

The flat `comments[]` + offset-based positioning is spec-pure but brutally
painful when the AST has been mutated. I had to ship two workarounds (N+1
`print()` calls, then synthesize fake start/end values on body nodes).

- [ ] **Add opt-in `leadingComments` / `trailingComments` per node.** Babel's
      API is pragmatic for a reason; pure-ESTree consumers can ignore the
      fields.
- [ ] **`print()` mode where comments attach to nodes** instead of positioning
      by offset.
- [ ] **`attachCommentsToTree(parseResult)` helper** that walks comments + body
      and decorates nodes. Saves every consumer from reinventing a WeakMap.

### `file.comments` is a read-only getter

- [ ] **Make `file.comments` writable.** Currently:
      ```js
      file.comments = []  // TypeError: Cannot set property comments which has only a getter
      file.comments.length = 0  // works, but feels wrong
      ```
      Or document the in-place pattern very prominently at the top of the README.

### `Identifier` discriminated subtypes are user-hostile

The single most TypeScript-painful part of the port. Every other line was a
TS2345 about `BindingIdentifier` not being assignable to `IdentifierReference`
because `decorators: Decorator[]` vs `decorators: []`.

- [ ] **Collapse `Identifier`, `IdentifierName`, `IdentifierReference`,
      `BindingIdentifier`, `LabelIdentifier` into one structural type.** The
      runtime is identical (`{type: 'Identifier', name}`); the union breaks
      every existing AST utility.
- [ ] (Alternative) Keep the categories as a `kind?` discriminator field
      instead of structurally-incompatible fields like `decorators`.

### Node factory / builders

ESTree shapes mandate a lot of required fields that babel was forgiving about.
Forget `optional` on a `CallExpression`, you get a runtime error.

- [ ] **Ship `yuku-types` (or `yuku-builders`)** with factories:
      `t.identifier('x')`, `t.stringLiteral('foo')`, `t.callExpression(callee, args)`, etc.
- [ ] Even a partial set covering Literal / Identifier / CallExpression /
      MemberExpression / VariableDeclaration covers ~80% of usage.

---
 
## Tier 2 — Codegen polish

- [ ] **`quotes: 'preserve'`** option (use the `raw` field) for migrations
      that need to keep input quote style.
- [ ] **Context-aware quote mode** (e.g. single quotes for `import('...')`,
      double for top-level imports — matches babel's default behaviour).
- [ ] **Document `raw` field behaviour.** It's load-bearing for output but
      currently undocumented. If you build a `Literal` without `raw`, behavior
      is uncertain.
- [ ] **`format: 'babel-compat'`** knob for one release as a migration aid —
      yuku's output is ~24% more compact than babel's, which means snapshot
      churn for everyone.
- [ ] **Spec the `comments: 'some'` rules precisely** in the README. Triple-slash
      directives are NOT included — I had to test empirically.
- [ ] **Document comment offset behavior**:
  - `start < 0` is silently dropped
  - `start = 0` may end up inside the first node if it has `start = 0` too
  - Either clamp negatives to "emit at top" or document the rules.
- [ ] **`printNode(node, options)`** that takes a single AST node without the
      full `ParseResult` wrapper. Big ergonomics win for transformers.

---

## Tier 3 — Parser polish

- [ ] **`preserveParens` default should match babel/TS-ESTree** (i.e.,
      `false`). `ParenthesizedExpression` is non-standard and breaks many
      walkers.
- [ ] **`TSImportType.source` vs spec's `argument`** — README claims
      "TS-ESTree-conformant" but uses babel's `source`. Either update the
      parser to match the spec, or list this in the deviations section.
- [ ] **Optional `loc` on nodes**: `parse(src, { withLoc: true })` adds
      `loc.start.line/column` to every node. Saves every porting consumer from
      calling `locOf()` per node.
- [ ] **Add `TSEmptyBodyFunctionExpression` to the `Node` union export.** It's
      a child of `MethodDefinition.value` but missing from `Node`. Real type
      bug.
- [ ] **`strict: true` parse option** that throws on syntax errors. The
      `diagnostics` array is great for tolerant mode, but sometimes you want
      fail-fast.
- [ ] **`hasErrors(result)` / `throwOnError(result)` helpers.**

---

## Tier 4 — Ergonomics

- [ ] **Ship a built-in walker** (sync + async) in `yuku-parser`. Acorn ships
      `acorn-walk`, Babel ships `@babel/traverse`. Making consumers shop for a
      third-party walker hurts adoption.
- [ ] **Source-map utilities**: `concatMaps([map1, map2, ...])` and
      `prependLines(map, n)`. Both are ~30 lines and every codegen user needs
      them (I had to write the latter myself).
- [ ] **`isIdentifierName(s)`, `isReservedWord(s)`** validators exported from
      yuku-parser. Replaces `@babel/helper-validator-identifier`.

---

## Tier 5 — Documentation

- [ ] **"Migrating from @babel/parser" guide** with:
  - Node-shape diff table (StringLiteral → Literal, ObjectProperty → Property,
    CommentLine → Line, etc.)
  - Option mapping (`plugins: [['typescript', { dts: true }]]` → `lang: 'dts'`)
  - The `Identifier` widening pattern
  - Comment-handling patterns
- [ ] **"How comment positioning works"** doc page with three concrete
      examples: top-of-file directives, inline annotations, between
      statements.
- [ ] **"Codegen formatting spec"** — when blank lines are emitted, quote
      conventions, what `comments: 'some'` includes/excludes. A test suite of
      "this input → this output" examples doubles as docs.
- [ ] **Performance notes**: scaling numbers + per-call overhead + the "prefer
      batch for many small inputs" guidance. Sets honest expectations.

---

## Quick wins (low effort, high payoff)

Ranked by bang-for-buck. Most are <1 day of work each.

- [ ] Make `file.comments` writable (or document the in-place pattern)
- [ ] Add `TSEmptyBodyFunctionExpression` to the `Node` union export
- [ ] Ship `yuku-types` builder package (partial set is fine for v1)
- [ ] Add a built-in `walk` to yuku-parser
- [ ] Collapse the four `Identifier*` subtypes into one
- [ ] Write the babel-migration doc
- [ ] `hasErrors(result)` / `throwOnError(result)` helpers

---

## Strategic suggestion

The data says yuku-parser is best-in-class. yuku-codegen is the constraint.
A 10× parser shipped with a 1× codegen is a half-product. A 10× parser shipped
with a 5× codegen and the ergonomics fixes above is the new standard.

Order I'd attack:

1. Profile yuku-codegen on a real workload — the rolldown-plugin-dts plugin
   in this repo would be a fine target (`bench/parse-print.ts` already
   exists).
2. Fix the codegen hot path (whichever of NAPI / walker / formatter is
   dominant).
3. Ship `yuku-types` — single biggest ergonomics gain.
4. Make `Identifier` types sane — single biggest TS DX gain.
5. Babel-migration doc + comment-positioning doc — biggest adoption gain.
