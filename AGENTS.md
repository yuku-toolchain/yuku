# AGENTS.md

This document defines the coding style and principles that every contribution to this codebase must follow. It is written for human and AI agents alike. Read it once, then keep it close. The rules below are not suggestions.

## Design Goals

Three goals guide every decision, in order of priority:

1. **Safety**
2. **Performance**
3. **Developer experience**

All three matter. Good style advances all three. Style is more than readability. Readability is table stakes, a means rather than an end. Where understanding is missing, style fills the gap.

## Simplicity and Elegance

Simplicity is not a concession to the other goals. It is the "super idea" that solves multiple constraints simultaneously to achieve elegance.

Simplicity is not the first attempt but the hardest revision. Spend mental energy upfront, proactively. An hour of design saves weeks in production.

> "Simplicity and elegance are unpopular because they require hard work and discipline to achieve." Edsger Dijkstra

## Zero Technical Debt

Do it right the first time. The second time may never come, and steady incremental progress depends on knowing that what shipped is solid.

Do not allow potential latency spikes, exponential-complexity algorithms, or other showstoppers to slip through. When a problem is discovered, solve it. Do not defer.

## Safety

### Control Flow

- Use only very simple, explicit control flow. Avoid recursion where iteration suffices; when recursion is unavoidable, give it a bounded depth and assert that bound. Bounded execution must be guaranteed.
- Use only a minimum of excellent abstractions, and only when they make the best sense of the domain. Abstractions are never zero cost, and every abstraction introduces the risk of leaking.
- **Put a limit on everything.** All loops and all queues must have a fixed upper bound to prevent infinite loops or tail-latency spikes. Where a loop cannot terminate (e.g. an event loop), assert this.
- Use explicitly-sized integer types like `u32`. Avoid architecture-specific types like `usize`. The one accepted exception is the seam with the Zig standard library: `std.ArrayList.len`, slice indices into `[]const u8`, and similar interop are typed `usize` by the language. Keep `u32` everywhere we own the type, and limit `usize` to those boundaries (no `@intCast` chains that just propagate the boundary outward).

### Assertions

Assertions detect programmer errors. Unlike operating errors, which are expected and must be handled, assertion failures are unexpected, and crashing is the only correct response. Assertions downgrade catastrophic correctness bugs into liveness bugs. They are a force multiplier for fuzzing.

- **Assert all function arguments, return values, pre/postconditions, and invariants.** A function must not operate blindly on data it has not checked. Assertion density must average at least **two assertions per function**.
- **Pair assertions.** For every property to enforce, find at least two different code paths where an assertion can be added. For example, assert the validity of a node's shape immediately before serializing it, *and* immediately after deserializing it back.
- A blatantly true assertion can be used in place of a comment when the condition is critical and surprising. It is stronger documentation.
- **Split compound assertions:** prefer `assert(a); assert(b);` over `assert(a and b);`. The former reads simpler and gives more precise failure information.
- Use single-line `if` to assert an implication: `if (a) assert(b);`.
- **Assert relationships between compile-time constants.** Compile-time assertions check design integrity *before* the program executes. They are extremely powerful.
- **The golden rule:** assert the **positive space** (what you expect) AND the **negative space** (what you do not expect). The boundary between valid and invalid is where interesting bugs live. Tests must cover both spaces exhaustively, including the transition from valid to invalid.
- Assertions are a safety net, not a substitute for understanding. A fuzzer proves only the presence of bugs, never their absence. Therefore:
  - Build a precise mental model of the code first.
  - Encode that understanding as assertions.
  - Write the code and comments to explain and justify the model to your reviewer.
  - Use fuzzing as the *final* line of defense.

### Memory

- **Allocate with intent, not by reflex.** Prefer arena allocators with a well-defined lifetime over per-object alloc/free. An arena makes the allocation pattern visible at the call site, eliminates use-after-free, and frees in one step.
- **Size buffers ahead of time.** When the upper bound of a collection is known or can be estimated, reserve capacity once instead of growing repeatedly. This keeps the data plane on a predictable hot path.
- **Reuse, don't reallocate.** Scratch buffers used during a single pass should be reset between iterations, not freed and re-grown. Hold them on a parent struct so reuse is the default.
- Long-lived caches and pools must have an explicit upper bound. Unbounded growth is a latency bug waiting to happen.

### Scope and Function Size

- Declare variables at the **smallest possible scope**. **Minimize the number of variables in scope** to reduce the probability of misuse.
- **Hard limit: 70 lines per function.** Art is born of constraints. There are many ways to cut a wall of code into chunks of 70 lines, but only a few will feel right:
  - Good function shape is the inverse of an hourglass: a few parameters, a simple return type, and meaty logic between the braces.
  - **Centralize control flow.** When splitting a large function, keep all `switch`/`if` statements in the parent and move non-branchy logic to helpers. *Push `if`s up and `for`s down.*
  - **Centralize state manipulation.** Let the parent function hold state in local variables and use helpers to compute what should change, rather than mutate directly. Keep leaf functions pure.

### Compiler

- Use the toolchain's **strictest warning setting** from day one. Treat all warnings as errors. This applies equally to Zig, TypeScript, and any other language used in this repository.

### Branches and Conditions

- Compound conditions are hard to verify. Split them into nested `if/else` branches. Split complex `else if` chains into `else { if { } }` trees. This makes branches and cases explicit.
- Consider whether every `if` needs a matching `else` so both positive and negative cases are handled or asserted.
- **State invariants positively.** Negations are not easy. Prefer:

  ```zig
  if (index < length) {
    // The invariant holds.
  } else {
    // The invariant doesn't hold.
  }
  ```

  over the inverted form (`if (index >= length)`).

### Error Handling

- **All errors must be handled.** A majority of catastrophic failures come from incorrect handling of non-fatal errors that were *explicitly* signaled in software. Silently swallowing an error is worse than crashing. Test your error paths.
- Distinguish operating errors (expected, recover and report) from programmer errors (unexpected, assert and crash). Mixing the two confuses callers.

### Motivation and Defaults

- **Always say why.** Every decision should include rationale. Explaining *why* increases the reader's understanding, encourages compliance, and shares criteria for future decisions.
- **Explicitly pass options at the call site** instead of relying on library defaults. Prefer `@prefetch(a, .{ .cache = .data, .rw = .read, .locality = 3 })` over `@prefetch(a, .{})`. This improves readability and prevents latent bugs if the library changes its defaults.

## Performance

> "The lack of back-of-the-envelope performance sketches is the root of all evil."

- **Think about performance from the outset.** The 1000x wins live in the design phase, before you can measure or profile. It is harder to fix performance after implementation, and the gains are smaller. Have mechanical sympathy. Work with the grain.
- **Sketch back-of-the-envelope estimates** for the resources that actually matter to this workload: CPU and memory above all, then disk for reads. Account for both bandwidth and latency. Sketches are cheap. Aim to land within 90% of the global maximum.
- **Optimize the slowest resources first**, but compensate for frequency. A memory cache miss may cost as much as a disk read if it happens many times more.
- **Distinguish the control plane from the data plane.** Configuration, error paths, and one-shot setup are the control plane and can afford to be slow and safe. Per-token, per-node, and per-byte work is the data plane and must be tight. Batching across this boundary delivers high assertion safety without losing performance.
- **Amortize costs by batching** memory, CPU, and I/O access.
- **Let the CPU be a sprinter** doing the 100m. Be predictable. Don't force it to change lanes. Give it large enough chunks of work. This is batching, again.
- **Be explicit. Minimize dependence on the compiler.** Extract hot loops into stand-alone functions with primitive arguments (no `self`). The compiler doesn't need to prove it can cache fields in registers, and a human reader can spot redundant computations more easily.

## Developer Experience

> "There are only two hard things in Computer Science: cache invalidation, naming things, and off-by-one errors."

### Naming

- **Get the nouns and verbs just right.** Great names capture what a thing is or does and reveal that you understand the domain. Take time to find names where the whole exceeds the sum of the parts.
- Use `snake_case` for functions, variables, and files. The underscore is the closest thing programmers have to a space, and it encourages descriptive names.
- **Do not abbreviate** variable names, except primitive integers used in sorts or matrix calculations. Use long-form flags in scripts (`--force`, not `-f`). Single-letter flags are for interactive use.
- Use proper capitalization for acronyms (`ASTNode`, not `AstNode`; `JSXElement`, not `JsxElement`).
- **Add units or qualifiers** to variable names and put them last, sorted by descending significance. Prefer `latency_ms_max` over `max_latency_ms`. This lines up nicely when `latency_ms_min` is added and groups related variables together.
- **Infuse names with meaning.** `allocator: Allocator` is fine, but `gpa: Allocator` and `arena: Allocator` are excellent: they tell the reader whether `deinit` is needed.
- **Match character lengths for related names.** `source` and `target` align better than `src` and `dest`, and downstream variables like `source_offset` and `target_offset` then line up cleanly in calculations.
- When a function calls a helper, **prefix the helper with the caller's name** to show call history: `parse_statement()` and `parse_statement_body()`.
- **Callbacks go last** in parameter lists. This mirrors control flow, since callbacks are also invoked last.
- **Order matters for readability.** Files are read top-down, so put important things first. `main` goes first. For structs: fields, then types, then methods.

  ```zig
  source: []const u8,
  cursor: u32,

  const Token = struct { tag: TokenTag, span: Span };
  const Lexer = @This(); // This alias concludes the types section.

  pub fn init(gpa: std.mem.Allocator, source: []const u8) !Lexer {
      ...
  }
  ```

  If a nested type is complex, promote it to top-level. When in doubt, sort alphabetically. Big-endian naming helps.

- **Don't overload names** with multiple context-dependent meanings.
- **Think about how names will be used outside the code**, in docs, conversation, derived identifiers. A noun usually beats an adjective or present participle. `parser.scratch` reads cleanly as a section heading, while `parser.parsing` needs rephrasing. Nouns also compose more clearly: `config.statements_max`.
- **Use named arguments** (options structs) when arguments can be mixed up. A function taking two `u64`s must use an options struct. If an argument can be `null`, name it so that `null` is meaningful at the call site.
- Singleton dependencies (allocator, logger) have unique types and should be threaded through constructors *positionally*, from most general to most specific.
- **Write descriptive commit messages** that inform and delight the reader. A pull-request description is not stored in the repository and is invisible in `git blame`, so it is not a replacement for a commit message.
- **Say why.** Code alone is not documentation. Comments explain why you wrote what you wrote. Show your workings.
- **Say how.** Tests need a description at the top explaining goal and methodology so readers can get up to speed (or skip past).
- **Comments are sentences:** space after the slash, capital letter, full stop (or colon if introducing what follows). Trailing-line comments may be phrases without punctuation.
- **No em dashes and no semicolons in comments.** Split into separate sentences instead. Both invite run-on prose that obscures the point.

### Cache Invalidation

- **Don't duplicate variables or take aliases** to them. State gets out of sync.
- If a function argument is more than ~16 bytes and shouldn't be copied, pass it as `*const`. This catches bugs where the caller makes an accidental stack copy before the call.
- **Construct large structs in-place** via an *out-pointer* during initialization. This enables pointer stability, allows immovable types, and eliminates intermediate copy-moves that grow the stack.

  In-place initialization is **viral**: if any field is initialized in-place, the container must be too.

  **Prefer:**
  ```zig
  fn init(target: *LargeStruct) !void {
    target.* = .{
      // in-place initialization.
    };
  }

  fn main() !void {
    var target: LargeStruct = undefined;
    try target.init();
  }
  ```

  **Over:**
  ```zig
  fn init() !LargeStruct {
    return LargeStruct {
      // moving the initialized object.
    };
  }

  fn main() !void {
    var target = try LargeStruct.init();
  }
  ```

- **Shrink the scope.** Fewer variables in play means fewer chances to use the wrong one.
- **Calculate or check variables close to where they are used.** Don't introduce variables before they are needed, and don't leave them around after. This avoids POCPOU (place-of-check to place-of-use), a cousin of TOCTOU. Most bugs come from a semantic gap created by distance in time or space.
- **Simpler signatures and return types reduce dimensionality at the call site.** Dimensionality is viral, propagating through the call chain. `void` beats `bool`, `bool` beats `u64`, `u64` beats `?u64`, and `?u64` beats `!u64`.
- **Group resource allocation and deallocation visually** with newlines: blank line before the allocation, blank line after the matching `defer`. Leaks become easier to spot.

### Off-By-One Errors

- The usual suspects are casual interactions between an `index`, a `count`, and a `size`. They are primitive integers but should be treated as distinct types:
  - `index` to `count`: add one (indexes are 0-based, counts are 1-based).
  - `count` to `size`: multiply by the unit.

  This is yet another reason to put units and qualifiers in variable names.

- **Show your intent with division.** Use `@divExact()`, `@divFloor()`, or `div_ceil()` so the reader knows you considered the rounding cases.

### Formatting

- Run the standard formatter (`zig fmt` for Zig sources, the project's configured formatter for TypeScript/JavaScript sources).
- **4 spaces of indentation**, not 2. More obvious at a distance.
- **Hard limit: 100 columns per line.** No exceptions. Nothing should hide behind a horizontal scrollbar. Set a column ruler in your editor. To wrap a signature, call, or data structure, add a trailing comma and let the formatter do the rest.

  The motivation, like the 70-line function limit, is physical: 100 columns lets two copies of the code fit side-by-side on a screen.

- Always brace `if` statements unless they fit on a single line. This is defense in depth against "goto fail;" bugs.

### Dependencies

**Zero dependencies** beyond the language toolchain. Dependencies invite supply-chain attacks, safety risk, performance risk, and slow installs. For foundational code, every cost is amplified throughout the stack above it.

### Tooling

Tools have costs. A small standardized toolbox is simpler to operate than an array of specialized instruments each with its own manual. Invest in your primary toolchain so new problems can be tackled with minimal accidental complexity.

> "The right tool for the job is often the tool you are already using. Adding new tools has a higher cost than many people appreciate." John Carmack

When writing a script, prefer one of the codebase's primary languages (Zig or TypeScript) over shell. This adds type safety, cross-platform portability, and raises the probability that the script works for everyone, not just those on a particular OS or shell.

Standardization reduces dimensionality as the team grows. Slower in the short term, faster in the long term.

---

These rules will feel like a seat-belt at first, a little uncomfortable. After a while, using them becomes second nature, and not using them becomes unimaginable.
