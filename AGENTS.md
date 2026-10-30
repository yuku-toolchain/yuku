# Yuku

How this repository works. The runtime it ships to, where its tests and benchmarks go, how a change lands, and how a release ships.

### Testing

Test at the layer that changed, in the form that layer already uses. Every layer has a home for new cases, so a fix almost never needs a new test file. A new file is for a new capability or topic, never for a single bug.

#### Where Tests Go

| What changed | Where the test goes |
| --- | --- |
| What the parser accepts or rejects, AST shape, diagnostic messages | A fixture in `test/parser/misc/` |
| The token API, or diagnostic spans across many one-line inputs | `test/parser/tokens.test.ts`, `test/parser/diagnostics.test.ts` |
| How the package loads and calls the Node-API addon or WebAssembly | `test/parser/native.test.ts`, `test/parser/wasm.test.ts` |
| Codegen output | An inline snapshot in `test/codegen/`: `print` (default and compact), `strip`, `minify`, `comments`, `quotes`, or `generate` for option composition and source map output |
| Scopes, bindings, references, imports, exports | An inline snapshot of `summary()` in `test/analyzer/<topic>.test.ts`, or `links()` for cross-file behavior |
| Renames | `test/analyzer/rename.test.ts` |
| Resolution as tsc or scope-manager defines it | A snippet in `test/analyzer/tsc-differential.test.ts` or `scope-manager-differential.test.ts` |
| A real codebase to analyze | An entry in `test/projects/manifest.ts`, pinned to a commit |
| Walking, builders, and utils | `test/ast/` |
| Zig internals the JS API cannot reach (traverser, scopes, walk order, allocation failure) | `src/parser/testing/cases/` |
| A small pure Zig helper | An inline `test` block next to the function |

### Documentation

- **One home.** [yuku.fyi](https://yuku.fyi), from `docs/content/`, documents the JavaScript package and the Zig API, and each page links its counterpart in the other. The package's README, `npm/yuku/README.md`, is its front door on npm and GitHub: what it is, one example, and links to the guide.
- **Docs change with the API.** A change to a public API updates its guide in the same change, then `bun run docs:llms` regenerates `llms.txt` and `llms-full.txt` from the site.
- **Every example runs.** An example that states what it returns returns exactly that.

### Changes

- **One change, one purpose.** The tree builds and every suite passes at every commit, and callers update in the same change as the API they use.
- **Commit subjects are short and surgical.** Name exactly what changed, in as few words as that takes, lowercase and imperative. Aim for under fifty characters, never over seventy-two. No explanation, no list of every detail, no area prefix, no conventional type or scope.
- **The subject says what, the body says why.** Write a body only when the subject cannot say why, in a few plain sentences wrapped at seventy-two columns. A breaking change always has one, saying what breaks and what to do instead. A pull-request description is not stored in the repository and is invisible in `git blame`, so it is not a replacement for a commit message.
- **A pull-request title is a commit subject.** Squash-merging makes it one, so it follows the same rules. A pull request with several commits still gets one short title for what they do together, never their subjects joined with "and".
- **No attribution trailers, no tool links.** Not in commits, not in pull requests.
- **Delete, do not deprecate.** Unused code and dependencies are removed. Removing a public export is a breaking change.

### Releases

- **Release notes are written by hand** in `CHANGELOG.md`, one `## <version>` section per release, newest first. Nothing generates them, so they say what a user needs to know and nothing else.
- **Write them from the log, at release time.** `git log v<previous>..HEAD --format=%s` lists every subject since the last release. Keep the changes a user can observe, and leave out tests, CI, docs, and refactors. The log is where the notes start, never what they say.
- **Written for people, not copied from commits.** A line tells a user what they will notice, in plain words and sentence case, the way a person would describe it to another. Never a commit subject, never an area prefix. Commits behind one visible change share a line, which ends with its pull requests and authors when it has them: `- TypeScript declarations can now break across lines wherever TypeScript allows it (#218 by @author)`.
- **Short, never an essay.** A line names the change, and a breaking one says what to do instead. Context, rationale, and internals belong in the commit.
- **Breaking changes come first.** When a release has any, they go under `### Breaking`, each saying what a user must change, and the rest under `### Changes`.
- **Commit the notes, then bump.** `bun run release:npm <version>` commits only the new version, then tags and pushes. The publish workflow stops before building when `CHANGELOG.md` has no section for the tag, and posts that section as the GitHub release once the package is on npm.

---

These rules will feel like a seat-belt at first, a little uncomfortable. After a while, using them becomes second nature, and not using them becomes unimaginable.
