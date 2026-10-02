# Changelog

What changed in each release of Yuku, newest first. Releases up to 0.14.0 are listed on [GitHub](https://github.com/yuku-toolchain/yuku/releases).

## 0.15.1

### Changes

- analyzer: memoize export resolution for each link
- analyzer: index nodes on the first node query

## 0.15.0

### Breaking

- all: tidy the analyzer API and share file options and diagnostics
  - `addFile` and `removeFile` are `setFile` and `deleteFile`.
  - `Symbol` and `SymbolFlags` are `Binding` and `BindingFlags`, and `symbols`, `symbolOf`, and `.symbol` are `bindings`, `bindingOf`, and `.binding`.
  - `module.resolve(name, scope, space)` is `module.lookup(name, { from, space })`.
  - `analyzer.definitionOf` and `analyzer.referencesOf` are `binding.definition()` and `binding.findReferences()`.
  - The `is*` kind predicates are gone, except `imp.isNamespace`. Compare `kind` instead.
  - A resolver returns `false` for a module outside the project. `null` means unresolved.
  - Link diagnostics have the shared diagnostic shape, with `path` in place of `module`.
  - An unknown `lang` or `sourceType` throws a `TypeError`.

### Changes

- all: resolve names, merges, and exports as tsc does (#223 by @arshad-yaseen)
- analyzer: probe module paths in TypeScript's order (#223 by @arshad-yaseen)
- analyzer: add `module.ancestors` (#223 by @arshad-yaseen)
- all: build decoder caches on demand (#223 by @arshad-yaseen)
- all: add a `path` option to `parse`, and `module.isCurrent`, `module.nodeAt`, and `module.resolveExport`
