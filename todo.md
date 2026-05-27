Ship (new API) — these remove plugin code

  1. ⭐ Flat, offset-indexed comment list, independent of attachComments. My biggest one. The plugin needs comments two ways: scan the whole
  file for /// <reference> directives (read-only), and attached comments for transforms. Today I parse with attachComments: true and then walk
  the entire tree (collectAllComments) just to re-flatten them. Give me result.comments (flat, cheap) regardless of attachment — ideally
  comments: 'flat' | 'attached' | 'both'. → deletes collectAllComments, lets scan-only paths skip attachment entirely.
  2. ⭐ Export a TopLevelStatement (or ProgramStatement) union = Program['body'][number] (Directive | Statement | ModuleDeclaration). Every
  consumer hits this immediately because Statement excludes import/export. → deletes my type Stmt = Program['body'][number] workaround and makes
   every statement-level signature correct out of the box.
  3. String-level isIdentifierName(name: string): boolean. Babel had it; the node-guard is.IdentifierName can't validate a raw string
  (module-specifier → local-name). → deletes my hand-rolled regex in ast-utils.ts.

  Fix (type/codegen bugs)

  4. ⭐ TSModuleBlock.body is typed Statement[] but namespaces legally contain export { … } (ModuleDeclaration). Forced an as unknown as
  Statement[] cast. Widen to (Statement | ModuleDeclaration)[]. (Audit other block bodies similarly.) → deletes a cast.
  5. Literal discriminant footgun. NumericLiteral/StringLiteral/BooleanLiteral are exported types, but node.type is always "Literal". So
  node.type === 'StringLiteral' compiles yet is always false — a silent, dangerous trap I hit during the port. Either make the runtime type
  carry the variant, or (minimum) add an ESLint-able pattern / make is.* + b.* the clearly-blessed path. Today it only didn't bite me because I
  switched everything to is.StringLiteral / b.stringLiteral.

  Change (codegen behavior) — these remove output churn

  6. ⭐ quotes: 'preserve' option (reuse each node's raw/source quote when present). Babel preserved per-string source quotes; Yuku normalizes

  Document (sharp edges that caused me real confusion)

  8. ⭐ result.program is a lazy getter that decodes the AST on first access. This made me mis-measure parse cost by ~4× (I timed parse() but
  the decode happens on .program). It's a great design for perf, but document it loudly — anyone profiling will get it wrong otherwise. Maybe
  expose result.decode() or a decoded flag so timing is explicit.
  9. walkAsync has per-node await overhead; prefer sync walk. The README hints at it; make it explicit, because the natural port (async
  traversal because one node type needs await) is a real, large perf trap — I had to pre-resolve and go sync to reclaim ~14%.
  10. Document the Babel→TS-ESTree shape deltas consumers will hit: export * as ns is ExportAllDeclaration.exported (not
  ExportNamespaceSpecifier) — this one was a genuine correctness bug in my port; this-type is TSThisType but TSTypeName still includes
  ThisExpression (this.Foo); object props are Property (not ObjectProperty); class members PropertyDefinition/MethodDefinition (not
  ClassProperty/TSDeclareMethod). A "migrating from Babel/@typescript-eslint" table would save every consumer the archaeology.
