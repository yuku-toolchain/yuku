<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="/docs/public/logo.svg" alt="Logo" width="300" />

  [![NPM Version](https://img.shields.io/npm/v/yuku?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku)
  [![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

A high-performance JavaScript/TypeScript toolchain written in Zig, bringing modern JavaScript tooling infrastructure to the Zig ecosystem.

</div>

_**Yuku includes:**_

## Parser

- **Spec Compliance**: Fully implements the ECMAScript specification. Passes all parser tests from [Test262](https://github.com/tc39/test262).
- **Exceptional Performance**: Engineered for speed and efficiency using data-oriented design. Competitive with leading parsers like Oxc.
- **Cutting-edge Support**: Parses modern and experimental JavaScript features—including decorators, source and defer imports, and more.
- **Accurate ASTs**: Produces rigorously tested, 100% accurate ESTree and TypeScript-ESTree ASTs.

### Current Status

- [x] JavaScript Parser  
- [x] WASM (Try it: https://yuku-parser.vercel.app/playground)  
- [ ] JSX Support (In Progress)  
- [ ] TypeScript Support  
- [ ] Best-in-class Visitor/Traverser

## TypeScript Declarations Transpiler & Bundler

- [ ] Transpiles and bundles TypeScript declarations (isolated declarations).
- [ ] Handles partially compliant files (not fully isolated declarations) when public APIs are available from typescript-go, enabling fast bundling without strict isolation requirements.
- [ ] Supports declaration splitting for shared types.
- [ ] Built-in minifier: minifies/mangles only internal types, removes internal JSDoc comments, or aggressively minifies when explicitly enabled.
- [ ] Node (NAPI) bindings.
- [ ] Exploratory: Type checking, if allowed by the typescript-go inference API. Potentially eliminates a separate type-checking step. (This is an ambitious idea, feasibility is uncertain, but it’s exciting to consider!)
