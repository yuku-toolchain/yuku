<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="/assets/logo.svg" alt="Logo" width="300" />

  [![NPM Version](https://img.shields.io/npm/v/yuku?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku)
  [![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

A high-performance JavaScript/TypeScript parser written in Zig, enabling JavaScript tooling within the Zig ecosystem.

</div>

<br/>

- [x] JavaScript Parser
- [x] WASM (try here: https://yuku-parser.vercel.app/playground)
- [ ] JSX Support
- [ ] Best-in-class Visitor and Traverser
- [ ] Semantic Checks (and add extra babel js test fixtures)
- [ ] TypeScript Support
- [ ] Some possible SIMD Optimizations
- [ ] Node (NAPI) Bindings

<br/>

- **Correctness**: Full ECMAScript spec compliance. Passes all parser tests from [Test262](https://github.com/tc39/test262).
- **Performance**: Exceptionally fast through meticulous performance engineering and data-oriented design, competitive with established parsers like Oxc.
- **Modern**: Supports modern and experimental JavaScript features, including decorators, source and defer imports, and more.
- **Compliance**: [Rigorously tested](#how-we-tested-ast-accuracy) for 100% accurate ESTree + TypeScript-ESTree AST compatibility in the Node package, while a performance-focused AST internally (in Zig).

<br/>

### How We Tested AST Accuracy

We ensured that Yuku delivers both correctness and performance. We tested over 3,900+ JavaScript files (marked as `pass` and `pass-explicit`) from the [Test262](https://github.com/tc39/test262) suite. First, we used Oxc to generate the expected ASTs for each file, then parsed the same files with Yuku and compared the ASTs. Yuku passed 100% of the tests.
