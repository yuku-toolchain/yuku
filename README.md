<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="/docs/public/logo.svg" alt="Logo" width="300" />

  [![NPM Version](https://img.shields.io/npm/v/yuku?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku)
  [![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

A high-performance JavaScript/TypeScript compiler written in Zig, featuring a fast parser, visitor/traverser, and transpiler.

</div>

## Features

- **Correctness**: Full ECMAScript spec compliance. Passes all parser tests from [Test262](https://github.com/tc39/test262).
- **Performance**: Exceptionally fast through meticulous performance engineering and data-oriented design, competitive with established parsers like Oxc.
- **Modern**: Supports modern and experimental JavaScript features, including decorators, source and defer imports, and more.
- **Compliance**: Rigorously tested, 100% accurate ESTree + TypeScript-ESTree AST.

## Roadmap

- [x] JavaScript Parser
- [x] WASM (try here: https://yuku-parser.vercel.app/playground)
- [ ] JSX Support (In Progress)
- [ ] TypeScript Support
- [ ] Best-in-class Visitor and Traverser
