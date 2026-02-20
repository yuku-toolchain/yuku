<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="/docs/public/logo.svg" alt="Logo" width="300" />
  
  <br>
  <br>

  [![NPM Version](https://img.shields.io/npm/v/yuku?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku)
  [![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

A high-performance JavaScript/TypeScript toolchain written in Zig, bringing modern JavaScript tooling infrastructure to the Zig ecosystem.

</div>

> **Early stage, under active development.** The JavaScript and JSX parser is complete with full spec compliance and thorough testing. A visitor/traverser API and TypeScript support are currently in progress. See the [Roadmap](#roadmap) for details.

## Parser

- **Correctness**: Full ECMAScript spec compliance. Passes all 40000+ files from [Test262](https://github.com/tc39/test262) with deep AST matching. See [test results](/test/results.txt).
- **Performance**: Exceptionally fast through meticulous performance engineering and data-oriented design. Competitive with leading parsers like Oxc. [See benchmarks](https://github.com/yuku-toolchain/parser-benchmark).
- **Modern**: Supports modern and experimental JavaScript features, including decorators, source and defer imports, and more.

### AST

Yuku produces the same AST as [Oxc](https://oxc.rs):

- **JavaScript / JSX** — Fully conformant with the [ESTree](https://github.com/estree/estree) standard, identical to the AST produced by [Acorn](https://www.npmjs.com/package/acorn).
- **TypeScript** — Conforms to the [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) format used by `@typescript-eslint`.

The only extensions beyond the base specs are support for Stage 3 [decorators](https://github.com/tc39/proposal-decorators), [import defer](https://github.com/tc39/proposal-defer-import-eval), [import source](https://github.com/tc39/proposal-source-phase-imports), and a non-standard `hashbang` field on `Program`.

AST accuracy is verified by running Test262, TypeScript, and Babel test suites and performing a deep comparison against the Oxc AST (ESTree/TS-ESTree). See [test results](/test/results.txt).

## Roadmap

- [x] JavaScript Parser
- [x] WASM
- [x] JSX Support
- [ ] Visitor/Traverser (In Progress)
- [ ] TypeScript Support
- [ ] Documentation
- [ ] Module Resolver
- [ ] TypeScript Declaration Transpiler, Minifier, and Bundler
