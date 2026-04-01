---
title: AST Reference
description: Complete AST node reference for Yuku's JavaScript and TypeScript parser.
---

Internally, Yuku uses an optimized AST designed for performance in Zig. When serialized to JSON or exposed through Node.js bindings, this internal AST is converted to an [ESTree](https://github.com/estree/estree)-compatible format, matching the output of [Oxc](https://oxc.rs):

- **JavaScript / JSX**: Fully conformant with the [ESTree](https://github.com/estree/estree) standard, identical to the AST produced by [Acorn](https://www.npmjs.com/package/acorn).
- **TypeScript**: Conforms to the [TS-ESTree](https://www.npmjs.com/package/@typescript-eslint/typescript-estree) format used by `@typescript-eslint`.

The only extensions beyond the base specs are support for Stage 3 [decorators](https://github.com/tc39/proposal-decorators), [import defer](https://github.com/tc39/proposal-defer-import-eval), [import source](https://github.com/tc39/proposal-source-phase-imports), and a non-standard `hashbang` field on `Program`.

This page covers the structure of the internal Zig AST, all node types, and how to work with them.
