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

Yuku is a high-performance JavaScript and TypeScript parser and toolchain written in Zig. Spec-compliant, zero dependencies, fast by design.

</div>

> **Early stage, under active development.** The JavaScript and JSX parser is complete with full spec compliance and thorough testing. A visitor/traverser API and TypeScript support are currently in progress. See the [Roadmap](#roadmap) for details.

## 📖 Documentation

Visit [yuku.fyi](https://yuku.fyi) for the full documentation, guides, and API reference.

## 🤝 Contributing

Yuku is pure Zig with no external dependencies.

```bash
git clone https://github.com/yuku-toolchain/yuku.git
cd yuku
zig build
```

### Testing

Run the full test suite (45,000+ files from Test262 and others) with AST matching:

```bash
bun run test
```

The first run will download the test suite (wait for it to finish). After the run completes, check `test/results.txt` for results.

## Roadmap

- [x] JavaScript Parser
- [x] WASM
- [x] JSX Support
- [ ] TypeScript Support (In Progress)
- [ ] Visitor/Traverser
- [ ] Documentation
- [ ] Module Resolver
- [ ] TypeScript Declaration Transpiler, Minifier, and Bundler
