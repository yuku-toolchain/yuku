<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="docs/assets/logo.svg" alt="Logo" width="300" />
  
  <br>
  <br>

[![NPM Version](https://img.shields.io/npm/v/yuku-parser?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku-parser)
[![NPM Downloads](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Farshad-yaseen%2Fstatic%2Fmain%2Fbadges%2Fyuku-downloads.json&logo=npm&logoColor=212121&labelColor=ffc44e&color=212121)](https://npmtrends.com/yuku-analyzer-vs-yuku-codegen-vs-yuku-parser)
[![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

Yuku is a high-performance JavaScript and TypeScript compiler toolchain written in Zig. Spec-compliant, zero dependencies, fast by design.

[Try it in the playground →](https://playground.yuku.fyi)

</div>

## Documentation

Visit [yuku.fyi](https://yuku.fyi) for the documentation.

## Packages

| Package                                     | For                                              |
| ------------------------------------------- | ------------------------------------------------ |
| [`yuku-parser`](npm/yuku-parser)            | Parsing to an ESTree / TypeScript-ESTree AST     |
| [`yuku-analyzer`](npm/yuku-analyzer)        | Scopes, bindings, references, and module linking |
| [`yuku-codegen`](npm/yuku-codegen)          | Printing an AST back to source, with source maps |

Each package documents its API in its README. From Zig, add the `parser` module as described in the [documentation](https://yuku.fyi/#zig).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for setup, testing, and playground instructions. Report security vulnerabilities privately, as described in [SECURITY.md](SECURITY.md).

## License

Yuku is free and open-source software licensed under the [MIT License](LICENSE).
