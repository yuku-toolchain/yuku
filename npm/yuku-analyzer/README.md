# yuku-analyzer

Semantic analysis for JavaScript and TypeScript: scopes, symbols, resolved references, closures, and cross-file module linking. Part of [Yuku](https://yuku.fyi).

```bash
npm install yuku-analyzer
```

```js
import { analyze } from "yuku-analyzer";

const module = analyze("let count = 0; count++;");
module.rootScope.find("count").references.length; // 1
```

See [yuku.fyi/analyzer](https://yuku.fyi/analyzer/) for documentation.

## License

MIT
