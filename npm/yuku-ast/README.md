# yuku-ast

Walk, build, and check any ESTree / TypeScript-ESTree AST, with typed visitors, builders, and guards. Part of [Yuku](https://yuku.fyi).

```bash
npm install yuku-ast
```

```js
import { walk } from "yuku-ast";

walk(program, {
  Identifier(node) {
    console.log(node.name);
  },
});
```

See [yuku.fyi/parser/traverse](https://yuku.fyi/parser/traverse/#javascript) and [yuku.fyi/parser/ast](https://yuku.fyi/parser/ast/#javascript) for documentation.

## License

MIT
