import { parse } from "./npm/parser-wasm/dist"

const result = await parse(`
      const nice = 'cool';
      function nice(cool) {
        const cool = 'haha'
      }
  `, {
  semanticErrors: true
})

console.log(result)
