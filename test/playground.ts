import { parse } from "yuku-parser";

const src = await Bun.file('test/fixture.ts').text();

for (let i = 0; i < 100; i++) {
  const {program} = parse(src, { attachComments: true });
}

console.time('parse')
const {program} = parse(src, { attachComments: true });
console.timeEnd('parse')
