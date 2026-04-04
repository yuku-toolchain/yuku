import { parseSync } from 'oxc-parser';

const source = await Bun.file('cool.js').text();

console.time('parse');
const { program } = parseSync('cool.js', source);
console.timeEnd('parse');
