import { Analyzer } from "yuku-analyzer";

const analyzer = new Analyzer();

const source = `
  import {step} from "./utils.ts"
  let count = 0;
  export function tick() {
    count += step;
    return () => count;
  }
`;

const source2 = `const count = 2; export { count }`;

analyzer.addFile("utils.ts", source2);
const module = analyzer.addFile("counter.ts", source);

const [tick] = module.findAll("FunctionDeclaration");

for (const capture of module.capturesOf(tick!)) {
  console.log(capture.symbol.name, capture.isWritten, capture.symbol.definition()?.module.path);
}
