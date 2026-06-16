import { Analyzer } from "yuku-analyzer";

const analyzer = new Analyzer();

const counter = `
  import {step} from "./utils.ts"
  let count = 0;
  export function tick() {
    count += step;
    step;
    return () => count;
  }
`;

const utils = `const step = 2; export { step }`;

analyzer.addFile("utils.ts", utils);
analyzer.addFile("counter.ts", counter);
