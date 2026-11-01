import { createRequire } from "node:module";

export default (): string => createRequire(import.meta.url)("./inline.cjs");
