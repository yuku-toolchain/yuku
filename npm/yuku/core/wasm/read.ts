import { createRequire } from "node:module";

export default (): string => {
  const { readFileSync } = createRequire(import.meta.url)("node:fs");
  return readFileSync(new URL("./inline.js", import.meta.url), "utf8").split('"')[1]!;
};
