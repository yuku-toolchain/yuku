import { readFileSync } from "node:fs";
import { join } from "node:path";

const root = join(import.meta.dir, "..", "..");

const nodeFloor = (path: string): string | undefined =>
  JSON.parse(readFileSync(join(root, path), "utf8")).engines?.node;

const floor = nodeFloor("package.json");
if (floor === undefined) {
  console.error("the root package.json declares no engines.node floor");
  process.exit(1);
}

const declared = nodeFloor("npm/yuku/package.json");
if (declared !== floor) {
  console.error(`npm/yuku declares ${declared ?? "no engines.node"}, expected ${floor}`);
  process.exit(1);
}

console.log(`the package declares the runtime floor, ${floor}`);
