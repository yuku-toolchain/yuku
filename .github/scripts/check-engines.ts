import { readFileSync } from "node:fs";
import { join } from "node:path";
import { Glob } from "bun";

interface PackageManifest {
  name: string;
  main?: string;
  engines?: { node?: string };
}

const root = join(import.meta.dir, "..", "..");

const readJson = (path: string): PackageManifest => JSON.parse(readFileSync(path, "utf8"));

const floor = readJson(join(root, "package.json")).engines?.node;
if (floor === undefined) {
  console.error("the root package.json declares no engines.node floor");
  process.exit(1);
}

// the WebAssembly build uses WebAssembly SIMD and reference types, which raise its own floor
const exceptions: Record<string, string> = { "@yuku-core/wasm": ">=18.0.0" };

const mismatched: string[] = [];

for (const manifestPath of new Glob("npm/*/package.json").scanSync(root)) {
  const manifest = readJson(join(root, manifestPath));
  if (manifest.main === undefined) continue;
  const expected = exceptions[manifest.name] ?? floor;
  const declared = manifest.engines?.node;
  if (declared !== expected) {
    mismatched.push(
      `${manifest.name}: declares ${declared ?? "no engines.node"}, expected ${expected}`,
    );
  }
}

if (mismatched.length > 0) {
  console.error("packages disagree with the runtime floor in the root package.json:\n");
  for (const line of mismatched) console.error(`  ${line}`);
  process.exit(1);
}

console.log(`every package declares the runtime floor, ${floor}`);
