import { readFileSync } from "node:fs";
import { join } from "node:path";
import { Glob } from "bun";

interface PackageManifest {
  name: string;
  main: string;
}

const root = join(import.meta.dir, "..", "..");

const missing: string[] = [];
let checked = 0;

for (const manifestPath of new Glob("npm/*/@*/binding-*/package.json").scanSync(root)) {
  const dir = join(root, manifestPath, "..");
  const manifest = JSON.parse(readFileSync(join(dir, "package.json"), "utf8")) as PackageManifest;
  checked += 1;

  const binary = Bun.file(join(dir, manifest.main));
  if (!binary.size) missing.push(`${manifest.name}: ${manifest.main}`);
}

if (!Bun.file(join(root, "npm/yuku-core-wasm/yuku-core.wasm")).size) {
  missing.push("@yuku-core/wasm: yuku-core.wasm");
}

if (checked === 0) {
  console.error("no binding packages found, expected `bun run build:npm` to have run first");
  process.exit(1);
}

if (missing.length > 0) {
  console.error("core packages are missing their binary:\n");
  for (const line of missing) console.error(`  ${line}`);
  console.error(
    "\nThe manifests are committed but the binaries are build output, so a target that failed" +
      "\nto compile packs into a valid package that cannot load." +
      "\nFix: re-run `bun run build:npm` and `bun run build:wasm` and check their output.",
  );
  process.exit(1);
}

console.log(`all ${checked} binding packages and the WebAssembly core carry their binary`);
