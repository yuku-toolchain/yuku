import { readdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { $ } from "bun";

const version = process.argv[2];
if (version === undefined || !/^\d+\.\d+\.\d+(-[0-9A-Za-z.]+)?$/.test(version)) {
  console.error("usage: bun run release:npm <version>, such as 0.20.0 or 0.20.0-beta.1");
  process.exit(1);
}

const root = join(import.meta.dir, "..", "..");
const packages = join(root, "npm");
for (const entry of readdirSync(packages, { withFileTypes: true })) {
  if (!entry.isDirectory()) continue;
  const manifestPath = join(packages, entry.name, "package.json");
  const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
  manifest.version = version;
  for (const field of ["optionalDependencies", "peerDependencies"]) {
    for (const dependency of Object.keys(manifest[field] ?? {})) {
      if (dependency.startsWith("@yuku-toolchain/")) manifest[field][dependency] = version;
    }
  }
  writeFileSync(manifestPath, JSON.stringify(manifest, null, 2) + "\n");
}

$.cwd(root);
await $`bun install`;
await $`git add npm/*/package.json bun.lock`;
await $`git commit --message ${version}`;
await $`git tag --annotate --message ${version} v${version}`;
await $`git push --follow-tags`;
