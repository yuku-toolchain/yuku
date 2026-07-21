import { readFileSync } from "node:fs";
import { join } from "node:path";

interface WorkspaceEntry {
  name?: string;
  version?: string;
}

interface BunLock {
  workspaces: Record<string, WorkspaceEntry>;
}

interface PackageManifest {
  name: string;
  version: string;
}

const root = join(import.meta.dir, "..", "..");

// bun.lock is JSONC, the only non-JSON syntax it uses is trailing commas.
const parseJsonc = <T>(text: string): T => JSON.parse(text.replace(/,(\s*[}\]])/g, "$1")) as T;

const readJson = <T>(path: string): T => JSON.parse(readFileSync(path, "utf8")) as T;

const lock = parseJsonc<BunLock>(readFileSync(join(root, "bun.lock"), "utf8"));

const stale: string[] = [];

for (const [dir, entry] of Object.entries(lock.workspaces)) {
  if (dir === "" || !entry.version) continue;
  const pkg = readJson<PackageManifest>(join(root, dir, "package.json"));
  if (entry.version !== pkg.version) {
    stale.push(`${pkg.name}: bun.lock has ${entry.version}, ${dir}/package.json has ${pkg.version}`);
  }
}

if (stale.length > 0) {
  console.error("bun.lock is out of sync with workspace package versions:\n");
  for (const line of stale) console.error(`  ${line}`);
  console.error(
    "\n`bun pm pack` resolves workspace deps from bun.lock (oven-sh/bun#20477)," +
      "\nso publishing with a stale lockfile pins dependencies to the previous release." +
      "\nFix: rm bun.lock && bun install, then commit the regenerated lockfile.",
  );
  process.exit(1);
}

console.log(
  `bun.lock workspace versions match package.json (${Object.keys(lock.workspaces).length - 1} packages)`,
);
