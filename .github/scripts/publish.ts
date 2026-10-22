import { join } from "node:path";
import { $ } from "bun";

const directory = join(import.meta.dir, "..", "..", "packages", "yuku");

const PLATFORMS = [
  "darwin-arm64",
  "darwin-x64",
  "linux-arm64",
  "linux-x64",
  "win32-arm64",
  "win32-x64",
];
const builds = ["yuku.wasm", ...PLATFORMS.map((platform) => `native/${platform}.node`)];
const missing = builds.filter((build) => Bun.file(join(directory, "dist", build)).size === 0);
if (missing.length > 0) {
  console.error(`packages/yuku/dist is missing ${missing.join(", ")}`);
  console.error("run `zig build native` and `bun run build` first");
  process.exit(1);
}

const { version } = await Bun.file(join(directory, "package.json")).json();
const tag = /-([A-Za-z]+)/.exec(version)?.[1] ?? "latest";
await $`npm publish --access public --provenance --tag ${tag}`.cwd(directory);
