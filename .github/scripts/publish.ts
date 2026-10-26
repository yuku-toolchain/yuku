import { join } from "node:path";
import { $ } from "bun";

const packages = join(import.meta.dir, "..", "..", "npm");

const CORES = [
  "darwin-arm64",
  "darwin-x64",
  "linux-arm64",
  "linux-x64",
  "win32-arm64",
  "win32-x64",
  "wasm",
];
const builds = [
  ...CORES.map((core) => join(core, core === "wasm" ? "yuku.wasm" : "yuku.node")),
  join("yuku", "dist", "index.js"),
];
const missing = builds.filter((build) => Bun.file(join(packages, build)).size === 0);
if (missing.length > 0) {
  console.error(`npm is missing ${missing.join(", ")}`);
  console.error("run `zig build native` and `bun run build` first");
  process.exit(1);
}

const { version } = await Bun.file(join(packages, "yuku", "package.json")).json();
const tag = /-([A-Za-z]+)/.exec(version)?.[1] ?? "latest";
for (const directory of [...CORES, "yuku"].map((name) => join(packages, name))) {
  const { name } = await Bun.file(join(directory, "package.json")).json();
  const published = await $`npm view ${name}@${version} version`.quiet().nothrow();
  if (published.exitCode === 0 && published.text().trim() === version) {
    console.log(`${name}@${version} is already on npm`);
    continue;
  }
  await $`npm publish --access public --provenance --tag ${tag}`.cwd(directory);
}
