import { readdir, rm, stat } from "node:fs/promises";
import path from "node:path";
import Bun from "bun";

const TEST_SUITE_REPO_URL = "https://github.com/yuku-toolchain/parser-test-suite";
const INCLUDE_FOLDERS = ["js", "jsx"];
const SUITE_DIR = "test/parser/suite";
const ONE_DAY_MS = 24 * 60 * 60 * 1000;

let shouldLoad = true;

try {
  const info = await stat(SUITE_DIR);
  if (Date.now() - info.mtimeMs > ONE_DAY_MS) {
    await rm(SUITE_DIR, { recursive: true, force: true });
    console.log("\nsuite expired, re-downloading");
  } else {
    shouldLoad = false;
  }
} catch {
  // directory doesn't exist
}

if (!shouldLoad) {
  process.exit(0);
}

const argv = Bun.argv.slice(2);
let dest = ".";
const branchIndex = argv.findIndex((arg) => arg === "--branch" || arg === "-b");
let branch = null;

if (argv.length > 0) {
  if (branchIndex !== -1 && argv[branchIndex + 1]) {
    branch = argv[branchIndex + 1];
    argv.splice(branchIndex, 2);
  }
  if (argv.length > 0) {
    dest = argv[0];
  }
}

console.log("\nDownloading test suite...\n");

const gitCmd = [
  "git",
  "clone",
  "--progress",
  "--single-branch",
  "--depth",
  "1",
  ...(branch ? ["--branch", branch] : []),
  TEST_SUITE_REPO_URL,
  dest,
];

Bun.spawnSync({ cmd: gitCmd, stderr: "inherit" });

const entries = await readdir(dest);
for (const entry of entries) {
  if (!INCLUDE_FOLDERS.includes(entry)) {
    await rm(path.join(dest, entry), { recursive: true, force: true });
  }
}

console.log("\nTest suite downloaded\n");
