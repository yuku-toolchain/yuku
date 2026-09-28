import { existsSync } from "node:fs";
import { readdir, rm, writeFile } from "node:fs/promises";
import path from "node:path";

const TEST_SUITE_REPO_URL = "https://github.com/yuku-toolchain/parser-test-suite";
const INCLUDE_FOLDERS = ["js", "jsx", "ts"];
const SUITE_DIR = "test/parser/suite";

const REV_FILE = ".suite-rev";

const argv = Bun.argv.slice(2);
const branchIndex = argv.findIndex((arg) => arg === "--branch" || arg === "-b");
let branch: string | null = null;

if (branchIndex !== -1 && argv[branchIndex + 1]) {
  branch = argv[branchIndex + 1]!;
  argv.splice(branchIndex, 2);
}

const dest = argv[0] ?? SUITE_DIR;
const ref = branch ?? "HEAD";
const revPath = path.join(dest, REV_FILE);

type RevResult =
  | { kind: "ok"; sha: string }
  | { kind: "missing-ref" }
  | { kind: "unreachable" };

function resolveRemoteRev(): RevResult {
  const result = Bun.spawnSync({
    cmd: ["git", "ls-remote", TEST_SUITE_REPO_URL, ref],
    stderr: "inherit",
  });

  if (result.exitCode !== 0) return { kind: "unreachable" };

  const sha = result.stdout.toString().trim().split(/\s+/)[0] ?? "";
  return /^[0-9a-f]{40}$/.test(sha) ? { kind: "ok", sha } : { kind: "missing-ref" };
}

const haveSuite = existsSync(dest);
const localRev = await Bun.file(revPath)
  .text()
  .then((text) => text.trim())
  .catch(() => null);

const remoteRev = resolveRemoteRev();

if (remoteRev.kind === "missing-ref") {
  console.error(`\n'${ref}' does not exist in ${TEST_SUITE_REPO_URL}\n`);
  process.exit(1);
}

if (remoteRev.kind === "unreachable") {
  if (haveSuite && localRev) {
    console.log("\ncould not reach the test suite remote, keeping the existing copy\n");
    process.exit(0);
  }
  console.error("\ncould not resolve the test suite revision and no local copy exists\n");
  process.exit(1);
}

const wanted = `${ref} ${remoteRev.sha}`;

if (haveSuite && localRev === wanted) {
  process.exit(0);
}

if (haveSuite) {
  console.log(`\nsuite ${localRev ? "changed upstream" : "revision unknown"}, re-downloading`);
  await rm(dest, { recursive: true, force: true });
}

console.log("\nDownloading test suite...\n");

const gitCmd = [
  "git",
  // snapshot spans are byte offsets, keep fixture line endings as committed
  "-c",
  "core.autocrlf=false",
  "clone",
  "--progress",
  "--single-branch",
  "--depth",
  "1",
  ...(branch ? ["--branch", branch] : []),
  TEST_SUITE_REPO_URL,
  dest,
];

const clone = Bun.spawnSync({ cmd: gitCmd, stderr: "inherit" });

if (clone.exitCode !== 0) {
  console.error("\nfailed to clone the test suite\n");
  process.exit(clone.exitCode ?? 1);
}

const entries = await readdir(dest);
for (const entry of entries) {
  if (!INCLUDE_FOLDERS.includes(entry)) {
    await rm(path.join(dest, entry), { recursive: true, force: true });
  }
}

await writeFile(revPath, `${wanted}\n`);

console.log("\nTest suite downloaded\n");
