import Bun from 'bun'
import { rm } from 'node:fs/promises'
import path from 'path'

const TEST_SUITE_REPO_URL = "https://github.com/yuku-toolchain/parser-test-suite"
const REMOVE_FILES = ["README.md", ".gitignore"]

const shouldLoad = !(await Bun.file('test/suite').exists())

if (!shouldLoad) {
  process.exit(0)
}

const argv = Bun.argv.slice(2)
let dest = '.'
let branchIndex = argv.findIndex(arg => arg === '--branch' || arg === '-b')
let branch = null

if (argv.length > 0) {
  if (branchIndex !== -1 && argv[branchIndex + 1]) {
    branch = argv[branchIndex + 1]
    argv.splice(branchIndex, 2)
  }
  if (argv.length > 0) {
    dest = argv[0]
  }
}

console.log("\nDownloading test suite...")

const gitCmd = [
  "git", "clone", "--quiet", "--no-progress", "--single-branch", "--depth", "1",
  ...(branch ? ["--branch", branch] : []),
  TEST_SUITE_REPO_URL, dest
]

Bun.spawnSync({ cmd: gitCmd })

for (const fileToRemove of REMOVE_FILES) {
  try {
    await rm(path.join(dest, fileToRemove), { force: true })
  } catch (err) {
    console.warn(`Failed to remove file ${fileToRemove}:`, err)
  }
}

console.log("\nTest suite downloaded\n")
