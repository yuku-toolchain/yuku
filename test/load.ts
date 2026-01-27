import Bun from 'bun'
import { rm } from 'node:fs/promises'
import path from 'path'

const TEST_SUITE_REPO_URL = "https://github.com/arshad-yaseen/typescript-estree-parser-test-suite"
const REMOVE_FILES = ["README.md", ".gitignore"]

const shouldLoad = !(await Bun.file('test/suite').exists())

if (!shouldLoad) {
  process.exit(0)
}

const dest = Bun.argv[2] || '.'

console.log("\nDownloading test suite...")

Bun.spawnSync({
  cmd: ["git", "clone", "--quiet", "--no-progress", "--single-branch", "--depth", "1", TEST_SUITE_REPO_URL, dest]
})


for (const fileToRemove of REMOVE_FILES) {
  try {
    await rm(path.join(dest, fileToRemove), { force: true })
  } catch (err) {
    console.warn(`Failed to remove file ${fileToRemove}:`, err)
  }
}

console.log("\nTest suite downloaded\n")
