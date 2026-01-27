import Bun from 'bun'

const dest = Bun.argv[2] || '.'

const TEST_SUITE_REPO_URL = "https://github.com/arshad-yaseen/typescript-estree-parser-test-suite"

Bun.spawnSync({
  cmd: ["git", "clone", "--quiet", "--no-progress", "--single-branch", "--depth", "1", TEST_SUITE_REPO_URL, dest]
})
