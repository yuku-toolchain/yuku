import Bun from 'bun'

const DEST = "profiler/files"
const PARSER_BENCH_FILES_REPO_URL = "https://github.com/yuku-toolchain/parser-benchmark-files"

const shouldLoad = !(await Bun.file('profiler/files').exists())

if (!shouldLoad) {
  process.exit(0)
}

console.log("\nDownloading parser benchmark files...")

const gitCmd = [
  "git", "clone", "--quiet", "--no-progress", "--single-branch", "--depth", "1",
  PARSER_BENCH_FILES_REPO_URL, DEST
]

Bun.spawnSync({ cmd: gitCmd })

console.log("\nParser benchmark files downloaded\n")
