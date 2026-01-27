// this is a wasm module, and i know it's not expected to use for these kinds of tasks, also not expected to use in node
// so replace it with the yuku node (napi) module when we have it
import { parseSync, preload } from "../npm/dist/browser"
import { Glob } from "bun"
import equal from "fast-deep-equal"
import { diff } from "jest-diff"
import { basename, dirname, join } from "path"
import { mkdir } from "fs/promises"

await preload()

console.clear()

const isCI = !!process.env.CI
const updateSnapshots = process.argv.includes("--update-snapshots")

type TestType = "should_pass" | "should_fail" | "snapshot"
type Language = "js" | "ts" | "jsx" | "tsx" | "dts"

interface TestConfig {
  path: string
  type: TestType
  languages: Language[]
  exclude?: string[] // file paths to exclude
  skipOnCI?: boolean
}

const configs: TestConfig[] = [
  { path: "test/suite/js/pass", type: "snapshot", languages: ["js"] },
  { path: "test/suite/js/fail", type: "should_fail", languages: ["js"],
    exclude: [
      // these are the semantic tests
      // remove these from this exclude list when we implement a visitor/traverser and semantic analyzer
      "67c714796e7f40a4.js",
      "e6559958e6954318.js",
      "4e2cce832b4449f1.js",
      "317c81f05510f4ad.js",
      "76465e2c7af91e73.js",
      "fb130c395c6aafe7.js",
      "c7ad2478fd72bffe.js",
      "5e6f67a0e748cc42.js",
      "efcb54b62e8f0e06.js",
      "8b72c44bd531621a.js",
      "ee0034582e67e51e.js",
      "2b050de45ab44c8c.js",
      "3078b4fed5626e2a.js",
      "04bc213db9cd1130.js",
      "4a887c2761eb95fb.js",
      "16947dc1d11e5e70.js",
      "8d5ef4dee9c7c622.js",
      "e808e347646c2670.js",
      "f2db53245b89c72f.js",
      "73d061b5d635a807.js",
      "b88ab70205263170.module.js",
      "6cd36f7e68bdfb7a.js",
      "a4bfa8e3b523c466.module.js",
      "858b72be7f8f19d7.js",
      "2226edabbd2261a7.module.js",
      "d54b2db4548f1d82.module.js",
      "5059efc702f08060.js",
      "f063969b23239390.module.js"
  ] },
  { path: "test/suite/jsx/pass", type: "snapshot", languages: ["jsx"] },
  { path: "test/suite/jsx/fail", type: "should_fail", languages: ["jsx"] },
  { path: "test/misc/jsx", type: "snapshot", languages: ["jsx"] },
  { path: "test/misc/js", type: "snapshot", languages: ["js"] },
]

interface TestResult {
  path: string
  passed: number
  failed: number
  total: number
  failures: string[]
  parseTime: number
  parsedFiles: number
}

const results = new Map<string, TestResult>()

const formatTime = (ms: number): string => {
  if (ms >= 1000) {
    return `${(ms / 1000).toFixed(2)}s`
  }

  if(ms <= 1)
    return `${ms.toFixed(3)}ms`

  return `${ms.toFixed(2)}ms`
}

const getLanguage = (path: string): Language => {
  if (path.endsWith(".tsx")) return "tsx"
  if (path.endsWith(".jsx")) return "jsx"
  if (path.endsWith(".d.ts")) return "dts"
  if (path.endsWith(".ts")) return "ts"
  return "js"
}

const getBaseName = (file: string): string => {
  const name = basename(file)
  const firstDot = name.indexOf(".")
  return firstDot >= 0 ? name.substring(0, firstDot) : name
}

const isTestArtifact = (path: string): boolean => {
  return (
    path.endsWith(".snapshot.json") ||
    path.includes(".snap") ||
    path.includes("/snapshots/") ||
    path.includes("\\snapshots\\")
  )
}

const isExcluded = (path: string, excludePatterns: string[] = []): boolean => {
  return excludePatterns.some(pattern => {
    return path.includes(pattern) || basename(path) === pattern
  })
}

const shouldIncludeFile = (path: string, languages: Language[], exclude?: string[]): boolean => {
  if (isTestArtifact(path)) return false
  if (isExcluded(path, exclude)) return false
  const lang = getLanguage(path)
  return languages.includes(lang)
}

const runTest = async (
  file: string,
  type: TestType,
  result: TestResult,
): Promise<void> => {
  try {
    const content = await Bun.file(file).text()
    const lang = getLanguage(file)
    const sourceType = file.includes(".module.") ? "module" : "script"

    const parseStart = performance.now()
    const parsed = parseSync(content, { sourceType, lang })
    const parseEnd = performance.now()

    result.parseTime += parseEnd - parseStart
    result.parsedFiles++

    const hasErrors = parsed.errors && parsed.errors.length > 0

    if (type === "should_pass") {
      if (hasErrors) {
        result.failures.push(file)
        return
      }
      result.passed++
      return
    }

    if (type === "should_fail") {
      if (!hasErrors) {
        result.failures.push(file)
        return
      }
      result.passed++
      return
    }

    if (type === "snapshot") {
      const dir = dirname(file)
      const snapshotsDir = join(dir, "snapshots")
      const base = getBaseName(file)
      const snapshotFile = join(snapshotsDir, `${base}.snapshot.json`)
      const snapshotExists = await Bun.file(snapshotFile).exists()

      if (!snapshotExists) {
        await mkdir(snapshotsDir, { recursive: true })
        await Bun.write(snapshotFile, JSON.stringify(parsed, null, 2))
        result.passed++
        return
      }

      const snapshot = await Bun.file(snapshotFile).json()

      if (!equal(parsed, snapshot)) {
        if (updateSnapshots) {
          await Bun.write(snapshotFile, JSON.stringify(parsed, null, 2))
          result.passed++
          return
        }
        const difference = diff(snapshot, parsed, { contextLines: 2 })
        if (!isCI) {
          console.log(`\nx ${file}\n${difference}\n`)
        }
        result.failures.push(file)
        return
      }

      result.passed++
    }
  } catch (err) {
    result.failures.push(`${file} (error: ${err})`)
  }
}

const runCategory = async (config: TestConfig) => {
  const result: TestResult = {
    path: config.path,
    passed: 0, failed: 0, total: 0, failures: [], parseTime: 0, parsedFiles: 0 }
  results.set(config.path, result)

  const pattern = `${config.path}/**/*`
  const glob = new Glob(pattern)
  const files: string[] = []

  for await (const file of glob.scan(".")) {
    if (shouldIncludeFile(file, config.languages, config.exclude)) {
      files.push(file)
    }
  }

  result.total = files.length

  if (result.total === 0) return

  for (const file of files) {
    await runTest(file, config.type, result)
  }

  result.failed = result.failures.length
}

console.log("Running tests...\n")

for (const config of configs) {
  if (config.skipOnCI && isCI) {
    console.log(`\nSkipping ${config.path} on CI`)
    continue
  }

  await runCategory(config)
}

let totalPassed = 0
let totalFailed = 0
let totalTests = 0
let totalParseTime = 0
let totalParsedFiles = 0

for (const [, result] of results) {
  const status = result.failed === 0 ? "✓" : "x"

  console.log(`${status} ${result.path} ${result.passed}/${result.total}`)

  result.failures.forEach(f => console.log(`  x ${f}`))
  console.log('')

  if (result.total === 0) continue
  totalPassed += result.passed
  totalFailed += result.failed
  totalTests += result.total
  totalParseTime += result.parseTime
  totalParsedFiles += result.parsedFiles
}

const passRate = ((totalPassed / totalTests) * 100).toFixed(2)
const avgParseTime = totalParsedFiles > 0 ? totalParseTime / totalParsedFiles : 0

console.log(`\n${totalPassed}/${totalTests} (${passRate}%) • ${formatTime(totalParseTime)} for parsing ${totalParsedFiles} files • ${formatTime(avgParseTime)} to parse per file`)

if (totalFailed > 0) {
  process.exit(1)
}
