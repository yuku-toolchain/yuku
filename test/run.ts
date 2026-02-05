// this is a wasm module, and i know it's not expected to use for these kinds of tasks, also not expected to use in node
// so replace it with the yuku node (napi) module when we have it
import { parseSync, preload } from "../npm/wasm/dist"
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
  checkAstOnError?: boolean
}

const configs: TestConfig[] = [
  { path: "test/suite/js/pass", type: "snapshot", languages: ["js"], skipOnCI: true },
  { path: "test/suite/js/fail", type: "should_fail", languages: ["js"], skipOnCI: true },
  // uncomment when we add semantic checks (first needs a visitor/traverser)
  // { path: "test/suite/js/semantic", type: "should_fail", languages: ["js"] },
  { path: "test/suite/jsx/pass", type: "snapshot", languages: ["jsx"] },
  { path: "test/suite/jsx/fail", type: "should_fail", languages: ["jsx"] },
  { path: "test/misc/jsx", type: "snapshot", languages: ["jsx"], checkAstOnError: true },
  { path: "test/misc/js", type: "snapshot", languages: ["js"], checkAstOnError: true },
]

interface TestResult {
  path: string
  passed: number
  failed: number
  total: number
  failures: string[]
  astMismatches: number
  astComparisons: number
}

const results = new Map<string, TestResult>()

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
  config: TestConfig,
  result: TestResult,
): Promise<void> => {
  try {
    const { type, checkAstOnError } = config
    const content = await Bun.file(file).text()
    const lang = getLanguage(file)
    const sourceType = file.includes(".module.") ? "module" : "script"

    const parsed = parseSync(content, { sourceType, lang })

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
      if (hasErrors && !checkAstOnError) {
        result.failures.push(file)
        return
      }

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

      result.astComparisons++

      if (!equal(parsed, snapshot)) {
        if (updateSnapshots) {
          await Bun.write(snapshotFile, JSON.stringify(parsed, null, 2))
          result.passed++
          return
        }
        const difference = diff(snapshot, parsed, { contextLines: 2 })
        console.log(`\nx ${file}\n${difference}\n`)
        result.failures.push(`${file} (AST mismatch)`)
        result.astMismatches++
        return
      }

      result.passed++
    }
  } catch (err) {
    result.failures.push(`${file} - error: ${err}`)
  }
}

const runCategory = async (config: TestConfig) => {
  const result: TestResult = {
    path: config.path,
    passed: 0, failed: 0, total: 0, failures: [], astMismatches: 0, astComparisons: 0
  }
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
    await runTest(file, config, result)
  }

  result.failed = result.failures.length
}

console.log("\nRunning tests...\n")

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
let totalAstMismatches = 0
let totalAstComparisons = 0

for (const [, result] of results) {
  const status = result.failed === 0 ? "✓" : "x"

  console.log(`${status} ${result.path} ${result.passed}/${result.total}`)

  result.failures.forEach(f => console.log(`  x ${f}`))
  console.log('')

  if (result.total === 0) continue
  totalPassed += result.passed
  totalFailed += result.failed
  totalTests += result.total
  totalAstMismatches += result.astMismatches
  totalAstComparisons += result.astComparisons
}

const passRate = ((totalPassed / totalTests) * 100).toFixed(2)

console.log(`\n${totalPassed}/${totalTests} (${passRate}%)`)

const saveResults = async () => {
  const lines: string[] = [
    "Test Results",
    "============",
    "",
    "Running TypeScript, Test262 and Babel test suites with ESTree/TypeScript-ESTree AST matching.",
    "",
    "Summary",
    "-------",
    `Passed:       ${totalPassed}`,
    `Failed:       ${totalFailed}`,
    `AST mismatch: ${totalAstMismatches}/${totalAstComparisons}`,
    `Total:        ${totalTests}`,
    `Coverage:     ${passRate}%`,
    "",
    "Results by Suite",
    "----------------",
  ]

  for (const [, result] of results) {
    if (result.total === 0) continue
    const status = result.failed === 0 ? "✓" : "✗"
    const suiteRate = ((result.passed / result.total) * 100).toFixed(2)
    lines.push("")
    lines.push(`${status} ${result.path}`)
    lines.push(`  Passed: ${result.passed}/${result.total} (${suiteRate}%)`)

    if (result.failures.length > 0) {
      lines.push(`  Failed: ${result.failed}`)
      lines.push("  Failures:")
      for (const failure of result.failures) {
        lines.push(`    - ${failure}`)
      }
    }
  }

  lines.push("")

  await Bun.write("test/results.txt", lines.join("\n"))
  console.log("\nResults saved to test/results.txt")
}

await saveResults()

if (totalFailed > 0) {
  process.exit(1)
}
