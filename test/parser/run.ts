import { mkdir } from "node:fs/promises";
import { basename, dirname, join } from "node:path";
import { Glob } from "bun";
import equal from "fast-deep-equal";
import { diff } from "jest-diff";
import { parse, type Diagnostic } from "yuku-parser";
import { deserializeAstJson, formatDiagnostics, serializeAstJson } from "../ast-helpers-for-test";

console.clear();
console.log("");

const TEST_DIR = "test";
const PARSER_TEST_DIR = `${TEST_DIR}/parser`;
const RESULTS_DIR = `${TEST_DIR}/results/parser`;
const isCI = !!process.env.CI;
const updateSnapshots = process.argv.includes("--update-snapshots");

type TestType = "should_pass" | "should_fail" | "snapshot";
type Language = "js" | "ts" | "jsx" | "tsx" | "dts";

interface TestConfig {
	path: string;
	type: TestType;
	languages: Language[];
	exclude?: string[];
	skipOnCI?: boolean;
	checkAstOnError?: boolean;
	semanticErrors?: boolean;
}

const configs: TestConfig[] = [
	{ path: "suite/js/pass", type: "snapshot", languages: ["js"], semanticErrors: true },
	{ path: "suite/js/fail", type: "should_fail", languages: ["js"] },
	{
		path: "suite/js/semantic",
		type: "should_fail",
		languages: ["js"],
		semanticErrors: true,
		skipOnCI: true,
	},
	{ path: "suite/jsx/pass", type: "snapshot", languages: ["jsx"], semanticErrors: true },
	{ path: "suite/jsx/fail", type: "should_fail", languages: ["jsx"] },
	{
		path: "misc/jsx",
		type: "snapshot",
		languages: ["jsx"],
		checkAstOnError: true,
	},
	{
		path: "misc/js",
		type: "snapshot",
		languages: ["js"],
		checkAstOnError: true,
	},
];

interface DiagnosticEntry {
	file: string;
	source: string;
	diagnostics: Diagnostic[];
}

interface TestResult {
	path: string;
	passed: number;
	failed: number;
	total: number;
	failures: string[];
	astMismatches: number;
	astComparisons: number;
	fileResults: { file: string; passed: boolean }[];
	diagnosticEntries: DiagnosticEntry[];
}

const results = new Map<string, TestResult>();

const getLanguage = (path: string): Language => {
	if (path.endsWith(".tsx")) return "tsx";
	if (path.endsWith(".jsx")) return "jsx";
	if (path.endsWith(".d.ts")) return "dts";
	if (path.endsWith(".ts")) return "ts";
	return "js";
};

const getBaseName = (file: string): string => {
	const name = basename(file);
	const firstDot = name.indexOf(".");
	return firstDot >= 0 ? name.substring(0, firstDot) : name;
};

const isTestArtifact = (path: string): boolean => {
	return (
		path.endsWith(".snapshot.json") ||
		path.includes(".snap") ||
		path.includes("/snapshots/") ||
		path.includes("\\snapshots\\")
	);
};

const shouldIncludeFile = (
	path: string,
	languages: Language[],
	exclude?: string[],
): boolean => {
	if (isTestArtifact(path)) return false;
	if (exclude?.some((p) => path.includes(p) || basename(path) === p)) {
		return false;
	}
	return languages.includes(getLanguage(path));
};

let progressCurrent = 0;
let progressTotal = 0;

const updateProgress = (file: string, passed: boolean) => {
	if (isCI) return;
	progressCurrent++;
	const icon = passed ? "\x1b[32m✓\x1b[0m" : "\x1b[31m✗\x1b[0m";
	const label = file.length > 60 ? `...${file.slice(-57)}` : file;
	process.stdout.write(
		`\r\x1b[K  ${icon} ${progressCurrent}/${progressTotal}  ${label}`,
	);
};

const clearProgress = () => {
	if (isCI) return;
	process.stdout.write("\r\x1b[K");
};

const runTest = async (
	file: string,
	config: TestConfig,
	result: TestResult,
): Promise<boolean> => {
	try {
		const { type, checkAstOnError } = config;
		const content = await Bun.file(file).text();
		const lang = getLanguage(file);
		const sourceType = file.includes(".module.") ? "module" : "script";

		const parsed = parse(content, {
			sourceType,
			lang,
      semanticErrors: config.semanticErrors ?? false,
      preserveParens: true
		});

    const hasErrors = parsed.diagnostics && parsed.diagnostics.length > 0;

    if (hasErrors) {
      result.diagnosticEntries.push({
				file,
				source: content,
				diagnostics: parsed.diagnostics,
			});
    }

		if (type === "should_pass") {
			if (hasErrors) {
				result.failures.push(file);
				return false;
			}
			result.passed++;
			return true;
		}

		if (type === "should_fail") {
			if (!hasErrors) {
				result.failures.push(file);
				return false;
			}
			result.passed++;
			return true;
		}

		if (type === "snapshot") {
			if (hasErrors && !checkAstOnError) {
				result.failures.push(file);
				return false;
			}

			const snapshotsDir = join(dirname(file), "snapshots");
			const base = getBaseName(file);
			const snapshotFile = join(snapshotsDir, `${base}.snapshot.json`);

			if (!(await Bun.file(snapshotFile).exists())) {
				await mkdir(snapshotsDir, { recursive: true });
				await Bun.write(snapshotFile, serializeAstJson(parsed, 2));
				result.passed++;
				return true;
			}

			const snapshot = deserializeAstJson(await Bun.file(snapshotFile).text());
			result.astComparisons++;

			if (!equal(parsed, snapshot)) {
				if (updateSnapshots) {
					await Bun.write(snapshotFile, serializeAstJson(parsed, 2));
					result.passed++;
					return true;
				}

				clearProgress();
				console.log(
					`\nx ${file}\n${diff(snapshot, parsed, { contextLines: 2 })}\n`,
				);
				result.failures.push(`${file} (AST mismatch)`);
				result.astMismatches++;
				return false;
			}

			result.passed++;
			return true;
		}
	} catch (err) {
		result.failures.push(`${file} - error: ${err}`);
		return false;
	}
	return true;
};

const collectFiles = async (config: TestConfig): Promise<string[]> => {
	const glob = new Glob(`${PARSER_TEST_DIR}/${config.path}/**/*`);
	const files: string[] = [];
	for await (const file of glob.scan(".")) {
		if (shouldIncludeFile(file, config.languages, config.exclude)) {
			files.push(file);
		}
	}
	return files;
};

const runCategory = async (config: TestConfig, files: string[]) => {
	const result: TestResult = {
		path: config.path,
		passed: 0,
		failed: 0,
		total: files.length,
		failures: [],
		astMismatches: 0,
		astComparisons: 0,
		fileResults: [],
		diagnosticEntries: [],
	};
	results.set(config.path, result);

	if (result.total === 0) return;

	for (const file of files) {
		const passed = await runTest(file, config, result);
		result.fileResults.push({ file, passed });
		updateProgress(file, passed);
	}

	result.failed = result.failures.length;
};

const configFiles = new Map<TestConfig, string[]>();

for (const config of configs) {
	if (config.skipOnCI && isCI) continue;
	const files = await collectFiles(config);
	configFiles.set(config, files);
	progressTotal += files.length;
}

for (const [config, files] of configFiles) {
	await runCategory(config, files);
}

clearProgress();

const getResultName = (path: string): string => {
	return `${path.replace(/^suite\//, "").replace(/\//g, "_")}.txt`;
};

await mkdir(RESULTS_DIR, { recursive: true });

let totalFailed = 0;

for (const [, result] of results) {
	if (result.total === 0) continue;

	totalFailed += result.failures.length;

	const diagMap = new Map<string, DiagnosticEntry>();
	for (const entry of result.diagnosticEntries) {
		diagMap.set(entry.file, entry);
	}

	const suiteRate = ((result.passed / result.total) * 100).toFixed(2);
	const lines: string[] = [
		result.path,
		"=".repeat(result.path.length),
		`Passed:       ${result.passed}/${result.total} (${suiteRate}%)`,
		`Failed:       ${result.failed}`,
	];

	if (result.astComparisons > 0) {
		lines.push(
			`AST mismatch: ${result.astMismatches}/${result.astComparisons}`,
		);
	}

	lines.push("");

	const sorted = [...result.fileResults].sort((a, b) =>
		a.file.localeCompare(b.file),
	);

	for (const { file, passed } of sorted) {
		const icon = passed ? "✓" : "✗";
		lines.push(`${icon} ${file}`);

		const entry = diagMap.get(file);
		if (entry && entry.diagnostics.length > 0) {
			lines.push(
				formatDiagnostics(entry.source, [entry.diagnostics[0]], file, {
					showFilename: false,
				}),
			);
			lines.push("");
		}
	}

	lines.push("");
	await Bun.write(
		`${RESULTS_DIR}/${getResultName(result.path)}`,
		lines.join("\n"),
	);
}

console.log(`Results saved to ${RESULTS_DIR}/\n`);

if (isCI && totalFailed > 0) {
	process.exit(1);
}
