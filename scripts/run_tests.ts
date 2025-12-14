import { Glob } from "bun";
import { join } from "path";
import equal from "fast-deep-equal";
import { diff } from "jest-diff";

const FOLDERS = ["test/pass"];

interface TestResult {
  file: string;
  passed: boolean;
  reason?: string;
  diff?: string;
}

interface Stats {
  total: number;
  passed: number;
  failed: number;
  astMismatches: number;
}

interface ParsedAST {
  errors: unknown[];
  comments: unknown[];
  program: Record<string, unknown>
}

async function readJSON(filePath: string): Promise<ParsedAST> {
  try {
    const file = Bun.file(filePath);
    const text = await file.text();
    return JSON.parse(text) as ParsedAST;
  } catch (error) {
    throw new Error(`Failed to read ${filePath}: ${error}`);
  }
}

function stripOriginalExtensions(fileName: string) {
  return fileName.replace(".js", "").replace(".module", "");
}

async function testFile(
  folderPath: string,
  jsFileName: string
): Promise<TestResult> {
  const receivedPath = join(
    folderPath,
    `${stripOriginalExtensions(jsFileName)}.received.json`
  );
  const expectedPath = join(
    folderPath,
    `${stripOriginalExtensions(jsFileName)}.expected.json`
  );

  try {
    const received = await readJSON(receivedPath);
    const expected = await readJSON(expectedPath);

    if (received.errors && received.errors.length > 0) {
      return {
        file: jsFileName,
        passed: false,
        reason: "Parse errors",
      };
    }

    const expectedToCheck = parsedAstToCheck(expected);
    const receivedToCheck = parsedAstToCheck(received);

    if (!equal(receivedToCheck, expectedToCheck)) {
      const difference = diff(expectedToCheck, receivedToCheck, {
        contextLines: 2,
        expand: false,
      });

      return {
        file: jsFileName,
        passed: false,
        reason: "AST mismatch",
        diff: difference || "AST structures differ",
      };
    }

    return {
      file: jsFileName,
      passed: true,
    };
  } catch (error) {
    return {
      file: jsFileName,
      passed: false,
      reason: `Error: ${error}`,
    };
  }
}

async function runTests(folderPath: string): Promise<TestResult[]> {
  const results: TestResult[] = [];
  const glob = new Glob("*.js");

  for await (const file of glob.scan(folderPath)) {
    const path = `test/pass/${file}`;
    const text = await Bun.file(path).text();
    if(!text.includes('import') && !text.includes('export')) {
      const result = await testFile(folderPath, file);
      if(!result.passed) {
        console.log(`${path} -- ${result.reason}`);
      }
      results.push(result);
    }
  }

  return results;
}

function calculateStats(results: TestResult[]): Stats {
  const passed = results.filter((r) => r.passed).length;
  const astMismatches = results.filter((r) => r.reason === "AST mismatch").length;

  return {
    total: results.length,
    passed,
    failed: results.length - passed,
    astMismatches,
  };
}

function calculateConformance(stats: Stats): number {
  if (stats.total === 0) return 0;
  return (stats.passed / stats.total) * 100;
}

async function main() {
  const allResults: TestResult[] = [];

  for (const folderPath of FOLDERS) {
    const results = await runTests(folderPath);
    allResults.push(...results);
  }

  const astMismatches = allResults.filter(
    (r) => !r.passed && r.reason === "AST mismatch"
  );

  if (astMismatches.length > 0) {
    console.log("\nAST Mismatches:\n");
    for (const failure of astMismatches) {
      console.log(`${failure.file}`);
      if (failure.diff) {
        console.log(`${failure.diff}\n`);
      }
    }
  }

  const stats = calculateStats(allResults);
  const conformance = calculateConformance(stats);

  console.log("\nTest Summary\n");
  console.log(`Total tests:       ${stats.total}`);
  console.log(`Passed:            ${stats.passed}`);
  console.log(`Failed:            ${stats.failed}`);
  console.log(`AST mismatches:    ${stats.astMismatches}`);
  console.log(`Conformance:       ${conformance.toFixed(2)}%\n`);
}

function parsedAstToCheck(ast: ParsedAST) {
  return {program: ast.program, comments: ast.comments}
}

main();
