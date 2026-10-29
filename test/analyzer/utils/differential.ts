import { expect, test } from "bun:test";
import ts from "typescript";
import type { Binding, SourceFile, SourceType } from "yuku";
import type { CorpusFile } from "../../corpus";

const FUNCTIONS = new Set(["FunctionDeclaration", "FunctionExpression", "ArrowFunctionExpression"]);

/** Whether Annex B hoists `binding` from a block, which neither tsc nor scope-manager models. */
export function isAnnexBHoisted(file: SourceFile, binding: Binding): boolean {
  if (binding.scope.kind === "block") return false;
  return binding.declarations.some((node) => {
    const declaration = file.parentOf(node);
    if (declaration?.type !== "FunctionDeclaration") return false;
    const holder = file.parentOf(declaration);
    if (holder?.type === "SwitchCase" || holder?.type === "IfStatement") return true;
    return holder?.type === "BlockStatement" && !FUNCTIONS.has(file.parentOf(holder)?.type ?? "");
  });
}

export interface Comparison {
  compared: number;
  mismatches: string[];
}

// reason, then path, then the exact mismatches that file must show
export type Known = Record<string, Record<string, string[]>>;

export const SAMPLE_MAX = 20;

export function differential(
  name: string,
  files: CorpusFile[],
  compare: (file: CorpusFile, source: string) => Comparison | null,
  known: Known,
): void {
  const listed = new Map<string, string[]>();
  for (const covered of Object.values(known)) {
    for (const [path, mismatches] of Object.entries(covered)) listed.set(path, mismatches);
  }
  test.skipIf(files.length === 0)(
    name,
    async () => {
      let compared = 0;
      let read = 0;
      const mismatches: string[] = [];
      const stale = new Set(listed.keys());
      const changed: string[] = [];
      for (const file of files) {
        const result = compare(file, await Bun.file(file.path).text());
        if (result === null) continue;
        read++;
        compared += result.compared;
        const path = file.path.replaceAll("\\", "/");
        const expected = listed.get(path);
        if (expected !== undefined) {
          stale.delete(path);
          const actual = [...result.mismatches].sort();
          if (actual.join("\n") !== [...expected].sort().join("\n")) {
            changed.push(`${path} ${JSON.stringify(actual)}`);
          }
        } else if (result.mismatches.length > 0 && mismatches.length < SAMPLE_MAX) {
          mismatches.push(...result.mismatches.map((mismatch) => `${path} ${mismatch}`));
        }
      }
      console.log(`${name}: ${compared} agreed across ${read} of ${files.length} files`);
      expect([...stale]).toEqual([]);
      expect(changed).toEqual([]);
      expect(mismatches.slice(0, SAMPLE_MAX)).toEqual([]);
    },
    600_000,
  );
}

interface SingleFile {
  name: string;
  source: string;
  kind: ts.ScriptKind;
  sourceType: SourceType;
}

interface SingleFileProgram {
  program: ts.Program;
  file: ts.SourceFile;
  /** Whether tsc reports an error naming `name` at one of `positions`. */
  rejects(name: string, positions: number[]): boolean;
}

const { getSetExternalModuleIndicator } = ts as unknown as {
  getSetExternalModuleIndicator(options: ts.CompilerOptions): (file: ts.SourceFile) => void;
};

// a tsc program of one file and no lib, a module or a script as yuku reads it
export function singleFileProgram(
  single: SingleFile,
  compilerOptions: ts.CompilerOptions,
  writeFile: ts.WriteFileCallback = () => {},
): SingleFileProgram {
  const options: ts.CompilerOptions = {
    ...compilerOptions,
    noLib: true,
    noResolve: true,
    moduleDetection:
      single.sourceType === "module" ? ts.ModuleDetectionKind.Force : ts.ModuleDetectionKind.Legacy,
  };
  const file = ts.createSourceFile(
    single.name,
    single.source,
    {
      languageVersion: ts.ScriptTarget.ESNext,
      setExternalModuleIndicator: getSetExternalModuleIndicator(options),
    },
    true,
    single.kind,
  );
  const host: ts.CompilerHost = {
    getSourceFile: (name) => (name === single.name ? file : undefined),
    getDefaultLibFileName: () => "lib.d.ts",
    writeFile,
    getCurrentDirectory: () => "",
    getCanonicalFileName: (name) => name,
    useCaseSensitiveFileNames: () => true,
    getNewLine: () => "\n",
    fileExists: (name) => name === single.name,
    readFile: () => undefined,
  };
  const program = ts.createProgram([single.name], options, host);
  let errors: readonly ts.Diagnostic[] | null = null;
  return {
    program,
    file,
    rejects(name, positions) {
      errors ??= [
        ...program.getSyntacticDiagnostics(file),
        ...program.getSemanticDiagnostics(file),
      ];
      const quoted = `'${name}'`;
      return errors.some(({ start, length, messageText }) => {
        if (start === undefined || length === undefined) return false;
        if (!ts.flattenDiagnosticMessageText(messageText, "\n").includes(quoted)) return false;
        return positions.some((position) => position >= start && position < start + length);
      });
    },
  };
}
