import { expect, test } from "bun:test";
import type { CorpusFile } from "../../corpus";

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
