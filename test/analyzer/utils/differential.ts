import { expect, test } from "bun:test";
import type { CorpusFile } from "../../corpus";

/** One file compared against an oracle. */
export interface Comparison {
  compared: number;
  mismatches: string[];
}

/** Divergences by reason, with the files each covers. */
export type Known = Record<string, string[]>;

/** The most mismatches a failure lists. */
export const SAMPLE_MAX = 20;

/** Compares every file with an oracle. A file `known` lists must disagree, any other agree. */
export function differential(
  name: string,
  files: CorpusFile[],
  compare: (file: CorpusFile, source: string) => Comparison | null,
  known: Known,
): void {
  const listed = new Set(Object.values(known).flat());
  test.skipIf(files.length === 0)(
    name,
    async () => {
      let compared = 0;
      let read = 0;
      const mismatches: string[] = [];
      const stale: string[] = [];
      for (const file of files) {
        const result = compare(file, await Bun.file(file.path).text());
        if (result === null) continue;
        read++;
        compared += result.compared;
        const path = file.path.replaceAll("\\", "/");
        if (result.mismatches.length === 0) {
          if (listed.has(path)) stale.push(path);
        } else if (!listed.has(path) && mismatches.length < SAMPLE_MAX) {
          mismatches.push(...result.mismatches.map((mismatch) => `${path} ${mismatch}`));
        }
      }
      console.log(`${name}: ${compared} agreed across ${read} of ${files.length} files`);
      expect(stale).toEqual([]);
      expect(mismatches.slice(0, SAMPLE_MAX)).toEqual([]);
    },
    600_000,
  );
}
