import { describe, expect, test } from "bun:test";
import * as espree from "espree";
import { parse, type SourceType, type Token } from "yuku-parser";
import { corpusFiles } from "../../corpus";

interface EspreeToken {
  type: string;
  value: string;
  start: number;
  end: number;
}

function espreeTokens(source: string, jsx: boolean, sourceType: SourceType): EspreeToken[] {
  const program = espree.parse(source, {
    ecmaVersion: "latest",
    sourceType,
    range: true,
    tokens: true,
    ecmaFeatures: { jsx },
  }) as unknown as { tokens: EspreeToken[] };
  return program.tokens;
}

function firstMismatch(source: string, mine: Token[], theirs: EspreeToken[]): string | null {
  if (mine.length !== theirs.length) {
    return `${mine.length} tokens, espree has ${theirs.length}`;
  }
  for (let i = 0; i < mine.length; i++) {
    const a = mine[i]!;
    const b = theirs[i]!;
    if (a.start !== b.start || a.end !== b.end) {
      return `[${i}] span ${a.start}..${a.end} vs ${b.start}..${b.end}`;
    }
    if (a.type !== b.type) {
      return `[${i}] ${JSON.stringify(a.value)} is ${a.type}, espree says ${b.type}`;
    }
    const expected = a.type === "JSXText" ? source.slice(a.start, a.end) : b.value;
    if (a.value !== expected) {
      return `[${i}] value ${JSON.stringify(a.value)} vs ${JSON.stringify(expected)}`;
    }
  }
  return null;
}

describe("token differential", () => {
  const corpus = corpusFiles().filter((file) => file.lang === "js" || file.lang === "jsx");
  const SAMPLE_TARGET = Number(process.env.DIFFERENTIAL_SAMPLE ?? 600);
  const MISMATCH_SAMPLE_MAX = 12;
  const step = Math.max(1, Math.floor(corpus.length / SAMPLE_TARGET));

  test.skipIf(corpus.length === 0)("sampled corpus tokenizes identically", async () => {
    const samples: { file: string; detail: string }[] = [];
    let tokens = 0;
    let files = 0;
    let unparsable = 0;

    for (let index = 0; index < corpus.length; index += step) {
      const file = corpus[index]!;
      const source = await Bun.file(file.path).text();

      let mine: Token[];
      let theirs: EspreeToken[];
      try {
        mine = parse(source, { tokens: true, lang: file.lang, sourceType: file.sourceType }).tokens;
        theirs = espreeTokens(source, file.lang === "jsx", file.sourceType);
      } catch {
        unparsable++;
        continue;
      }

      files++;
      tokens += mine.length;
      const detail = firstMismatch(source, mine, theirs);
      if (detail !== null && samples.length < MISMATCH_SAMPLE_MAX) {
        samples.push({ file: file.path, detail });
      }
    }

    console.log(
      `token differential: ${tokens} tokens matched espree across ${files} files ` +
        `(${unparsable} espree could not parse)`,
    );
    expect(samples).toEqual([]);
    expect(tokens).toBeGreaterThan(10_000);
  }, 240_000);
});
