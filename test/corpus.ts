import { existsSync } from "node:fs";
import { join, sep } from "node:path";
import { Glob } from "bun";
import { parse, type SourceLang, type SourceType } from "yuku";
import { PROJECTS, PROJECTS_DIR, type Project } from "./projects/manifest";

export const CORPUS_DIRS = [
  "test/parser/suite/js/pass",
  "test/parser/suite/jsx/pass",
  "test/parser/suite/ts/pass",
];

const CORPUS_GLOB = "**/*.{js,jsx,ts,tsx,mjs,cjs,mts,cts}";
const BATCH_SIZE = 256;

export interface CorpusFile {
  path: string;
  relative: string;
  lang: SourceLang;
  sourceType: SourceType;
}

export function corpusPresent(): boolean {
  return CORPUS_DIRS.some((dir) => existsSync(dir));
}

export function corpusFilesUnder(dir: string): CorpusFile[] {
  return filesUnder(dir, (path) => (path.includes(".module.") ? "module" : "script"));
}

function filesUnder(dir: string, sourceTypeOf: (path: string) => SourceType): CorpusFile[] {
  if (!existsSync(dir)) return [];
  const files: CorpusFile[] = [];
  for (const relative of new Glob(CORPUS_GLOB).scanSync({ cwd: dir })) {
    const path = join(dir, relative);
    files.push({ path, relative, lang: parse("", { path }).lang, sourceType: sourceTypeOf(path) });
  }
  files.sort((a, b) => (a.path < b.path ? -1 : a.path > b.path ? 1 : 0));
  return files;
}

export interface LoadedProject {
  project: Project;
  root: string;
  files: CorpusFile[];
}

export function loadedProjects(): LoadedProject[] {
  const loaded: LoadedProject[] = [];
  for (const project of PROJECTS) {
    const root = join(PROJECTS_DIR, project.name);
    if (!existsSync(root)) continue;
    const excluded = (project.exclude ?? []).map((path) => join(root, path) + sep);
    const sourceTypeOf = (path: string): SourceType => {
      if (project.type === "commonjs" && path.endsWith(".js")) return "commonjs";
      return parse("", { path }).sourceType;
    };
    const files = project.sources
      .flatMap((source) => filesUnder(join(root, source), sourceTypeOf))
      .filter((file) => !excluded.some((path) => file.path.startsWith(path)));
    loaded.push({ project, root, files });
  }
  return loaded;
}

export function corpusAndProjectFiles(): CorpusFile[] {
  const projects = loadedProjects().flatMap((loaded) => loaded.files);
  return [...CORPUS_DIRS.flatMap(corpusFilesUnder), ...projects];
}

// batched, so thousands of files never open at once
export async function forEachCorpusFile(
  files: CorpusFile[],
  visit: (file: CorpusFile, source: string) => void,
): Promise<void> {
  for (let i = 0; i < files.length; i += BATCH_SIZE) {
    const batch = files.slice(i, i + BATCH_SIZE);
    await Promise.all(batch.map(async (file) => visit(file, await Bun.file(file.path).text())));
  }
}
