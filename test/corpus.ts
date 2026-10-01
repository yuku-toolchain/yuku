import { existsSync } from "node:fs";
import { join, sep } from "node:path";
import { Glob } from "bun";
import { langFromPath, sourceTypeFromPath, type SourceLang, type SourceType } from "yuku-parser";
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

/** True when at least one corpus directory has been downloaded. */
export function corpusPresent(): boolean {
  return CORPUS_DIRS.some((dir) => existsSync(dir));
}

/** Every corpus file under one directory, sorted by path for stable order. */
export function corpusFilesUnder(dir: string): CorpusFile[] {
  if (!existsSync(dir)) return [];
  const files: CorpusFile[] = [];
  for (const relative of new Glob(CORPUS_GLOB).scanSync({ cwd: dir })) {
    const path = join(dir, relative);
    files.push({ path, relative, lang: langFromPath(path), sourceType: sourceTypeFromPath(path) });
  }
  files.sort((a, b) => (a.path < b.path ? -1 : a.path > b.path ? 1 : 0));
  return files;
}

/** Every corpus file across all directories, with inferred language and type. */
export function corpusFiles(): CorpusFile[] {
  return CORPUS_DIRS.flatMap(corpusFilesUnder);
}

/** A fetched project and its source files, see `test/projects/manifest.ts`. */
export interface LoadedProject {
  project: Project;
  root: string;
  files: CorpusFile[];
}

/** Every project fetched by `test/projects/load.ts`. */
export function loadedProjects(): LoadedProject[] {
  const loaded: LoadedProject[] = [];
  for (const project of PROJECTS) {
    const root = join(PROJECTS_DIR, project.name);
    if (!existsSync(root)) continue;
    const excluded = (project.exclude ?? []).map((path) => join(root, path) + sep);
    const files = project.sources
      .flatMap((source) => corpusFilesUnder(join(root, source)))
      .filter((file) => !excluded.some((path) => file.path.startsWith(path)));
    loaded.push({ project, root, files });
  }
  return loaded;
}

/** The source files of every fetched project. */
export function projectFiles(): CorpusFile[] {
  return loadedProjects().flatMap((loaded) => loaded.files);
}

/**
 * Runs `fn` over every corpus file, reading sources in batches so thousands of
 * files do not open at once.
 */
export async function forEachCorpusFile(
  fn: (file: CorpusFile, source: string) => void,
): Promise<void> {
  const files = corpusFiles();
  for (let i = 0; i < files.length; i += BATCH_SIZE) {
    const batch = files.slice(i, i + BATCH_SIZE);
    await Promise.all(batch.map(async (file) => fn(file, await Bun.file(file.path).text())));
  }
}
