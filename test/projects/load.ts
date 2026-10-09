// fetches every project at its pinned commit

import { existsSync } from "node:fs";
import { mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { PROJECTS, PROJECTS_DIR, type Project } from "./manifest";

const REV_FILE = ".rev";

function git(cwd: string, ...args: string[]): void {
  const result = Bun.spawnSync({ cmd: ["git", ...args], cwd, stderr: "inherit" });
  if (result.exitCode !== 0) throw new Error(`git ${args.join(" ")} failed in ${cwd}`);
}

async function load(project: Project): Promise<void> {
  const root = join(PROJECTS_DIR, project.name);
  const wanted = `${project.repository} ${project.commit} ${project.sources.join(" ")}\n`;
  const current = await readFile(join(root, REV_FILE), "utf8").catch(() => null);
  if (current === wanted) return;

  console.log(`fetching ${project.repository} at ${project.commit.slice(0, 12)}`);
  await rm(root, { recursive: true, force: true });
  await mkdir(root, { recursive: true });
  git(root, "init", "--quiet");
  // keep line endings as committed, even where `text=auto` asks for the platform's
  git(root, "config", "core.autocrlf", "false");
  git(root, "config", "core.eol", "lf");
  git(root, "remote", "add", "origin", `https://github.com/${project.repository}.git`);
  git(root, "sparse-checkout", "set", "--cone", ...project.sources);
  git(root, "fetch", "--quiet", "--depth", "1", "--filter=blob:none", "origin", project.commit);
  git(root, "checkout", "--quiet", "FETCH_HEAD");
  await rm(join(root, ".git"), { recursive: true, force: true });
  await writeFile(join(root, REV_FILE), wanted);
}

if (!existsSync(PROJECTS_DIR)) throw new Error("run from the repository root");
for (const project of PROJECTS) await load(project);
