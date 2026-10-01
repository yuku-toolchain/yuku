/** A real codebase the analyzer is tested on, pinned to a commit. */
export interface Project {
  name: string;
  /** The GitHub repository, as `owner/name`. */
  repository: string;
  commit: string;
  /** The directories fetched, from the repository root. */
  sources: string[];
  /** Paths under `sources` the project's own tsconfig leaves out. */
  exclude?: string[];
  /** Module names mapped to files from the repository root, as a tsconfig `paths`. */
  paths?: Record<string, string[]>;
}

export const PROJECTS_DIR = "test/projects";

export const PROJECTS: Project[] = [
  {
    name: "typescript",
    repository: "microsoft/TypeScript",
    commit: "050880ce59e30b356b686bd3144efe24f875ebc8",
    sources: ["src/compiler"],
  },
  {
    name: "vue",
    repository: "vuejs/core",
    commit: "4ab865a848a1da3d10fb674f857e5fff13094644",
    sources: ["packages"],
    exclude: ["packages/runtime-core/types"],
    paths: { "@vue/*": ["packages/*/src"], vue: ["packages/vue/src"] },
  },
  {
    name: "svelte",
    repository: "sveltejs/svelte",
    commit: "020242d6bef059df9ae8c13dc8dbff4c9b31e0ff",
    sources: ["packages/svelte/src"],
  },
  {
    name: "preact",
    repository: "preactjs/preact",
    commit: "3fcc391adc243d479ab10b4cf70fa609708c9348",
    sources: ["src", "hooks/src", "compat/src"],
    paths: { preact: ["src/index.js"], "preact/hooks": ["hooks/src/index.js"] },
  },
  {
    name: "three",
    repository: "mrdoob/three.js",
    commit: "157f0885b8428b5ffe8f6f7309b2d6f59faa1497",
    sources: ["src"],
  },
  {
    name: "zod",
    repository: "colinhacks/zod",
    commit: "004d800c9e3cd4c79930f55aa4ad080225b22efd",
    sources: ["packages/zod/src"],
  },
  {
    name: "date-fns",
    repository: "date-fns/date-fns",
    commit: "717ce0a807ea4c6b540d015b5c408723175b2838",
    sources: ["pkgs/core/src"],
  },
  {
    name: "excalidraw",
    repository: "excalidraw/excalidraw",
    commit: "1919728724a1b71af73cb7e6d2d1a418a1415b1c",
    sources: ["packages"],
  },
];
