import { Glob } from "bun";
import { join } from "node:path";

interface Page {
  slug: string; // "" for the home page
  url: string;
  title: string;
  description: string;
  body: string;
}

interface Site {
  title: string;
  host: string;
  repo: string;
}

const DOCS_DIR = import.meta.dir;
const CONTENT_DIR = join(DOCS_DIR, "content");
const ASSETS_DIR = join(DOCS_DIR, "assets");

const GUIDES = [
  {
    slug: "javascript",
    path: "npm/yuku/README.md",
    url: "tree/main/npm/yuku#readme",
    title: "JavaScript",
    description: "Use Yuku from JavaScript and TypeScript with the yuku package.",
  },
  {
    slug: "dialects",
    path: "src/parser/dialect/README.md",
    url: "tree/main/src/parser/dialect#readme",
    title: "Dialects",
    description: "Add your own syntax to JavaScript, TypeScript, and JSX.",
  },
];

const PLAYGROUND_URL = "https://playground.yuku.fyi";

const ORDER = [
  "",
  "parser",
  "parser/ast",
  "parser/semantic",
  "parser/traverse",
  "parser/codegen",
  "testing",
  "security",
  "javascript",
  "dialects",
];

async function main() {
  const site = await readSite();
  const pages = [...(await collectPages(site.host)), ...(await readGuides(site.repo))];
  pages.sort((a, b) => rank(a.slug) - rank(b.slug) || a.slug.localeCompare(b.slug));
  const summary = pages.find((page) => page.slug === "")?.description ?? site.title;

  const index = buildIndex(site, pages, summary);
  const full = buildFull(site, pages, summary);
  await Bun.write(join(ASSETS_DIR, "llms.txt"), index);
  await Bun.write(join(ASSETS_DIR, "llms-full.txt"), full);

  console.log(`llms.txt       ${index.length} bytes`);
  console.log(`llms-full.txt  ${full.length} bytes  (${pages.length} pages)`);
}

async function readSite(): Promise<Site> {
  const ziggy = await Bun.file(join(DOCS_DIR, "zine.ziggy")).text();
  const title = ziggy.match(/\.title\s*=\s*"((?:[^"\\]|\\.)*)"/)?.[1] ?? "Yuku";
  const host = (ziggy.match(/\.host_url\s*=\s*"([^"]+)"/)?.[1] ?? "https://yuku.fyi").replace(
    /\/$/,
    "",
  );

  const manifest = await Bun.file(join(DOCS_DIR, "..", "package.json")).json();
  const repo = (manifest.repository?.url ?? "https://github.com/yuku-toolchain/yuku")
    .replace(/^git\+/, "")
    .replace(/\.git$/, "");

  return { title, host, repo };
}

async function collectPages(host: string): Promise<Page[]> {
  const pages: Page[] = [];
  for (const path of new Glob("**/*.smd").scanSync({ cwd: CONTENT_DIR })) {
    const { data, body } = parseFrontmatter(await Bun.file(join(CONTENT_DIR, path)).text());
    const slug = path
      .replace(/\.smd$/, "")
      .replace(/(^|\/)index$/, "$1")
      .replace(/\/$/, "");
    if (!ORDER.includes(slug)) console.warn(`note: "${slug}" is not in ORDER, appended at the end`);
    pages.push({
      slug,
      url: slug === "" ? `${host}/` : `${host}/${slug}/`,
      title: data.title ?? (slug || "Home"),
      description: data.description ?? "",
      body: cleanBody(body, host),
    });
  }
  return pages;
}

async function readGuides(repo: string): Promise<Page[]> {
  return Promise.all(
    GUIDES.map(async ({ slug, path, url, title, description }) => {
      const markdown = await Bun.file(join(DOCS_DIR, "..", path)).text();
      const body = markdown.replace(/^(<div[\s\S]*?<\/div>|# .*)\s*/, "").trim();
      return { slug, url: `${repo}/${url}`, title, description, body };
    }),
  );
}

function rank(slug: string): number {
  const index = ORDER.indexOf(slug);
  return index === -1 ? ORDER.length : index;
}

function buildIndex(site: Site, pages: Page[], summary: string): string {
  const docs = pages.map(
    (page) => `- [${page.title}](${page.url})${page.description ? `: ${page.description}` : ""}`,
  );
  const resources = [
    `- [Full documentation text](${site.host}/llms-full.txt): every page, concatenated`,
    `- [Playground](${PLAYGROUND_URL})`,
    `- [Source code](${site.repo})`,
  ];
  return [
    `# ${site.title}`,
    "",
    `> ${summary}`,
    "",
    "## Documentation",
    "",
    docs.join("\n"),
    "",
    "## Resources",
    "",
    resources.join("\n"),
    "",
  ].join("\n");
}

function buildFull(site: Site, pages: Page[], summary: string): string {
  const head = [
    `# ${site.title}`,
    "",
    `> ${summary}`,
    `> Full documentation, every page concatenated for LLMs. Index: ${site.host}/llms.txt`,
  ].join("\n");
  const bodies = pages.map((page) =>
    [`# ${page.title}`, "", `Source: ${page.url}`, "", page.body].join("\n"),
  );
  return [head, ...bodies].join("\n\n") + "\n";
}

function parseFrontmatter(source: string): { data: Record<string, string>; body: string } {
  const match = source.match(/^---\r?\n([\s\S]*?)\r?\n---\r?\n?/);
  if (!match) return { data: {}, body: source };

  const data: Record<string, string> = {};
  for (const key of ["title", "description"]) {
    const pattern = new RegExp(`^\\s*\\.${key}\\s*=\\s*"((?:[^"\\\\]|\\\\.)*)"`, "m");
    const field = match[1]!.match(pattern);
    if (field) data[key] = field[1]!.replace(/\\"/g, '"').replace(/\\n/g, " ");
  }
  return { data, body: source.slice(match[0].length) };
}

function cleanBody(markdown: string, host: string): string {
  return (
    markdown
      // "># [Title]($block...)" becomes "> **Title**"
      .replace(/^>#\s*\[([^\]]*)\]\(\$\w+\.\w+\([^)]*\)\)\s*$/gm, "> **$1**")
      // ">[]($block.attrs('note'))" becomes "> **Note**"
      .replace(
        /^>\s*\[\]\(\$\w+\.attrs\(['"]([^'"]+)['"]\)\)\s*$/gm,
        (_, label: string) => `> **${label.charAt(0).toUpperCase()}${label.slice(1)}**`,
      )
      // "[text]($section.id('x'))" becomes "text"
      .replace(/\[([^\]]*)\]\(\$\w+\.\w+\([^)]*\)\)/g, "$1")
      .replace(/\(\$\w+\.\w+\([^)]*\)\)/g, "")
      // root-relative links resolve outside the site
      .replace(/\]\((\/[^)]*)\)/g, `](${host}$1)`)
      .replace(/\n{3,}/g, "\n\n")
      .trim()
  );
}

await main();
