import { Glob } from "bun";
import { join } from "node:path";

const DOCS_DIR = import.meta.dir;
const CONTENT_DIR = join(DOCS_DIR, "content");
const ASSETS_DIR = join(DOCS_DIR, "assets");

const PLAYGROUND_URL = "https://playground.yuku.fyi";

const ORDER = [
  "",
  "parser",
  "parser/ast",
  "parser/semantic",
  "parser/traverse",
  "parser/codegen",
  "analyzer",
  "testing",
];

interface Page {
  slug: string; // "" for the home page, else "parser", "parser/ast", ...
  url: string;
  title: string;
  description: string;
  body: string; // cleaned markdown
}

async function main() {
  const site = await readSite();

  const pages = await collectPages(site.host);
  pages.sort((a, b) => rank(a.slug) - rank(b.slug) || a.slug.localeCompare(b.slug));

  const home = pages.find((p) => p.slug === "");
  const summary = home?.description ?? site.title;

  const index = buildIndex(site, pages, summary);
  const full = buildFull(site, pages, summary);

  await Bun.write(join(ASSETS_DIR, "llms.txt"), index);
  await Bun.write(join(ASSETS_DIR, "llms-full.txt"), full);

  console.log(`llms.txt       ${index.length} bytes`);
  console.log(`llms-full.txt  ${full.length} bytes  (${pages.length} pages)`);
}

async function collectPages(host: string): Promise<Page[]> {
  const pages: Page[] = [];
  for (const rel of new Glob("**/*.smd").scanSync({ cwd: CONTENT_DIR })) {
    const raw = await Bun.file(join(CONTENT_DIR, rel)).text();
    const { data, body } = parseFrontmatter(raw);
    const slug = slugOf(rel);
    pages.push({
      slug,
      url: urlOf(host, slug),
      title: data.title ?? (slug || "Home"),
      description: data.description ?? "",
      body: cleanBody(body, host),
    });
  }
  return pages;
}

function rank(slug: string): number {
  const i = ORDER.indexOf(slug);
  if (i === -1) console.warn(`note: "${slug}" is not in ORDER, appended at the end`);
  return i === -1 ? ORDER.length : i;
}

function buildIndex(site: Site, pages: Page[], summary: string): string {
  const docs = pages.map(
    (p) => `- [${p.title}](${p.url})${p.description ? `: ${p.description}` : ""}`,
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

  const body = pages.map((p) => [`# ${p.title}`, "", `Source: ${p.url}`, "", p.body].join("\n"));

  return [head, ...body].join("\n\n") + "\n";
}

function parseFrontmatter(raw: string): { data: Record<string, string>; body: string } {
  const match = raw.match(/^---\r?\n([\s\S]*?)\r?\n---\r?\n?/);
  if (!match) return { data: {}, body: raw };

  const fm = match[1];
  const data: Record<string, string> = {};
  for (const key of ["title", "description"]) {
    const m = fm.match(new RegExp(`^\\s*\\.${key}\\s*=\\s*"((?:[^"\\\\]|\\\\.)*)"`, "m"));
    if (m) data[key] = m[1].replace(/\\"/g, '"').replace(/\\n/g, " ");
  }
  return { data, body: raw.slice(match[0].length) };
}

function cleanBody(md: string, host: string): string {
  return (
    md
      // titled callout heading: "># [Title]($block...)" -> "> **Title**"
      .replace(/^>#\s*\[([^\]]*)\]\(\$\w+\.\w+\([^)]*\)\)\s*$/gm, "> **$1**")
      // empty callout marker: ">[]($block.attrs('note'))" -> "> **Note**"
      .replace(/^>\s*\[\]\(\$\w+\.attrs\(['"]([^'"]+)['"]\)\)\s*$/gm, (_, l) => `> **${cap(l)}**`)
      // any remaining "[text]($section.id('x'))" -> "text" (heading anchors, inline)
      .replace(/\[([^\]]*)\]\(\$\w+\.\w+\([^)]*\)\)/g, "$1")
      // any orphan directive with no link text
      .replace(/\(\$\w+\.\w+\([^)]*\)\)/g, "")
      // make root-relative links absolute so they resolve outside the site
      .replace(/\]\((\/[^)]*)\)/g, `](${host}$1)`)
      // collapse runs of blank lines
      .replace(/\n{3,}/g, "\n\n")
      .trim()
  );
}

interface Site {
  title: string;
  host: string;
  repo: string;
}

async function readSite(): Promise<Site> {
  const ziggy = await Bun.file(join(DOCS_DIR, "zine.ziggy")).text();
  const title = ziggy.match(/\.title\s*=\s*"((?:[^"\\]|\\.)*)"/)?.[1] ?? "Yuku";
  const host = (ziggy.match(/\.host_url\s*=\s*"([^"]+)"/)?.[1] ?? "https://yuku.fyi").replace(
    /\/$/,
    "",
  );

  const pkg = await Bun.file(join(DOCS_DIR, "..", "package.json")).json();
  const repo = (pkg.repository?.url ?? "https://github.com/yuku-toolchain/yuku")
    .replace(/^git\+/, "")
    .replace(/\.git$/, "");

  return { title, host, repo };
}

function slugOf(rel: string): string {
  return rel
    .replace(/\.smd$/, "")
    .replace(/(^|\/)index$/, "$1")
    .replace(/\/$/, "");
}

function urlOf(host: string, slug: string): string {
  return slug === "" ? `${host}/` : `${host}/${slug}/`;
}

function cap(s: string): string {
  return s.charAt(0).toUpperCase() + s.slice(1);
}

await main();
