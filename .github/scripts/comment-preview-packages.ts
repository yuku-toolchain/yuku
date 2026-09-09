import { readFileSync } from "node:fs";

interface PreviewPackage {
  name: string;
  url: string;
}

interface PreviewMetadata {
  packages: PreviewPackage[];
}

const MARKER = "<!-- preview-packages -->";

const { GITHUB_TOKEN, GITHUB_REPOSITORY, PR_NUMBER, METADATA_PATH } = process.env;

if (!PR_NUMBER) {
  console.log("not a pull request, skipping the preview comment");
  process.exit(0);
}

const metadata = JSON.parse(readFileSync(METADATA_PATH!, "utf8")) as PreviewMetadata;
const packages = metadata.packages.filter((pkg) => !pkg.name.includes("/binding-"));

if (packages.length === 0) {
  console.error(`no publishable packages in ${METADATA_PATH}`);
  process.exit(1);
}

const wasm = packages.find((pkg) => pkg.name === "@yuku-parser/wasm");
const sha = wasm?.url.split("@").pop();
const playground = sha ? `https://playground.yuku.fyi/?pr=${sha}` : null;

const body = [
  MARKER,
  "### Preview packages",
  "",
  ...(playground
    ? [`Try this PR in the [playground](${playground}).`, ""]
    : []),
  "| Package | Install |",
  "| --- | --- |",
  ...packages
    .sort((a, b) => a.name.localeCompare(b.name))
    .map((pkg) => `| \`${pkg.name}\` | \`npm i ${pkg.url}\` |`),
].join("\n");

const api = `https://api.github.com/repos/${GITHUB_REPOSITORY}`;
const headers = {
  authorization: `Bearer ${GITHUB_TOKEN}`,
  accept: "application/vnd.github+json",
  "content-type": "application/json",
};

const listed = await fetch(`${api}/issues/${PR_NUMBER}/comments?per_page=100`, { headers });
if (!listed.ok) {
  console.error(`cannot list comments (${listed.status}): ${await listed.text()}`);
  process.exit(1);
}

const existing = ((await listed.json()) as { id: number; body: string }[]).find((comment) =>
  comment.body.startsWith(MARKER),
);

const target = existing ? `${api}/issues/comments/${existing.id}` : `${api}/issues/${PR_NUMBER}/comments`;
const posted = await fetch(target, {
  method: existing ? "PATCH" : "POST",
  headers,
  body: JSON.stringify({ body }),
});

if (!posted.ok) {
  console.error(`cannot post comment (${posted.status}): ${await posted.text()}`);
  process.exit(1);
}

console.log(`${existing ? "updated" : "created"} the preview comment with ${packages.length} packages`);
