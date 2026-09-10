import { readFileSync } from "node:fs";

interface PreviewPackage {
  name: string;
  url: string;
}

interface PreviewMetadata {
  packages: PreviewPackage[];
}

const MARKER = "<!-- preview-packages -->";

const { GITHUB_TOKEN, GITHUB_REPOSITORY, PR_NUMBER, METADATA_PATH, HEAD_SHA } = process.env;

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

const api = `https://api.github.com/repos/${GITHUB_REPOSITORY}`;
const headers = {
  authorization: `Bearer ${GITHUB_TOKEN}`,
  accept: "application/vnd.github+json",
  "content-type": "application/json",
};

// the playground preview Vercel deployed for this commit, recorded as a GitHub deployment
async function previewPlayground(): Promise<string | null> {
  if (!HEAD_SHA) return null;
  for (let attempt = 0; attempt < 6; attempt++) {
    const res = await fetch(`${api}/deployments?sha=${HEAD_SHA}`, { headers });
    if (res.ok) {
      const deployments = (await res.json()) as { environment: string; statuses_url: string }[];
      for (const deployment of deployments) {
        if (!deployment.environment.startsWith("Preview")) continue;
        const statuses = await fetch(deployment.statuses_url, { headers });
        if (!statuses.ok) continue;
        const done = ((await statuses.json()) as { state: string; environment_url?: string }[]).find(
          (status) => status.state === "success" && status.environment_url,
        );
        if (done) return done.environment_url!;
      }
    }
    await new Promise((resolve) => setTimeout(resolve, 10_000));
  }
  return null;
}

const wasm = packages.find((pkg) => pkg.name === "@yuku-parser/wasm");
const sha = wasm?.url.split("@").pop();
const origin = (await previewPlayground()) ?? "https://playground.yuku.fyi";
const playground = sha ? `${origin}/?pr=${sha}` : null;

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
