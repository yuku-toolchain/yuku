import { readFileSync } from "node:fs";

interface PreviewPackage {
  name: string;
  url: string;
}

interface PreviewMetadata {
  packages: PreviewPackage[];
}

interface Deployment {
  environment: string;
  statuses_url: string;
}

interface DeploymentStatus {
  state: string;
  environment_url?: string;
}

interface IssueComment {
  id: number;
  body: string;
}

const MARKER = "<!-- preview-packages -->";

const { GITHUB_TOKEN, GITHUB_REPOSITORY, PR_NUMBER, METADATA_PATH, HEAD_SHA } = process.env;

if (!PR_NUMBER) {
  console.log("not a pull request, skipping the preview comment");
  process.exit(0);
}

const { packages } = JSON.parse(readFileSync(METADATA_PATH!, "utf8")) as PreviewMetadata;

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

async function previewPlayground(): Promise<string | null> {
  if (!HEAD_SHA) return null;
  for (let attempt = 0; attempt < 6; attempt++) {
    const response = await fetch(`${api}/deployments?sha=${HEAD_SHA}`, { headers });
    if (response.ok) {
      for (const deployment of (await response.json()) as Deployment[]) {
        if (!deployment.environment.startsWith("Preview")) continue;
        const statuses = await fetch(deployment.statuses_url, { headers });
        if (!statuses.ok) continue;
        const done = ((await statuses.json()) as DeploymentStatus[]).find(
          (status) => status.state === "success" && status.environment_url,
        );
        if (done) return done.environment_url!;
      }
    }
    await new Promise((resolve) => setTimeout(resolve, 10_000));
  }
  return null;
}

const yuku = packages.find((entry) => entry.name === "yuku");
const sha = yuku?.url.split("@").pop();
const origin = (await previewPlayground()) ?? "https://playground.yuku.fyi";
const playground = sha ? `${origin}/?pr=${sha}` : null;

const body = [
  MARKER,
  "### Preview package",
  "",
  ...(playground ? [`Try this PR in the [playground](${playground}).`, ""] : []),
  ...(yuku ? [`\`npm i ${yuku.url}\``] : []),
].join("\n");

const listed = await fetch(`${api}/issues/${PR_NUMBER}/comments?per_page=100`, { headers });
if (!listed.ok) {
  console.error(`cannot list comments (${listed.status}): ${await listed.text()}`);
  process.exit(1);
}

const existing = ((await listed.json()) as IssueComment[]).find((comment) =>
  comment.body.startsWith(MARKER),
);

const posted = await fetch(
  existing ? `${api}/issues/comments/${existing.id}` : `${api}/issues/${PR_NUMBER}/comments`,
  { method: existing ? "PATCH" : "POST", headers, body: JSON.stringify({ body }) },
);

if (!posted.ok) {
  console.error(`cannot post comment (${posted.status}): ${await posted.text()}`);
  process.exit(1);
}

const action = existing ? "updated" : "created";
console.log(`${action} the preview comment`);
