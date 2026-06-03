import { parse } from "/pkg/yuku-parser-wasm/index.js";
import { print, strip, minify } from "/pkg/yuku-codegen-wasm/index.js";
import { CodeJar } from "https://esm.sh/codejar@4.2.0";
import hljs from "https://esm.sh/highlight.js@11.10.0/lib/core";
import typescript from "https://esm.sh/highlight.js@11.10.0/lib/languages/typescript";

hljs.registerLanguage("typescript", typescript);

const codegen = { print, strip, minify };
const $ = (id) => document.getElementById(id);

const astView = $("ast");
const outView = $("out");
const status = $("status");

let op = "print";

const OPEN_DEPTH = 4;

const SAMPLE = [
  "interface User {",
  "  id: number",
  "  name: string",
  "}",
  "",
  "export const greet = (u: User): string => {",
  "  return `hello ${u.name}`",
  "}",
  "",
].join("\n");

function hl(text, lang) {
  return hljs.highlight(text, { language: lang }).value;
}

const jar = CodeJar(
  $("code"),
  (el) => {
    el.innerHTML = hl(el.textContent, "typescript");
  },
  { tab: "  ", spellcheck: false },
);

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

function isNode(value) {
  return value !== null && typeof value === "object" && typeof value.type === "string";
}

function leafSpan(value) {
  if (typeof value === "string") return el("span", "ast-str", JSON.stringify(value));
  if (typeof value === "number") return el("span", "ast-num", String(value));
  if (typeof value === "bigint") return el("span", "ast-num", `${value}n`);
  if (typeof value === "boolean") return el("span", "ast-bool", String(value));
  return el("span", "ast-null", "null");
}

function row(key, valueNode) {
  const r = el("div", "ast-row");
  if (key !== null) r.append(el("span", "ast-key", `${key}: `));
  r.append(valueNode);
  return r;
}

function branch(key, value, depth) {
  const details = el("details");
  details.open = depth < OPEN_DEPTH;
  const summary = el("summary");
  if (key !== null) summary.append(el("span", "ast-key", `${key}: `));

  let entries;
  if (Array.isArray(value)) {
    summary.append(el("span", "ast-meta", `[${value.length}]`));
    entries = value.map((item, i) => [String(i), item]);
  } else if (isNode(value)) {
    summary.append(el("span", "ast-type", value.type));
    if (typeof value.start === "number" && typeof value.end === "number") {
      summary.append(el("span", "ast-span", ` ${value.start}:${value.end}`));
    }
    entries = Object.entries(value).filter(([k]) => k !== "type" && k !== "start" && k !== "end");
  } else {
    summary.append(el("span", "ast-meta", "{}"));
    entries = Object.entries(value);
  }

  details.append(summary);
  const body = el("div", "ast-body");
  for (const [k, v] of entries) body.append(value_(k, v, depth + 1));
  details.append(body);
  return details;
}

function value_(key, value, depth) {
  if (Array.isArray(value)) {
    return value.length === 0 ? row(key, el("span", "ast-meta", "[]")) : branch(key, value, depth);
  }
  if (value !== null && typeof value === "object") {
    return Object.keys(value).length === 0 ? row(key, el("span", "ast-meta", "{}")) : branch(key, value, depth);
  }
  return row(key, leafSpan(value));
}

function options() {
  return {
    lang: $("lang").value,
    sourceType: $("sourceType").value,
    preserveParens: $("preserveParens").checked,
    allowReturnOutsideFunction: $("allowReturnOutsideFunction").checked,
    semanticErrors: $("semanticErrors").checked,
    attachComments: $("attachComments").checked,
  };
}

function setStatus(text, kind) {
  status.textContent = text;
  status.className = kind;
}

function render() {
  let result;
  const t0 = performance.now();
  try {
    result = parse(jar.toString(), options());
  } catch (e) {
    astView.replaceChildren();
    outView.textContent = "";
    setStatus(String(e), "err");
    return;
  }
  const t1 = performance.now();

  astView.replaceChildren(value_(null, result.program, 0));

  const t2 = performance.now();
  try {
    outView.innerHTML = hl(codegen[op](result.program), "typescript");
  } catch (e) {
    outView.textContent = String(e);
  }
  const t3 = performance.now();

  const times = `parse ${(t1 - t0).toFixed(2)}ms · ${op} ${(t3 - t2).toFixed(2)}ms`;
  const diags = result.diagnostics;
  if (diags.length) {
    setStatus(`${diags.length} diagnostic${diags.length > 1 ? "s" : ""} · ${times} · ${diags.map((d) => d.message).join("  |  ")}`, "warn");
  } else {
    setStatus(`ok · ${times}`, "ok");
  }
}

const themeBtn = $("theme");
function labelTheme() {
  themeBtn.textContent = document.documentElement.dataset.theme === "dark" ? "light" : "dark";
}
themeBtn.addEventListener("click", () => {
  const next = document.documentElement.dataset.theme === "dark" ? "light" : "dark";
  document.documentElement.dataset.theme = next;
  localStorage.setItem("yuku-theme", next);
  labelTheme();
});
labelTheme();

let timer;
jar.onUpdate(() => {
  clearTimeout(timer);
  timer = setTimeout(render, 150);
});

for (const control of document.querySelectorAll("header select, header input")) {
  control.addEventListener("change", render);
}

for (const btn of document.querySelectorAll(".tabs button")) {
  btn.addEventListener("click", () => {
    op = btn.dataset.op;
    for (const b of document.querySelectorAll(".tabs button")) {
      b.classList.toggle("active", b === btn);
    }
    render();
  });
}

jar.updateCode(SAMPLE);
render();
