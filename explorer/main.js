import { parse } from "/pkg/yuku-parser-wasm/index.js";
import { print, strip, minify } from "/pkg/yuku-codegen-wasm/index.js";
import { CodeJar } from "https://esm.sh/codejar@4.2.0";
import hljs from "https://esm.sh/highlight.js@11.10.0/lib/core";
import typescript from "https://esm.sh/highlight.js@11.10.0/lib/languages/typescript";
import json from "https://esm.sh/highlight.js@11.10.0/lib/languages/json";

hljs.registerLanguage("typescript", typescript);
hljs.registerLanguage("json", json);

const codegen = { print, strip, minify };
const $ = (id) => document.getElementById(id);

const astView = $("ast");
const outView = $("out");
const status = $("status");

let op = "print";

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

function replacer(_key, value) {
  return typeof value === "bigint" ? `${value}n` : value;
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
    astView.textContent = "";
    outView.textContent = "";
    setStatus(String(e), "err");
    return;
  }
  const t1 = performance.now();

  astView.innerHTML = hl(JSON.stringify(result.program, replacer, 2), "json");

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

for (const el of document.querySelectorAll("header select, header input")) {
  el.addEventListener("change", render);
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
