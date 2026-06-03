import { parse } from "/pkg/yuku-parser-wasm/index.js";
import { print, strip, minify } from "/pkg/yuku-codegen-wasm/index.js";

const codegen = { print, strip, minify };
const $ = (id) => document.getElementById(id);

const code = $("code");
const astView = $("ast");
const outView = $("out");
const status = $("status");

let op = "print";

code.value = [
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
    result = parse(code.value, options());
  } catch (e) {
    astView.textContent = "";
    outView.textContent = "";
    setStatus(String(e), "err");
    return;
  }
  const t1 = performance.now();

  astView.textContent = JSON.stringify(result.program, replacer, 2);

  const t2 = performance.now();
  try {
    outView.textContent = codegen[op](result.program);
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
code.addEventListener("input", () => {
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

render();
