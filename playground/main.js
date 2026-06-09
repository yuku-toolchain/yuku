import { parse } from "@yuku-parser/wasm";
import { print, strip, minify } from "@yuku-codegen/wasm";
import { CodeJar } from "https://esm.sh/codejar@4.2.0";
import hljs from "https://esm.sh/highlight.js@11.10.0/lib/core";
import typescript from "https://esm.sh/highlight.js@11.10.0/lib/languages/typescript";

hljs.registerLanguage("typescript", typescript);

const codegen = { print, strip, minify };
const $ = (id) => document.getElementById(id);

const codeView = $("code");
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
  codeView,
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

let astNodes = [];
let astDetails = [];
const openState = new Map();

function branch(key, value, depth, path) {
  const details = el("details");
  details.__path = path;
  const stored = openState.get(path);
  details.open = stored !== undefined ? stored : depth < OPEN_DEPTH;
  astDetails.push(details);
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
      summary.dataset.start = value.start;
      summary.dataset.end = value.end;
      astNodes.push({ start: value.start, end: value.end, summary });
    }
    entries = Object.entries(value).filter(([k]) => k !== "type" && k !== "start" && k !== "end");
  } else {
    summary.append(el("span", "ast-meta", "{}"));
    entries = Object.entries(value);
  }

  details.append(summary);
  const body = el("div", "ast-body");
  for (const [k, v] of entries) body.append(value_(k, v, depth + 1, `${path}/${k}`));
  details.append(body);
  return details;
}

function value_(key, value, depth, path) {
  if (Array.isArray(value)) {
    return value.length === 0 ? row(key, el("span", "ast-meta", "[]")) : branch(key, value, depth, path);
  }
  if (value !== null && typeof value === "object") {
    return Object.keys(value).length === 0 ? row(key, el("span", "ast-meta", "{}")) : branch(key, value, depth, path);
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
  astNodes = [];
  astDetails = [];
  focused = null;
  lastHit = null;
  clearCodeHighlight();

  let result;
  const t0 = performance.now();
  try {
    result = parse(jar.toString(), options());
  } catch (e) {
    astView.replaceChildren();
    outView.textContent = "";
    setStatus(String(e), "err");
    persist();
    return;
  }
  const t1 = performance.now();

  const astScroll = astView.scrollTop;
  astView.replaceChildren(value_(null, result.program, 0, ""));
  astView.scrollTop = astScroll;

  const t2 = performance.now();
  const outScroll = outView.scrollTop;
  try {
    outView.innerHTML = hl(codegen[op](result.program), "typescript");
  } catch (e) {
    outView.textContent = String(e);
  }
  outView.scrollTop = outScroll;
  const t3 = performance.now();

  const times = `parse ${(t1 - t0).toFixed(2)}ms · ${op} ${(t3 - t2).toFixed(2)}ms`;
  const diags = result.diagnostics;
  if (diags.length) {
    setStatus(`${diags.length} diagnostic${diags.length > 1 ? "s" : ""} · ${times} · ${diags.map((d) => d.message).join("  |  ")}`, "warn");
  } else {
    setStatus(`ok · ${times}`, "ok");
  }

  mapCaretToAst(false, false);
  persist();
}

const HAS_HL = typeof Highlight !== "undefined" && typeof CSS !== "undefined" && !!CSS.highlights;
const srcHighlight = HAS_HL ? new Highlight() : null;
if (srcHighlight) CSS.highlights.set("yuku-src", srcHighlight);

let suppressCaret = false;

function rangeFromOffsets(parent, start, end) {
  const walker = document.createTreeWalker(parent, NodeFilter.SHOW_TEXT);
  const range = document.createRange();
  range.setStart(parent, 0);
  range.setEnd(parent, 0);
  let pos = 0;
  let startSet = false;
  let node;
  while ((node = walker.nextNode())) {
    const len = node.nodeValue.length;
    const next = pos + len;
    if (!startSet && start <= next) {
      range.setStart(node, start - pos);
      startSet = true;
    }
    if (end <= next) {
      range.setEnd(node, Math.min(Math.max(end - pos, 0), len));
      return startSet ? range : null;
    }
    pos = next;
  }
  return null;
}

function caretOffset() {
  const sel = window.getSelection();
  if (!sel || sel.rangeCount === 0) return null;
  const range = sel.getRangeAt(0);
  if (!codeView.contains(range.startContainer)) return null;
  const pre = range.cloneRange();
  pre.selectNodeContents(codeView);
  pre.setEnd(range.startContainer, range.startOffset);
  return pre.toString().length;
}

function highlightCode(start, end) {
  if (!srcHighlight) return;
  srcHighlight.clear();
  const range = rangeFromOffsets(codeView, start, end);
  if (range) srcHighlight.add(range);
}

function clearCodeHighlight() {
  if (srcHighlight) srcHighlight.clear();
}

function selectCode(start, end) {
  const range = rangeFromOffsets(codeView, start, end);
  if (!range) return;
  const sel = window.getSelection();
  suppressCaret = true;
  sel.removeAllRanges();
  sel.addRange(range);
  codeView.focus();
  requestAnimationFrame(() => {
    suppressCaret = false;
  });
}

function scrollCodeIntoView(offset) {
  const range = rangeFromOffsets(codeView, offset, offset);
  let node = range && range.startContainer;
  if (node && node.nodeType === Node.TEXT_NODE) node = node.parentElement;
  if (node && node.scrollIntoView) node.scrollIntoView({ block: "nearest", inline: "nearest" });
}

function focusCode(start, end, scroll) {
  if (srcHighlight) highlightCode(start, end);
  else if (scroll) selectCode(start, end);
  if (scroll) scrollCodeIntoView(start);
}

let focused = null;
let lastHit = null;

function setOpen(details, open) {
  if (details.open !== open) details.open = open;
  openState.set(details.__path, open);
}

function reveal(details) {
  let node = details;
  while (node && node !== astView) {
    if (node.tagName === "DETAILS") setOpen(node, true);
    node = node.parentElement;
  }
}

function isolate(target) {
  for (const d of astDetails) setOpen(d, false);
  for (const d of target.querySelectorAll("details")) setOpen(d, true);
  reveal(target);
}

function removeHit() {
  if (lastHit) {
    lastHit.classList.remove("ast-hit");
    lastHit = null;
  }
  focused = null;
}

function focusNode(summary, scroll, collapseRest) {
  const details = summary.parentElement;
  if (lastHit !== details) {
    if (lastHit) lastHit.classList.remove("ast-hit");
    lastHit = details;
    details.classList.add("ast-hit");
  }
  focused = summary;
  if (collapseRest) isolate(details);
  else reveal(details);
  if (scroll) summary.scrollIntoView({ block: "nearest" });
}

function nodeAt(offset) {
  let best = null;
  for (const n of astNodes) {
    if (offset >= n.start && offset <= n.end) {
      if (!best || n.end - n.start <= best.end - best.start) best = n;
    }
  }
  return best;
}

function mapCaretToAst(scroll, collapseRest) {
  if (suppressCaret) return;
  const offset = caretOffset();
  if (offset == null) return;
  const hit = nodeAt(offset);
  if (hit) focusNode(hit.summary, scroll, collapseRest);
  else removeHit();
}

let caretFrame;
document.addEventListener("selectionchange", () => {
  cancelAnimationFrame(caretFrame);
  caretFrame = requestAnimationFrame(() => mapCaretToAst(true, false));
});

codeView.addEventListener("click", () => mapCaretToAst(true, true));
codeView.addEventListener("blur", removeHit);

astView.addEventListener(
  "toggle",
  (e) => {
    if (e.target.__path !== undefined) openState.set(e.target.__path, e.target.open);
  },
  true,
);

astView.addEventListener("mouseover", (e) => {
  const summary = e.target.closest("summary");
  if (!summary || !astView.contains(summary) || summary.dataset.start === undefined) return;
  highlightCode(+summary.dataset.start, +summary.dataset.end);
});

astView.addEventListener("mouseleave", clearCodeHighlight);

astView.addEventListener("click", (e) => {
  const summary = e.target.closest("summary");
  if (!summary || summary.dataset.start === undefined) return;
  focusCode(+summary.dataset.start, +summary.dataset.end, true);
});

const STATE_KEY = "yuku-state";
const CONTROLS = ["lang", "sourceType", "preserveParens", "allowReturnOutsideFunction", "semanticErrors", "attachComments"];

function snapshot() {
  const s = { code: jar.toString(), op, theme: document.documentElement.dataset.theme };
  for (const id of CONTROLS) {
    const c = $(id);
    s[id] = c.type === "checkbox" ? c.checked : c.value;
  }
  return s;
}

function encodeState(s) {
  const bytes = new TextEncoder().encode(JSON.stringify(s));
  let bin = "";
  for (let i = 0; i < bytes.length; i += 0x8000) {
    bin += String.fromCharCode.apply(null, bytes.subarray(i, i + 0x8000));
  }
  return btoa(bin).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

function decodeState(str) {
  if (!str) return null;
  try {
    const bin = atob(str.replace(/-/g, "+").replace(/_/g, "/"));
    const bytes = Uint8Array.from(bin, (c) => c.charCodeAt(0));
    return JSON.parse(new TextDecoder().decode(bytes));
  } catch {
    return null;
  }
}

function loadState() {
  const fromHash = decodeState(location.hash.slice(1));
  if (fromHash) return fromHash;
  try {
    return JSON.parse(localStorage.getItem(STATE_KEY));
  } catch {
    return null;
  }
}

let persistTimer;
let lastHash = "";
function persist() {
  clearTimeout(persistTimer);
  persistTimer = setTimeout(() => {
    const s = snapshot();
    try {
      localStorage.setItem(STATE_KEY, JSON.stringify(s));
    } catch {}
    const hash = "#" + encodeState(s);
    if (hash === lastHash) return;
    lastHash = hash;
    try {
      history.replaceState(null, "", hash);
    } catch {}
  }, 250);
}

function applyState(s) {
  if (!s) return;
  for (const id of CONTROLS) {
    if (s[id] === undefined) continue;
    const c = $(id);
    if (c.type === "checkbox") c.checked = !!s[id];
    else c.value = s[id];
  }
  if (s.op && codegen[s.op]) {
    op = s.op;
    for (const b of document.querySelectorAll(".tabs button")) {
      b.classList.toggle("active", b.dataset.op === op);
    }
  }
  if (s.theme === "dark" || s.theme === "light") {
    document.documentElement.dataset.theme = s.theme;
    localStorage.setItem("yuku-theme", s.theme);
  }
}

async function showVersion() {
  const el = $("version");
  try {
    const res = await fetch(el.dataset.pkg);
    const match = res.headers.get("x-esm-path")?.match(/@(\d[^/]*)/);
    if (match) {
      el.textContent = match[1];
      return;
    }
    const pkgUrl = new URL("./package.json", new URL(el.dataset.pkg, location.href));
    const { version } = await (await fetch(pkgUrl)).json();
    if (version) el.textContent = version;
  } catch {}
}

showVersion();

const themeBtn = $("theme");
function labelTheme() {
  themeBtn.textContent = document.documentElement.dataset.theme === "dark" ? "light" : "dark";
}
themeBtn.addEventListener("click", () => {
  const next = document.documentElement.dataset.theme === "dark" ? "light" : "dark";
  document.documentElement.dataset.theme = next;
  localStorage.setItem("yuku-theme", next);
  labelTheme();
  persist();
});
labelTheme();

jar.onUpdate(render);

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

const initial = loadState();
applyState(initial);
labelTheme();
jar.updateCode(initial && typeof initial.code === "string" ? initial.code : SAMPLE);
render();
