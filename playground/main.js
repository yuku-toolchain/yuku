import { parse } from "@yuku-parser/wasm";
import { generate } from "@yuku-codegen/wasm";
import { analyze, SymbolFlags } from "@yuku-analyzer/wasm";
import { CodeJar } from "https://esm.sh/codejar@4.2.0";
import hljs from "https://esm.sh/highlight.js@11.10.0/lib/core";
import typescript from "https://esm.sh/highlight.js@11.10.0/lib/languages/typescript";

hljs.registerLanguage("typescript", typescript);

const $ = (id) => document.getElementById(id);

const codeView = $("code");
const astView = $("ast");
const outView = $("out");
const status = $("status");

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

const FLAG_BADGES = [
  ["Const", "const"],
  ["BlockScopedVariable", "let"],
  ["FunctionScopedVariable", "var"],
  ["Function", "function"],
  ["Class", "class"],
  ["Parameter", "param"],
  ["CatchVariable", "catch"],
  ["ValueImport", "import"],
  ["TypeImport", "type-import"],
  ["Interface", "interface"],
  ["TypeAlias", "type"],
  ["TypeParameter", "type-param"],
  ["RegularEnum", "enum"],
  ["ConstEnum", "const-enum"],
  ["EnumMember", "member"],
  ["NamespaceModule", "namespace"],
  ["Exported", "exported"],
  ["Default", "default"],
];

function badges(sym) {
  const out = [];
  for (const [flag, label] of FLAG_BADGES) if (sym.has(SymbolFlags[flag])) out.push(label);
  const i = out.indexOf("let");
  if (i >= 0 && out.includes("const")) out.splice(i, 1);
  return out;
}

function spansOf(sym) {
  return {
    decl: sym.declarations.map((d) => [d.start, d.end]),
    refs: sym.references.map((r) => [r.node.start, r.node.end]),
  };
}

function siteRow(label, badgeList, start, end, isDecl) {
  const span = [[start, end]];
  const r = el("div", "sem-sym sem-site");
  r.append(el("span", "ast-meta", label));
  for (const b of badgeList) r.append(el("span", "sem-flag", b));
  r.append(el("span", "ast-span", `${start}:${end}`));
  r.__spans = { decl: isDecl ? span : [], refs: isDecl ? [] : span };
  r.__focus = [start, end];
  return r;
}

function refBadges(ref) {
  const out = [ref.isWrite ? "write" : "read"];
  if (ref.space !== "value") out.push(ref.space);
  if (ref.inTypePosition && ref.space === "value") out.push("type-pos");
  return out;
}

// a symbol (or unresolved-name group): summary hover lights every site
// at once, the expanded per-site rows light one at a time
function symNode(name, badgeList, spans, sites, path) {
  const details = el("details");
  details.__path = path;
  details.open = openState.get(path) ?? false;
  const summary = el("summary", "sem-sym");
  summary.append(el("span", "sem-name", name));
  for (const b of badgeList) summary.append(el("span", "sem-flag", b));
  summary.append(el("span", "ast-meta", `${spans.refs.length} ref${spans.refs.length === 1 ? "" : "s"}`));
  summary.__spans = spans;
  summary.__focus = spans.decl[0] ?? spans.refs[0];
  details.append(summary);
  const body = el("div", "ast-body");
  for (const row of sites) body.append(row);
  details.append(body);
  return details;
}

function symSites(sym) {
  return [
    ...sym.declarations.map((d) => siteRow("decl", [], d.start, d.end, true)),
    ...sym.references.map((r) => siteRow("ref", refBadges(r), r.node.start, r.node.end, false)),
  ];
}

function scopeNode(scope, kids, depth, path) {
  const details = el("details");
  details.__path = path;
  details.open = openState.get(path) ?? depth < OPEN_DEPTH;
  const summary = el("summary");
  summary.append(el("span", "ast-type", scope.kind));
  if (scope.strict) summary.append(el("span", "sem-flag", "strict"));
  const n = scope.node;
  if (n && typeof n.start === "number") {
    summary.append(el("span", "ast-span", ` ${n.start}:${n.end}`));
    summary.dataset.start = n.start;
    summary.dataset.end = n.end;
  }
  details.append(summary);
  const body = el("div", "ast-body");
  for (const sym of scope.bindings) {
    body.append(symNode(sym.name, badges(sym), spansOf(sym), symSites(sym), `${path}/s${sym.id}`));
  }
  for (const child of kids[scope.id]) body.append(scopeNode(child, kids, depth + 1, `${path}/${child.id}`));
  details.append(body);
  return details;
}

function section(root, title, count, path) {
  const details = el("details");
  details.__path = path;
  details.open = openState.get(path) ?? true;
  const summary = el("summary");
  summary.append(el("span", "ast-type", title));
  summary.append(el("span", "ast-meta", ` [${count}]`));
  details.append(summary);
  const body = el("div", "ast-body");
  details.append(body);
  root.append(details);
  return body;
}

function recordRow(label, badgeList, specifier, spans, focus) {
  const r = el("div", "sem-sym");
  r.append(el("span", "sem-name", label));
  for (const b of badgeList) r.append(el("span", "sem-flag", b));
  if (specifier !== null) r.append(el("span", "ast-str", JSON.stringify(specifier)));
  r.__spans = spans;
  r.__focus = focus;
  return r;
}

function importRow(imp) {
  const badgeList = [];
  if (imp.isNamespace) badgeList.push("namespace");
  if (imp.isSideEffect) badgeList.push("side-effect");
  if (imp.typeOnly) badgeList.push("type-only");
  if (imp.phase) badgeList.push(imp.phase);
  let label = imp.local ? imp.local.name : "(side effect)";
  if (imp.name && imp.local && imp.name !== imp.local.name) label += ` ← ${imp.name}`;
  const spans = imp.local ? spansOf(imp.local) : { decl: [[imp.node.start, imp.node.end]], refs: [] };
  return recordRow(label, badgeList, imp.specifier, spans, [imp.node.start, imp.node.end]);
}

function exportRow(ex) {
  const badgeList = [];
  if (ex.isStar) badgeList.push("star");
  if (ex.isNamespaceReexport && !ex.isStar) badgeList.push("namespace");
  if (ex.isExportEquals) badgeList.push("export=");
  if (ex.typeOnly) badgeList.push("type-only");
  let label = ex.name ?? (ex.isExportEquals ? "export =" : "*");
  if (ex.globalName) label = `as namespace ${ex.globalName}`;
  if (ex.fromName && ex.fromName !== ex.name) label += ` ← ${ex.fromName}`;
  const spans = ex.local ? spansOf(ex.local) : { decl: [[ex.node.start, ex.node.end]], refs: [] };
  return recordRow(label, badgeList, ex.specifier, spans, [ex.node.start, ex.node.end]);
}

function semTree(m) {
  const kids = m.scopes.map(() => []);
  for (const s of m.scopes) if (s.parent) kids[s.parent.id].push(s);
  const root = el("div");
  root.append(scopeNode(m.rootScope, kids, 0, "sem"));

  if (m.imports.length) {
    const body = section(root, "imports", m.imports.length, "sem/imports");
    for (const imp of m.imports) body.append(importRow(imp));
  }
  if (m.exports.length) {
    const body = section(root, "exports", m.exports.length, "sem/exports");
    for (const ex of m.exports) body.append(exportRow(ex));
  }
  if (m.unresolvedReferences.length) {
    const body = section(root, "unresolved", m.unresolvedReferences.length, "sem/unresolved");
    const byName = new Map();
    for (const r of m.unresolvedReferences) {
      if (!byName.has(r.name)) byName.set(r.name, []);
      byName.get(r.name).push(r);
    }
    for (const [name, list] of byName) {
      const spans = { decl: [], refs: list.map((r) => [r.node.start, r.node.end]) };
      const sites = list.map((r) => siteRow("ref", refBadges(r), r.node.start, r.node.end, false));
      body.append(symNode(name, ["global"], spans, sites, `sem/unresolved/${name}`));
    }
  }
  return root;
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
  $("paneTitle").textContent = $("view").value;
  if ($("view").value === "semantics") {
    try {
      astView.replaceChildren(semTree(analyze(jar.toString(), options())));
    } catch (e) {
      astView.replaceChildren(el("div", "sem-err", String(e)));
    }
  } else {
    astView.replaceChildren(value_(null, result.program, 0, ""));
  }
  astView.scrollTop = astScroll;

  const t2 = performance.now();
  const outScroll = outView.scrollTop;
  try {
    const out = generate(result.program, {
      strip: $("strip").checked,
      minify: { syntax: $("minify").checked },
      format: $("format").value,
      quotes: $("quotes").value,
      comments: $("comments").value,
      indent: +$("indent").value,
    });
    outView.innerHTML = hl(out, "typescript");
  } catch (e) {
    outView.textContent = String(e);
  }
  outView.scrollTop = outScroll;
  const t3 = performance.now();

  const times = `parse ${(t1 - t0).toFixed(2)}ms · codegen ${(t3 - t2).toFixed(2)}ms`;
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
const refHighlight = HAS_HL ? new Highlight() : null;
if (refHighlight) CSS.highlights.set("yuku-ref", refHighlight);
const declHighlight = HAS_HL ? new Highlight() : null;
if (declHighlight) CSS.highlights.set("yuku-decl", declHighlight);

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
  clearCodeHighlight();
  const range = rangeFromOffsets(codeView, start, end);
  if (range) srcHighlight.add(range);
}

function clearCodeHighlight() {
  if (srcHighlight) srcHighlight.clear();
  if (refHighlight) refHighlight.clear();
  if (declHighlight) declHighlight.clear();
}

function highlightSpans({ decl, refs }) {
  if (!srcHighlight) return;
  clearCodeHighlight();
  for (const [s, e] of decl) {
    const r = rangeFromOffsets(codeView, s, e);
    if (r) declHighlight.add(r);
  }
  for (const [s, e] of refs) {
    const r = rangeFromOffsets(codeView, s, e);
    if (r) refHighlight.add(r);
  }
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

function scrollCodeIntoView(start, end) {
  const range = rangeFromOffsets(codeView, start, end);
  if (!range) return;
  const rect = range.getBoundingClientRect();
  const view = codeView.getBoundingClientRect();
  if (rect.top < view.top || rect.bottom > view.bottom) {
    codeView.scrollTop += rect.top - view.top - codeView.clientHeight / 2;
  }
  if (rect.left < view.left || rect.right > view.right) {
    codeView.scrollLeft += rect.left - view.left - codeView.clientWidth / 2;
  }
}

function focusCode(start, end, scroll) {
  if (srcHighlight) highlightCode(start, end);
  else if (scroll) selectCode(start, end);
  if (scroll) scrollCodeIntoView(start, end);
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
  const sym = e.target.closest(".sem-sym");
  if (sym && sym.__spans) return highlightSpans(sym.__spans);
  const summary = e.target.closest("summary");
  if (summary && astView.contains(summary) && summary.dataset.start !== undefined) {
    return highlightCode(+summary.dataset.start, +summary.dataset.end);
  }
  clearCodeHighlight();
});

astView.addEventListener("mouseleave", clearCodeHighlight);

astView.addEventListener("click", (e) => {
  const sym = e.target.closest(".sem-sym");
  if (sym && sym.__focus) return focusCode(sym.__focus[0], sym.__focus[1], true);
  const summary = e.target.closest("summary");
  if (!summary || summary.dataset.start === undefined) return;
  focusCode(+summary.dataset.start, +summary.dataset.end, true);
});

const STATE_KEY = "yuku-state";
const CONTROLS = ["lang", "sourceType", "preserveParens", "allowReturnOutsideFunction", "semanticErrors", "attachComments", "strip", "minify", "format", "quotes", "comments", "indent", "view"];

function snapshot() {
  const s = { code: jar.toString(), theme: document.documentElement.dataset.theme };
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
  if (s.op === "strip" || s.op === "minify") $(s.op).checked = true;
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

// registered before the render listeners so render sees the synced values
$("minify").addEventListener("change", (e) => {
  $("format").value = e.target.checked ? "compact" : "pretty";
  $("quotes").value = e.target.checked ? "shortest" : "preserve";
});

for (const control of document.querySelectorAll("header select, header input, .pane-head select, .pane-head input")) {
  control.addEventListener("change", render);
}

const initial = loadState();
applyState(initial);
labelTheme();
jar.updateCode(initial && typeof initial.code === "string" ? initial.code : SAMPLE);
render();
