import type { Backend } from "../read";

interface Binding {
  parse(source: string, flags: number): ArrayBuffer;
  analyze(source: string, flags: number): ArrayBuffer;
}

interface NodeProcess {
  platform?: string;
  arch?: string;
  dlopen?(module: { exports: unknown }, path: string): void;
}

interface DenoNamespace {
  permissions?: { querySync?(descriptor: { name: "ffi" }): { state: string } };
}

let backend: Backend | null | undefined;

/** The Node-API addon the package ships for this platform, or `null` where none loads. */
export function nativeBackend(): Backend | null {
  if (backend === undefined) backend = load();
  return backend;
}

function load(): Backend | null {
  const process = (globalThis as { process?: NodeProcess }).process;
  if (typeof process?.dlopen !== "function") return null;
  if (!ffiGranted()) return null;
  const module = { exports: {} as Partial<Binding> };
  try {
    const url = addonUrl(`${process.platform}-${process.arch}`);
    if (url?.protocol !== "file:") return null;
    process.dlopen(module, pathOf(url, process.platform === "win32"));
  } catch {
    return null;
  }
  const { parse, analyze } = module.exports;
  if (typeof parse !== "function" || typeof analyze !== "function") return null;
  const binding: Binding = { parse, analyze };
  return { dialect: null, run: (entry, source, flags) => binding[entry](source, flags) };
}

function addonUrl(platform: string): URL | null {
  switch (platform) {
    case "darwin-arm64":
      return new URL("./darwin-arm64.node", import.meta.url);
    case "darwin-x64":
      return new URL("./darwin-x64.node", import.meta.url);
    case "linux-arm64":
      return new URL("./linux-arm64.node", import.meta.url);
    case "linux-x64":
      return new URL("./linux-x64.node", import.meta.url);
    case "win32-arm64":
      return new URL("./win32-arm64.node", import.meta.url);
    case "win32-x64":
      return new URL("./win32-x64.node", import.meta.url);
    default:
      return null;
  }
}

function ffiGranted(): boolean {
  const deno = (globalThis as { Deno?: DenoNamespace }).Deno;
  if (deno === undefined) return true;
  try {
    return deno.permissions?.querySync?.({ name: "ffi" }).state === "granted";
  } catch {
    return false;
  }
}

function pathOf(url: URL, windows: boolean): string {
  const path = decodeURIComponent(url.pathname);
  if (!windows) return path;
  return url.host === "" ? path.slice(1) : `//${url.host}${path}`;
}
