import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import type { CoreOptions, Platform } from "../core";

const { resolve } = createRequire(import.meta.url);

export const addons: CoreOptions["addons"] = {
  "darwin-arm64": () => addon("darwin-arm64"),
  "darwin-x64": () => addon("darwin-x64"),
  "linux-arm64": () => addon("linux-arm64"),
  "linux-x64": () => addon("linux-x64"),
  "win32-arm64": () => addon("win32-arm64"),
  "win32-x64": () => addon("win32-x64"),
};

function addon(platform: Platform): URL {
  return pathToFileURL(resolve(`@yuku-toolchain/${platform}`));
}
