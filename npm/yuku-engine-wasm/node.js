import { readFileSync } from "node:fs";
import { createEngine } from "./core.js";

export const { analyze, init, parse } = createEngine(readFileSync);
