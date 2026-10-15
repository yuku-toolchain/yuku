import { setFlagsFromString } from "node:v8";

setFlagsFromString("--wasm-sync-tier-up");
setFlagsFromString("--wasm-num-compilation-tasks=0");
