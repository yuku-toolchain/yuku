import { setFlagsFromString } from "node:v8";

// V8 optimizes hot WebAssembly on background threads even under CodSpeed's --predictable,
// and a thread that lands inside a measurement makes it differ from run to run. these run
// the same compilation on the main thread, so it lands in the same place every time
setFlagsFromString("--wasm-sync-tier-up");
setFlagsFromString("--wasm-num-compilation-tasks=0");
