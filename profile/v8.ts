import { setFlagsFromString } from "node:v8";

setFlagsFromString("--no-liftoff");
setFlagsFromString("--no-wasm-lazy-compilation");
