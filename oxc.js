import { parseSync } from "oxc-parser";
import fs from "fs";

const source = fs.readFileSync("test/index.js", "utf8");

console.time("parse");
const { program } = parseSync("filename.js", source, {
  experimentalRawTransfer: true
});
console.timeEnd("parse");
