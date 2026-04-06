import fs from "fs";
import { parse } from "oxc-parser";

const source = fs.readFileSync("test/index.js", "utf8");

const { program: _ } = await parse("filename.js", source, {
  sourceType: "module",
  experimentalRawTransfer: true,
});

console.time("parse");
const {program} = await parse("filename.js", source, {
  sourceType: "module",
  experimentalRawTransfer: true,
});
console.timeEnd("parse");
