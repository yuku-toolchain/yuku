import { parseSync } from "oxc-parser";
import { join } from "path";
import { Glob } from "bun";

const FOLDERS = ["test/pass"];
// We do not currently supported, will remove fields from this list one by one
const FIELDS_TO_REMOVE = ["typeAnnotation", "typeParameters", "typeArguments", "returnType", "superTypeArguments", "implements", "abstract", "definite", "override", "accessibility", "readonly", "importKind", "exportKind", "in", "asserts", "out", "const", "optional"];

function removeFields(obj: any, fieldsToRemove: string[]): any {
  if (obj === null || obj === undefined) {
    return obj;
  }

  if (Array.isArray(obj)) {
    return obj.map((item) => removeFields(item, fieldsToRemove));
  }

  if (typeof obj === "object") {
    const cleaned: any = {};
    for (const [key, value] of Object.entries(obj)) {
      if (!fieldsToRemove.includes(key)) {
        cleaned[key] = removeFields(value, fieldsToRemove);
      }
    }
    return cleaned;
  }

  return obj;
}

async function processFile(folderPath: string, fileName: string) {
  try {
    const filePath = join(folderPath, fileName);
    const source = await Bun.file(filePath).text();

    const isModule = fileName.includes(".module.js");

    const result = parseSync(filePath, source, {
      sourceType: isModule ? "module" : "script",
    });

    const output = {
      program: result.program,
      errors: result.errors,
    };

    const cleaned = removeFields(output, FIELDS_TO_REMOVE);

    const outputName = `${fileName}.expected.json`;
    const outputPath = join(folderPath, outputName);

    await Bun.write(outputPath, JSON.stringify(cleaned, null, 2));

    console.log(`generated: ${outputName}`);
  } catch (error) {
    console.error(`error processing ${fileName}:`, error);
  }
}

async function processFolder(folderPath: string) {
  try {
    const glob = new Glob("*.js");

    for await (const file of glob.scan(folderPath)) {
      await processFile(folderPath, file);
    }
  } catch (error) {
    console.error(`cannot open ${folderPath}:`, error);
  }
}

async function main() {
  for (const folderPath of FOLDERS) {
    await processFolder(folderPath);
  }
}

main();
