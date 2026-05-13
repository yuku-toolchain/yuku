import { minify } from "yuku-minify";

const source = await Bun.file("test/fixture.ts").text();

const _ = minify(source, {
  lang: "ts",
});
