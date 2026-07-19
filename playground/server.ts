const dir = import.meta.dir;
const npm = `${dir}/../npm`;

const local: Record<string, string> = {
  "https://esm.sh/@yuku-parser/wasm": "/pkg/yuku-parser-wasm/index.js",
  "https://esm.sh/@yuku-codegen/wasm": "/pkg/yuku-codegen-wasm/index.js",
  "https://esm.sh/@yuku-analyzer/wasm": "/pkg/yuku-analyzer-wasm/index.js",
  "https://esm.sh/yuku-ast": "/pkg/yuku-ast/dist/index.js",
};

const server = Bun.serve({
  port: Number(Bun.env.PORT) || 3000,
  async fetch(req) {
    const { pathname } = new URL(req.url);
    if (pathname === "/" || pathname === "/index.html") {
      let html = await Bun.file(`${dir}/index.html`).text();
      // replaceAll: the parser url appears twice (import map + version badge).
      for (const [cdn, path] of Object.entries(local)) html = html.replaceAll(cdn, path);
      return new Response(html, { headers: { "content-type": "text/html" } });
    }
    const file = pathname.startsWith("/pkg/")
      ? Bun.file(npm + pathname.slice("/pkg".length))
      : Bun.file(dir + pathname);
    if (await file.exists()) return new Response(file);
    return new Response("not found", { status: 404 });
  },
});

console.log(`yuku playground -> ${server.url}`);
