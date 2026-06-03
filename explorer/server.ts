const dir = import.meta.dir;
const npm = `${dir}/../npm`;

const server = Bun.serve({
  port: Number(Bun.env.PORT) || 3000,
  async fetch(req) {
    const { pathname } = new URL(req.url);
    const rel = pathname === "/" ? "/index.html" : pathname;
    const path = rel.startsWith("/pkg/") ? `${npm}/${rel.slice(5)}` : `${dir}${rel}`;
    const file = Bun.file(path);
    if (await file.exists()) return new Response(file);
    return new Response("not found", { status: 404 });
  },
});

console.log(`yuku playground -> ${server.url}`);
