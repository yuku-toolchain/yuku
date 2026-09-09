const dir = import.meta.dir;
const npm = `${dir}/../npm`;

const server = Bun.serve({
  port: Number(Bun.env.PORT) || 3000,
  async fetch(req) {
    const { pathname } = new URL(req.url);
    const file = pathname.startsWith("/pkg/")
      ? Bun.file(npm + pathname.slice("/pkg".length))
      : Bun.file(dir + (pathname === "/" ? "/index.html" : pathname));
    if (await file.exists()) return new Response(file);
    return new Response("not found", { status: 404 });
  },
});

console.log(`yuku playground -> ${server.url}`);
