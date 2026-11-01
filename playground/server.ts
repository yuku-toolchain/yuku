const dir = import.meta.dir;

const server = Bun.serve({
  port: Number(Bun.env.PORT) || 3000,
  async fetch(req) {
    const { pathname } = new URL(req.url);
    const file = pathname.startsWith("/npm/")
      ? Bun.file(`${dir}/..${pathname}`)
      : Bun.file(dir + (pathname === "/" ? "/index.html" : pathname));
    if (await file.exists()) return new Response(file);
    return new Response("not found", { status: 404 });
  },
});

console.log(`yuku playground -> ${server.url}`);
