const dist = `${import.meta.dir}/dist`;

const server = Bun.serve({
  port: Number(Bun.env.PORT) || 3000,
  async fetch(req) {
    const { pathname } = new URL(req.url);
    const file = Bun.file(dist + (pathname === "/" ? "/index.html" : pathname));
    if (await file.exists()) return new Response(file);
    return new Response("not found", { status: 404 });
  },
});

console.log(`yuku playground -> ${server.url}`);
