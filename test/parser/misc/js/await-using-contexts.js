// await using declares only in an await context with all three tokens on one
// line, otherwise `await` is an operator or a plain identifier
async function declarations() {
  await using x = h();
  await using using = h();
}

async function expressions() {
  await using.x + await using(x) ? await using?.x : await using`x`;
  await using instanceof foo;
  await using
  next = 1;
}

function sloppy() {
  await
  using
  using = h();
}
