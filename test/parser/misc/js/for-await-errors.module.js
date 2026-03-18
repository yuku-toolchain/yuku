async function f() {
  for await (;;) {}
  for await (x in y) {}
  for await (let x in y) {}
  for await (let x = 0;;) {}
}
