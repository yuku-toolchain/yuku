for (var a = 1 in b);

for (let c = 1 in b);
for (const d = 1 in b);
for (var [e] = 1 in b);
for (var { f } = 1 in b);

for (var g = 1 of b);
for (let h = 1 of b);
for (const i = 1 of b);
async function j() { for await (var k = 1 of b); }
