for (x = (a ? b : c in d);;);
for (x = [a ? b : c in d];;);
for (x = { k: a ? b : c in d };;);
for (x = `${a ? b : c in d}`;;);
for (x = function (p = a ? b : c in d) {};;);
for (x = class { m = a ? b : c in d };;);
for ((x => a in b);;);
for (x = [() => a in b];;);
function* g() { for (x = (yield a in b);;); }
for (x = function () { var y = a in b; };;);
for (x = a ? b in c : d;;);

for (x = a in b;;);
for (x = a ? b : c => d in e;;);
