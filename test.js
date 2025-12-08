const [a, b, ...rest] = [1, 2, 3, 4, 5];
const [x, , ...y] = [1, 2, 3];

const {a: aa, b: bb, ...restObj} = {a: 1, b: 2, c: 3, d: 4};
const {x: xx, ...yy} = {x: 1, y: 2} = x
