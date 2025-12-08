let [] = x
let [a] = x
let [a, b] = x
let [a, b, c] = x
let [a, b, c, d, e] = x
let [,] = x
let [,,] = x
let [,,,] = x
let [a,] = x
let [a, b,] = x
let [,a] = x
let [,,a] = x
let [,a,] = x
let [a,,b] = x
let [a,,,d] = x
let [...rest] = x
let [a, ...rest] = x
let [a, b, ...rest] = x
let [[]] = x
let [[a]] = x
let [[[a]]] = x
let [[a, b]] = x
let [[a], [b]] = x
let [[[a, b], [c, d]]] = x
let [a, [b, c], d] = x
let [[...rest]] = x
let [[...a, ...b]] = x
let [a, [...rest]] = x
let [{a}] = x
let [{a}, {b}] = x
let [{a, b}] = x
let [[a], {b}] = x
let [{a: [b, c]}] = x
let [a = 1] = x
let [a = 1, b = 2] = x
let [a = 1, b = 2, c = 3] = x
let [[a = 1]] = x
let [[[a = 1]]] = x
let [[a = 1, b = 2]] = x
let [a = 1, [b = 2]] = x
let [{a = 1}] = x
let [{a: b = 1}] = x
let [a = 1, {b = 2}] = x
let [...[a, b]] = x
let [...[a, b, c]] = x
let [a, ...[b, c]] = x
let [...{a, b}] = x
let [a, ...{b, c}] = x
