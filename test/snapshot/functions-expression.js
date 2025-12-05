const a = function() {};
const b = function named() {};
const c = function(x) {};
const d = function(x, y) {};
const e = function(x, y, z) {};
const f = function(...args) {};
const g = function(x, ...rest) {};
const h = function(x = 1) {};
const i = function(x = 1, y = 2) {};
const j = function({x}) {};
const k = function({x, y}) {};
const l = function([a]) {};
const m = function([a, b]) {};
const n = function({x}, y) {};
const o = function([a], b) {};
const p = function({x: renamed}) {};
const q = function({x = 1}) {};
const r = function([a = 1]) {};
const s = function({x, ...rest}) {};
const t = function([a, ...rest]) {};

const u = function*() {};
const v = function* named() {};
const w = function*(x) {};
const x = function*(x, y) {};
const y = function*(...args) {};
const z = function*(x = 1) {};
const aa = function*({x}) {};
const ab = function*([a]) {};

const ac = async function() {};
const ad = async function named() {};
const ae = async function(x) {};
const af = async function(x, y) {};
const ag = async function(...args) {};
const ah = async function(x = 1) {};
const ai = async function({x}) {};
const aj = async function([a]) {};

const ak = async function*() {};
const al = async function* named() {};
const am = async function*(x) {};
const an = async function*(x, y) {};
const ao = async function*(...args) {};
const ap = async function*(x = 1) {};
const aq = async function*({x}) {};
const ar = async function*([a]) {};

const a1 = function*() {};
const b1 = function *() {};
const c1 = function* () {};
const d1 = function * () {};

const e1 = function*named() {};
const f1 = function *named() {};
const g1 = function* named() {};
const h1 = function * named() {};

const i1 = async function*() {};
const j1 = async function *() {};
const k1 = async function* () {};
const l1 = async function * () {};
