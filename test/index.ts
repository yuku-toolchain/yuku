// empty params - previously produced a broken wrapper span
function f() {}
const g = () => 1;

// non-empty params
function h(x: number, y: string) {}
const i = (x: number, y: string) => x;

// generic arrow
const j = <T>(x: T): T => x;
