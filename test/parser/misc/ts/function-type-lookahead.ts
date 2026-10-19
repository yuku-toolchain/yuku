const a = (): (void) => {};
const b = (): (null) => {};
const c = (): (true) => {};
const d = (): (false) => {};
type F = (this) => void;
type G = (this, a: number) => void;
