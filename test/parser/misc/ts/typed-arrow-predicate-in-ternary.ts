const ok = true;
const values: unknown[] = [];

const fromCall = ok ? values.filter((value): value is string => Boolean(value)) : [];
const fromParens = ok ? ((value): value is string => Boolean(value)) : null;
const fromArray = ok ? [(value): value is string => Boolean(value)] : [];
const fromObject = ok ? { guard: (value): value is string => Boolean(value) } : null;
const fromTemplate = ok ? `${(value): value is string => Boolean(value)}` : "";
const fromComputed = ok ? values[(values.filter((value): value is string => Boolean(value)), 0)] : null;
const fromNested = ok ? (ok ? values.filter((value): value is string => Boolean(value)) : []) : [];
const fromBody = ok ? function () { return (value): value is string => Boolean(value); } : null;

const direct = ok ? (count: number): string => String(count) : null;
