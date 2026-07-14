// outside ambient contexts a trailing comma after a rest parameter is an
// error, while a declare modifier makes the declaration ambient even here
declare function ambient(...args: string[],): string;

function impl(...args: string[],): void {}
