// ambient contexts allow a trailing comma after a rest parameter
export declare function foo(...args: string[],): string;

export interface I {
    m(...args: string[],): void
}

export type F = (...args: string[],) => void

export type C2 = new (...args: string[],) => object

export declare class C {
    m(...args: string[],): void
}
