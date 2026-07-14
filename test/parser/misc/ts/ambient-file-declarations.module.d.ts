// a .d.ts file is ambient throughout, so const needs no initializer and
// functions are body-less without a declare modifier
export const foo: string

export const bar: number, baz: boolean;

export function greet(name: string): void

const local: string

declare const explicit: string
