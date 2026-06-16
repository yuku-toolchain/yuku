import { expect, test } from "bun:test";
import { gen } from "./helpers";

const MIXED = String.raw`const single = 'hi';
const double = "hi";
const escaped = "esc\u0041pe";
const quoteInside = 'say "hi"';
const apostrophe = "it's";
const obj = { 'key': 1, "other": 2 };`;

test("quotes:preserve keeps the original quote of each literal", () => {
  expect(gen("print", MIXED, { quotes: "preserve" })).toMatchInlineSnapshot(`
    "const single = 'hi';
    const double = "hi";
    const escaped = "escApe";
    const quoteInside = 'say "hi"';
    const apostrophe = "it's";
    const obj = { 'key': 1, "other": 2 };"
  `);
});

test("quotes:single normalizes to single quotes where it avoids escaping", () => {
  expect(gen("print", MIXED, { quotes: "single" })).toMatchInlineSnapshot(`
    "const single = 'hi';
    const double = 'hi';
    const escaped = 'escApe';
    const quoteInside = 'say "hi"';
    const apostrophe = 'it\\'s';
    const obj = { 'key': 1, 'other': 2 };"
  `);
});
