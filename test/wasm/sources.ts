export const jsSource = `// leading comment
const greeting = "hello";
function add(a, b = 1, ...rest) {
  return a + b + rest.length;
}
class Foo extends Bar {
  #x = 42;
  static { console.log("static block"); }
  get value() { return this.#x; }
}
export const result = add(1, 2, 3) ?? greeting;
`;

export const tsSource = `
interface Point { x: number; y?: string }
type Mapped<T> = { readonly [K in keyof T]-?: T[K] };
enum Color { Red, Green = "g" }
namespace NS { export const v: Point = { x: 1 }; }
const fn = async <T,>(arg: T): Promise<T> => arg;
`;
