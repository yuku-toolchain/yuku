/** Print the AST back to formatted source. */
export function print(program: any): string;
/** Print TypeScript to JavaScript, dropping TypeScript-only syntax. */
export function strip(program: any): string;
/** Print minified (compact, comments dropped) source. */
export function minify(program: any): string;
