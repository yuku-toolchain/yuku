import "a";
export const top = 1;

{
  import "b";
}
function f() {
  export default 1;
}
if (top) export * from "c";
label: export { top as renamed };
{
  @dec export class C {}
}
