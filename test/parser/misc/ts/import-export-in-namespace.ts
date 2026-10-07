namespace N {
  export const a = 1;
  export import B = N;
}
declare module "m" {
  import x from "y";
  export { x };
}
export {};
