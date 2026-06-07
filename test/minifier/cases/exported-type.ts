export type ColorProps<T> = {
  name: T;
  defaultValue?: string;
};

export interface Logger {
  log(msg: string): void;
}

const internal: ColorProps<string> = { name: "x" };

export function run() {
  return { internal };
}
