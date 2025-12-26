import type { PropsWithChildren } from "react";
import ThemeProvider from "./theme/theme-provider";

export default function Providers({ children }: PropsWithChildren) {
  return <ThemeProvider>{children}</ThemeProvider>;
}
