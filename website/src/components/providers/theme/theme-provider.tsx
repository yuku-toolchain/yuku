"use client";

import { ThemeProvider as NextThemeProvider, useTheme as useNextTheme } from "next-themes";
import type { PropsWithChildren } from "react";

export default function ThemeProvider({ children }: PropsWithChildren) {
  return (
    <NextThemeProvider
      enableSystem
      defaultTheme="system"
      disableTransitionOnChange
      attribute="class"
    >
      {children}
    </NextThemeProvider>
  );
}

export function useTheme() {
  return useNextTheme();
}
