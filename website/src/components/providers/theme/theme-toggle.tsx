"use client";

import { Icon } from "@/ui/icon";
import { useTheme } from "./theme-provider";
import { Button } from "@/ui/button";

export default function ThemeToggle() {
  const { resolvedTheme, setTheme } = useTheme();

  return (
    <Button
      variant="plain"
      onClick={() => {
        setTheme(resolvedTheme === "dark" ? "light" : "dark");
      }}
    >
      <Icon name="Moon02Icon" className="size-4 dark:hidden" />
      <Icon name="Sun01Icon" className="size-4 hidden dark:block" />
    </Button>
  );
}
