import ThemeToggle from "../providers/theme/theme-toggle";

export default function Header() {
  return (
    <header className="flex items-center sticky top-0 justify-end pr-3 sm:pl-8 pl-6 h-(--header-height)">
      <ThemeToggle />
    </header>
  );
}
