import type { PropsWithChildren } from "react";
import Header from "./header";
import { BrandIcon } from "@/ui/icon";

export default function RootLayout({ children }: PropsWithChildren) {
  return (
    <main className="flex flex-col min-h-screen w-full [--header-height:--spacing(16)]">
      <Header />
      <div className="h-[calc(100dvh-var(--header-height))] w-full">
        {children}

        <div className="flex justify-center h-[48vh] items-center w-full absolute bottom-0 overflow-hidden right-0">
          <BrandIcon className="w-screen" />
        </div>
      </div>
    </main>
  );
}
