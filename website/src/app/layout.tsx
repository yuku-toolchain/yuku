import type { Metadata } from "next";
import { Geist_Mono, Inter, Libre_Baskerville } from "next/font/google";
import Providers from "@/components/providers/providers";

import "@/styles/globals.css";

import Layout from "@/components/layout/root-layout";

const fontSans = Inter({
  variable: "--font-sans",
  subsets: ["latin"],
  display: "swap",
});

const fontMono = Geist_Mono({
  variable: "--font-mono",
  subsets: ["latin"],
  display: "swap",
});

const fontSerif = Libre_Baskerville({
  weight: ["400", "700"],
  variable: "--font-serif",
  display: "swap",
});

export const metadata: Metadata = {
  title: "Yuku",
  description: "Very fast JavaScript/TypeScript parser, written in Zig.",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body
        className={`${fontSans.variable} ${fontMono.variable} ${fontSerif.variable} font-sans antialiased`}
      >
        <Providers>
          <Layout>{children}</Layout>
        </Providers>
      </body>
    </html>
  );
}
