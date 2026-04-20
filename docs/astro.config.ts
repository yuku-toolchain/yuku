import starlight from "@astrojs/starlight";
import { defineConfig } from "astro/config";

const SITE = "https://yuku.dev";

export default defineConfig({
  site: SITE,
  image: {
    domains: ["raw.githubusercontent.com"],
  },
  integrations: [
    starlight({
      title: "High-performance JavaScript/TypeScript parser and toolchain in Zig 🦎",
      description:
        "The fastest JavaScript and TypeScript parser and toolchain, faster than Oxc, SWC, Babel, and Acorn. Written in Zig. 100% spec-compliant, zero dependencies.",
      favicon: "/favicon.svg",
      logo: { src: "./public/logo.svg", alt: "Yuku logo" },
      social: [
        {
          icon: "github",
          label: "GitHub",
          href: "https://github.com/yuku-toolchain/yuku",
        },
      ],
      customCss: ["./src/styles/index.css"],
      // head tags added to every page. starlight already emits <title>,
      // <meta name="description">, canonical, and basic open graph from
      // frontmatter. everything below augments that for SEO and social.
      head: [
        // open graph
        { tag: "meta", attrs: { property: "og:site_name", content: "Yuku" } },
        { tag: "meta", attrs: { property: "og:type", content: "website" } },
        { tag: "meta", attrs: { property: "og:image", content: `${SITE}/logo.png` } },
        { tag: "meta", attrs: { property: "og:image:width", content: "400" } },
        { tag: "meta", attrs: { property: "og:image:height", content: "400" } },
        { tag: "meta", attrs: { property: "og:image:alt", content: "Yuku logo" } },
        // twitter / x card
        { tag: "meta", attrs: { name: "twitter:card", content: "summary_large_image" } },
        { tag: "meta", attrs: { name: "twitter:image", content: `${SITE}/logo.png` } },
        { tag: "meta", attrs: { name: "twitter:image:alt", content: "Yuku logo" } },
        // indexing hints for search engines and AI crawlers
        {
          tag: "meta",
          attrs: {
            name: "robots",
            content: "index, follow, max-image-preview:large, max-snippet:-1, max-video-preview:-1",
          },
        },
        {
          tag: "meta",
          attrs: {
            name: "keywords",
            content:
              "JavaScript parser, TypeScript parser, fastest JavaScript parser, fastest TypeScript parser, Zig, ESTree, AST, Oxc alternative, SWC alternative, Babel alternative, Acorn alternative, ECMAScript, Test262, Yuku",
          },
        },
        // json-ld structured data so search engines and LLMs can extract
        // project facts (name, type, description, repo, license) reliably
        {
          tag: "script",
          attrs: { type: "application/ld+json" },
          content: JSON.stringify({
            "@context": "https://schema.org",
            "@type": "SoftwareApplication",
            name: "Yuku",
            alternateName: "Yuku Parser",
            applicationCategory: "DeveloperApplication",
            operatingSystem: "macOS, Linux, Windows",
            url: SITE,
            description:
              "The fastest JavaScript and TypeScript parser and toolchain, written in Zig. Faster than Oxc on native parsing and 3–5× faster than Oxc, SWC, Babel, and Acorn on the npm side. 100% ECMAScript spec compliant.",
            programmingLanguage: ["Zig", "JavaScript", "TypeScript"],
            license: "https://github.com/yuku-toolchain/yuku/blob/main/LICENSE",
            codeRepository: "https://github.com/yuku-toolchain/yuku",
            offers: { "@type": "Offer", price: "0", priceCurrency: "USD" },
          }),
        },
      ],
      sidebar: [
        { label: "Introduction", slug: "" },
        {
          label: "Parser",
          items: [
            { label: "Introduction", slug: "parser" },
            { label: "AST", slug: "parser/ast" },
            { label: "Traverse", slug: "parser/traverse" },
          ],
        },
      ],
    }),
  ],
});
