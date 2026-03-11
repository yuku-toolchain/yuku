import starlight from "@astrojs/starlight";
import { defineConfig } from "astro/config";

export default defineConfig({
	integrations: [
		starlight({
			title: "High-performance JavaScript/TypeScript compiler in Zig 🦎",
			social: [
				{
					icon: "github",
					label: "GitHub",
					href: "https://github.com/yuku-toolchain/yuku",
				},
			],
			customCss: ["./src/styles/index.css"],
			sidebar: [
				{ label: "Introduction", slug: "" },
				{
					label: "Parser",
					items: [
						{ label: "Introduction", slug: "parser" },
						{ label: "AST", slug: "parser/ast" },
						{
							label: "Traverse",
							slug: "parser/traverse",
							badge: "Coming Soon",
						},
						{
							label: "Architecture",
							slug: "parser/architecture",
							badge: "Coming Soon",
						},
					],
				},
			],
		}),
	],
});
