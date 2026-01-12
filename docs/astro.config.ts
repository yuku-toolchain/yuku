import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

export default defineConfig({
	integrations: [
		starlight({
			title: 'High-performance JavaScript/TypeScript compiler in Zig 🦎',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/arshad-yaseen/yuku' }],
			customCss: ["./src/styles/index.css"],
			sidebar: [
			{ label: 'Introduction', slug: '' }
			],
		}),
	],
});
