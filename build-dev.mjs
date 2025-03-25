import esbuild from "esbuild";
import pursPlugin from "esbuild-plugin-purescript";
import copyStaticFiles from "esbuild-copy-static-files";

const buildDir = "dist";
const ctx = await esbuild
	.context({
		entryPoints: ["index.js"],
		bundle: true,
		minify: true,
		sourcemap: true,
		outdir: buildDir,
		plugins: [
			// allow importing Purescript modules in JavaScript files.
			pursPlugin(),
			// copy everything under `static` to `dist`.
			copyStaticFiles({ src: "./static", dest: "./dist" }),
		],
		logLevel: "debug",
	})
	.catch((e) => {
		console.error(e);
		process.exit(1);
	});

await ctx.watch();
await ctx.serve({ servedir: buildDir, port: 4000 });
