import { reactRouter } from "@react-router/dev/vite";
import { defineConfig } from "vite";
import tsconfigPaths from "vite-tsconfig-paths";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  plugins: [
    reactRouter(),
    tsconfigPaths(),
    tailwindcss()
  ],
  publicDir: './app/public/',
  assetsInclude: ['./../**/*.lhs'],
  server: {
    watch: {
      usePolling: true,
    },
  }
});
