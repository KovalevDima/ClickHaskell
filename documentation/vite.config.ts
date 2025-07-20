import { reactRouter } from "@react-router/dev/vite";
import { defineConfig } from "vite";
import tsconfigPaths from "vite-tsconfig-paths";

export default defineConfig({
  plugins: [
    reactRouter(),
    tsconfigPaths(),
  ],
  publicDir: './app/public/',
  assetsInclude: ['./../**/*.lhs'],
  server: {
    watch: {
      usePolling: true,
    },
  }
});
