import { reactRouter } from "@react-router/dev/vite";
import { defineConfig } from "vite";
import tsconfigPaths from "vite-tsconfig-paths";

export default defineConfig({
  plugins: [
    reactRouter(),
    tsconfigPaths(),
  ],
  assetsInclude: ['./../**/*.lhs'],
  server: {
    proxy: {
      '/performance': 'http://localhost:3000',
      '/protocol': 'http://localhost:3000',
    }
}
});
