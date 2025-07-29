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
    proxy: {
      // Проксируем все WebSocket-запросы
      '/visits': {
        target: 'ws://localhost:3000',  // Указываем адрес WebSocket-сервера
        ws: true,                       // Включаем поддержку WebSocket
        changeOrigin: true,             // Меняет origin запроса (при необходимости)
      },
    },
    watch: {
      usePolling: true,
    },
  }
});
