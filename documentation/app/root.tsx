import { Outlet, Scripts } from "react-router";

import "./index.css";

export default function App() {
  return (
    <html lang="en">
      <head>
        <meta charset="utf-8" />
        <meta http-equiv="x-ua-compatible" content="ie=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <meta name="description" content="A high-performance Haskell implementation of the ClickHouse Native protocol" />
        <meta property="og:title" content="ClickHaskell - ClickHouse client in Haskell" />
        <meta property="og:description" content="A high-performance Haskell implementation of the ClickHouse Native protocol" />
        <title>ClickHaskell</title>
        <link rel="canonical" href="https://clickhaskell.dev/" />
        <link href="/assets/logo.svg" rel="icon" type="image/x-icon" />
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/vs2015.min.css" />

      </head>
      <body>
        <Outlet />
        <Scripts />
      </body>
    </html>
  );
}