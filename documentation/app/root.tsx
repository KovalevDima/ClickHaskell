import { Links, Meta, Outlet, Scripts } from "react-router";

import "./index.css";
import { Link } from "react-router";
import { LinksFunction } from "react-router";
import GitHubStars from "./components/GitHubStart";

export const links: LinksFunction = () => {
	return [
	]
}

export function Layout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en">
      <head>
        <meta charSet="utf-8" />
        <meta httpEquiv="x-ua-compatible" content="ie=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <meta name="description" content="A high-performance Haskell implementation of the ClickHouse Native protocol" />
        <meta property="og:title" content="ClickHaskell - ClickHouse client in Haskell" />
        <meta property="og:description" content="A high-performance Haskell implementation of the ClickHouse Native protocol" />
        <title>ClickHaskell</title>
        <link rel="canonical" href="https://clickhaskell.dev/" />
        <link href="/assets/logo.svg" rel="icon" type="image/x-icon" />
        <Links />
        <Meta />
      </head>
      <body>
      <header>
        <div>
          <Link style={{
            fontSize: "24px",
            color: "rgb(240, 246, 252)",
            textDecoration: "none",
            backgroundColor: "rgb(30, 30, 30)",
            gap: "8px"
          }}
            to="/"
          >
            <img alt="logo" width="28" height="28" src="/assets/logo.svg" />
            ClickHaskell
          </Link>
        </div>
        <div>
          <a href="https://hackage.haskell.org/package/ClickHaskell">
            <img alt="hckg" width="36" height="36" src="/assets/hackage.svg" />
          </a>
          <a id="stars" href="https://git.clickhaskell.dev">
            <img alt="git" width="36" height="36" src="/assets/git.svg" />
            <GitHubStars/>
          </a>
        </div>
      </header>
      <main role="main">
        {children}
        <Scripts />
      </main>
      </body>
    </html>
  )
}

export default function App() {
  return <Outlet />;
}
