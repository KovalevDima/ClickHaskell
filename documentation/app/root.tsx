import { Link, LinksFunction, Links, Meta, Outlet, Scripts } from "react-router";
import "./index.css";
import logo from "/assets/logo.svg";
import GitHubStars from "@/components/GitHubStart";
import { ThemeProvider } from "@/components/theme-provider";
import { ModeToggle } from "@/components/ui/mode-toggle";
import { Button } from "@/components/ui/button";
import { NavigationMenu, NavigationMenuContent, NavigationMenuItem, NavigationMenuLink, NavigationMenuList, NavigationMenuTrigger } from "./components/ui/navigation-menu";
import hackage from "/assets/hackage.svg";

export const links: LinksFunction = () => {
  return []
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
        <link href={logo} rel="icon" type="image/x-icon" />
        <Links />
        <Meta />
      </head>
      <body>
        <header className="flex">
          <NavigationMenu viewport={false} className="flex justify-between" >
            <NavigationMenuList className="flex justify-between">
              <NavigationMenuItem>
                <NavigationMenuLink asChild>
                  <Link to="/"><img alt="logo" width="32" height="32" src={logo} /></Link>
                </NavigationMenuLink>
              </NavigationMenuItem>
            </NavigationMenuList>
            <Link to="/contribution">/contribution</Link>
            <Link to="/testing">/testing</Link>
            <Link to="/usage/">/usage</Link>
            {/* <li><Link to="/performance">/performance</Link></li> */}
            {/* <li><a href="/protocol/server/">/protocol/server</a></li> */}
            {/* <li><a href="/protocol/client/">/protocol/client</a></li> */}
            <NavigationMenuList>
              <NavigationMenuItem>
                <NavigationMenuLink asChild>
                  <Link to="https://hackage.haskell.org/package/ClickHaskell">
                    <img alt="hckg" width="32" height="32" src={hackage} />
                  </Link>
                </NavigationMenuLink>
              </NavigationMenuItem>
              <NavigationMenuItem >
                <NavigationMenuLink asChild >
                  <Link to="https://git.clickhaskell.dev" className="flex-row items-center">
                    <img alt="git" width="32" height="32" src="/assets/git.svg" />
                    <GitHubStars />
                  </Link>
                </NavigationMenuLink>
              </NavigationMenuItem>
            </NavigationMenuList>
          </NavigationMenu>

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
  return (
    <ThemeProvider defaultTheme="dark" storageKey="vite-ui-theme">
      <Outlet />
    </ThemeProvider>
  )
}