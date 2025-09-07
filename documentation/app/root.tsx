import { Link, LinksFunction, Links, Meta, Outlet, Scripts } from "react-router";
import "./index.css";
import logo from "/assets/logo.svg";
import GitHubStars from "~/components/GitHubStars";
import { ThemeProvider } from "@/components/theme-provider"
import { ModeToggle } from "@/components/ui/mode-toggle";
import {
  NavigationMenu, NavigationMenuItem, NavigationMenuLink, NavigationMenuList
} from "./components/ui/navigation-menu";
import hackage from "/assets/hackage.svg";

export const links: LinksFunction = () => {
  return []
}

export function Layout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en" suppressHydrationWarning>
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
      <body className="flex flex-col w-full">
        <ThemeProvider
          attribute="class"
          defaultTheme="system"
          enableSystem
          disableTransitionOnChange
        >
          <header className="flex bg-background  align-middle sticky top-0 h-14">
            <NavigationMenu className="pl-6 pr-6" viewport={false}>
              <NavigationMenuList className="flex justify-between">
                <NavigationMenuItem>
                  <NavigationMenuLink asChild>
                    <Link to="/"><img alt="logo" className="size-5" src={logo} /></Link>
                  </NavigationMenuLink>
                </NavigationMenuItem>
                <NavigationMenuItem>
                  <NavigationMenuLink asChild>
                    <Link to="/contribution">/contribution</Link>
                  </NavigationMenuLink>
                </NavigationMenuItem>
                <NavigationMenuItem>
                  <NavigationMenuLink asChild>
                    <Link to="/testing">/testing</Link>
                  </NavigationMenuLink>
                </NavigationMenuItem>
                <NavigationMenuItem>
                  <NavigationMenuLink asChild>
                    <Link to="/usage/">/usage</Link>
                  </NavigationMenuLink>
                </NavigationMenuItem>
                {/* <li><Link to="/performance">/performance</Link></li> */}
                {/* <li><a href="/protocol/server/">/protocol/server</a></li> */}
                {/* <li><a href="/protocol/client/">/protocol/client</a></li> */}
              </NavigationMenuList>
              <NavigationMenuList>
                <NavigationMenuItem>
                  <ModeToggle />
                </NavigationMenuItem>
                <NavigationMenuItem>
                  <NavigationMenuLink asChild>
                    <Link to="https://hackage.haskell.org/package/ClickHaskell" className="flex-row items-center h-9">
                      <img alt="hckg" width="24" height="24" src={hackage} />
                    </Link>
                  </NavigationMenuLink>
                </NavigationMenuItem>
                <NavigationMenuItem >
                  <NavigationMenuLink asChild>
                    <Link to="https://git.clickhaskell.dev" className="flex-row items-center h-9">
                      <img alt="git" width="24" height="24" src="/assets/git.svg" />
                      <GitHubStars />
                    </Link>
                  </NavigationMenuLink>
                </NavigationMenuItem>
              </NavigationMenuList>
            </NavigationMenu>
          </header>
          <main role="main" className="flex flex-col items-center">
            {children}
            <Scripts />
          </main>
        </ThemeProvider>
      </body>
    </html>
  )
}

export default function App() {
  return (
    <Outlet />
  )
}