import { Link, LinksFunction, Links, Meta, Outlet, Scripts } from "react-router";
import "./index.css";
import logo from "/assets/logo.svg";
import hackage from "/assets/hackage.svg";
import git from "/assets/git.svg";
import { ThemeProvider } from "@clickhaskell/ui/components/theme-provider"
import { SidebarInset, SidebarProvider } from "@clickhaskell/ui/components/ui/sidebar";
import { AppSidebar, SideBarItems } from "@clickhaskell/ui/components/app-sidebar";
import { Header } from "@clickhaskell/ui/components/header";
import {
  BugOff,
  GitPullRequestCreate,
  PackagePlus,
  Microscope,
  Binary,
} from "lucide-react"
import GitHubStars from "@clickhaskell/ui/components/GitHubStars";
import { useLocation } from "react-router";

export const links: LinksFunction = () => {
  return []
}

export function Layout({ children }: { children: React.ReactNode }) {
  const { pathname } = useLocation();

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
          <main role="main" className="flex flex-col items-center">
            <div className="w-full [--header-height:calc(--spacing(14))]">
              <SidebarProvider className="flex flex-col" defaultOpen={true}>
                <div className="flex flex-1">
                  <AppSidebar items={nav(pathname)} />
                  <SidebarInset>
                    <Header items={headerLinks} />
                    <div className="flex flex-1 flex-col gap-4 p-4">
                      {children}
                    </div>
                  </SidebarInset>
                </div>
              </SidebarProvider>
              <Scripts />
            </div>
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


const headerLinks = [
  {
    link: "https://hackage.haskell.org/package/ClickHaskell",
    component:
      <>
        <img alt="hckg" width="24" height="24" src={hackage} />
      </>,
  },
  {
    link: "https://git.clickhaskell.dev",
    component:
      <>
        <img alt="git" width="24" height="24" src={git} />
        <GitHubStars />
      </>,
  }

]

const nav = (pathname : string): SideBarItems => ({
  navHeader: {
    logo: logo
  },
  navMain: [
    {
      title: "Build",
      icon: PackagePlus,
      isActive: true,
      items: [
        {
          title: "All-in-one",
          url: "/docs/usage/index",
        },
      ]
    },
    {
      title: "Contribution",
      icon: GitPullRequestCreate,
      isActive: true,
      items: [
        {
          title: "Enviroment",
          url: "/docs/contibution/index"
        }
      ],
    },
    {
      title: "About QA",
      icon: BugOff,
      isActive: pathname.startsWith("/docs/testing"),
      items: [
        {
          title: "Query serialization",
          url: "/docs/testing/t001-query-serializaton/index",
        },
        {
          title: "Serialization",
          url: "/docs/testing/t002-rw-equality/index",
        },
        {
          title: "Multithreading",
          url: "/docs/testing/t003-multithreading/index",
        },
        {
          title: "Expections",
          url: "/docs/testing/t004-errors/index",
        },
        {
          title: "Settings",
          url: "/docs/testing/t005-settings/index",
        },
      ]
    },
    {
      title: "Protocol",
      icon: Binary,
      isActive: true,
      items: [
        {
          title: "Client",
          url: "/docs/protocol/client",
        },
        {
          title: "Server",
          url: "/docs/protocol/server",
        },
        {
          title: "Common",
          url: "/docs/protocol/common",
        },
      ]
    },
  ]
});
