import { Link, LinksFunction, Links, Meta, Outlet, Scripts } from "react-router";
import "./index.css";
import logo from "/assets/logo.svg";
import { ThemeProvider } from "@/components/theme-provider"
import { SidebarInset, SidebarProvider, SidebarTrigger } from "./components/ui/sidebar";
import { AppSidebar } from "./components/app-sidebar";
import { Header } from "./components/header";

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
          <main role="main" className="flex flex-col items-center">
            <div className="w-full [--header-height:calc(--spacing(14))]">
              <SidebarProvider className="flex flex-col" defaultOpen={false}>
                <Header />
                <div className="flex flex-1">
                  <AppSidebar/>
                  <SidebarInset>
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